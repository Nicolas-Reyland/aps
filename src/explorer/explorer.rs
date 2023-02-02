// Graph Explorer

use std::{
    collections::{HashMap, HashSet},
    fmt,
    thread::{self, JoinHandle},
    vec,
};

use crate::parser::OperatorAssociativity::{
    LeftAssociative, LeftRightAssociative, NonAssociative, RightAssociative, Unknown,
};
use crate::parser::{
    parenthesized_atom, AlgebraicFunction, AlgebraicProperty, AssociativityHashMap, Atom, AtomExpr,
    FunctionCallExpr, Operator, OperatorAssociativity, SequentialExpr,
};

#[derive(Debug, Clone)]
pub struct ExprGraph {
    pub nodes: Vec<ExprNode>,
    max_depth: u8,
}

impl fmt::Display for ExprGraph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ExprGraph (max_depth: {}) {{\n", self.max_depth)?;
        for node in &self.nodes {
            write!(f, "\t{},\n", node)?;
        }
        write!(f, "}}")
    }
}

type ExprNodeIndex = usize;

#[derive(Debug, Clone, Hash)]
pub struct ExprNode {
    // TODO: make atom_expr a parser::Atom, since
    // one should be able to ask for a prove such as "A * B = 0"
    pub atom: Atom,
    pub transform: Option<AlgebraicProperty>,
    pub parent: ExprNodeIndex,
    pub index: ExprNodeIndex,
    pub depth: u8,
}

impl PartialEq for ExprNode {
    fn eq(&self, other: &Self) -> bool {
        // not comparing neighbours, nor depths
        self.atom == other.atom
    }
}

impl fmt::Display for ExprNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "ExprNode {{ expr: '{}', depth: {}, index: {}, parent: {}, transform: {}",
            self.atom,
            self.depth,
            self.index,
            self.parent,
            match &self.transform {
                Some(x) => x.to_string(),
                None => "?".to_string(),
            },
        )?;
        write!(f, "] }}")
    }
}

type Atom2AtomHashMap = HashMap<Atom, Atom>;

/// init a expression-graph
pub fn init_graph(atom: Atom) -> ExprGraph {
    let base_node = ExprNode {
        atom,
        parent: 0,
        transform: None,
        depth: 0,
        index: 0,
    };
    let graph = ExprGraph {
        nodes: vec![base_node],
        max_depth: 0,
    };
    graph
}

/// Explore a Graph once
///
/// Exploring a graph means to apply all applicable
/// properties to the nodes of the graph.
///
/// The returned value is the number of new nodes that were added
pub fn explore_graph(
    graph: &mut ExprGraph,
    properties: &HashSet<AlgebraicProperty>,
    associativities: &AssociativityHashMap,
) -> bool {
    // apply every property to all the outer-layer nodes of the graph
    let mut new_nodes: Vec<(ExprNode, ExprNode)> = Vec::new();
    let mut handles: Vec<JoinHandle<(AlgebraicProperty, usize, HashSet<Atom>)>> = Vec::new();
    for node in graph
        .nodes
        .iter()
        .filter(|node| node.depth == graph.max_depth)
    {
        // get some info about that node
        let (node_has_transform, node_transform) = match &node.transform {
            Some(node_tr) => (true, node_tr.clone()),
            None => (false, {
                let empty_atom = Atom::Parenthesized(AtomExpr {
                    atoms: vec![],
                    operator: None,
                });
                AlgebraicProperty {
                    left_atom: empty_atom.clone(),
                    right_atom: empty_atom,
                }
            }),
        };
        // filter :
        // Skip the rounds that would simply 'go back' one iteration.
        // Here is an example: let 1@"X + Y" -> 2@"Y + Z" be two connected nodes
        // The transformation from 1 to 2 is "A + B = B + A". We don't want
        // to run this transformation again on 2 ("Y + X"),
        // since it will only give 1 back ("X + Y"), where we came from.
        for property in properties
            .iter()
            .filter(|property| !node_has_transform || node_transform != **property)
        {
            // one thread per exploration
            // maybe batch jobs together using worker threads later ?
            handles.push({
                let atom_clone = node.atom.clone();
                let property_clone = property.clone();
                let associativities_clone = associativities.clone();
                let node_index = node.index;
                thread::spawn(move || {
                    (
                        property_clone.clone(),
                        node_index,
                        apply_property(&atom_clone, &property_clone, &associativities_clone),
                    )
                })
            });
        }
    }
    // join all the handles
    for handle in handles {
        let (property, node_index, new_expressions) = handle.join().unwrap();
        let node = &graph.nodes[node_index];
        new_nodes.extend(new_expressions.iter().map(|new_atom| {
            (
                node.clone(),
                ExprNode {
                    atom: new_atom.clone(),
                    parent: 0,
                    transform: Some(property.clone()),
                    depth: 0,
                    index: 0,
                },
            )
        }));
    }
    // add new nodes, etc
    let mut at_least_one_new_node = false;
    let mut new_node_index = graph.nodes.len();
    for (src_node, mut new_node) in new_nodes {
        if !graph.nodes.contains(&new_node) {
            at_least_one_new_node = true;
            // add source node to the new node's neighbours
            new_node.parent = src_node.index;
            // set the depth of the new node
            new_node.depth = src_node.depth + 1;
            // set the index of the new node
            new_node.index = new_node_index;
            new_node_index += 1;
            // add new node to graph
            graph.nodes.push(new_node)
        }
    }
    if at_least_one_new_node {
        graph.max_depth += 1;
    }
    // has the graph at least one new node ?
    at_least_one_new_node
}

pub fn print_graph_dot_format(graph: &ExprGraph) -> String {
    let mut content = String::new();
    content.push_str("/* DOT FORMAT START */\n");
    // start printing the graph
    content.push_str("digraph G {{\n\trankdir = LR;\n\tedge [minlen=3.5];\n");
    for node in &graph.nodes {
        content.push_str(&print_node_dot_format(node));
    }
    content.push_str("}}\n");
    content.push_str("/* DOT FORMAT END */\n");
    content
}

fn print_node_dot_format(node: &ExprNode) -> String {
    let mut content = String::new();
    // start printing definition line
    content.push_str(&format!(
        "\t{} [shape=record color={} label=\"",
        node.index,
        if node.index == 0 { "red" } else { "blue" }
    ));
    // print label of node
    content.push_str(&format!("{}", node.atom));
    // end printing definition line
    content.push_str(&format!("\"];\n"));

    // start printing neighbours
    if node.index != 0 || node.transform != None {
        content.push_str(&format!(
            "\t{} -> {} [label=< <B> {} </B> > fontsize=7 fontcolor=darkgreen];\n",
            node.parent,
            node.index,
            match &node.transform {
                Some(x) => x.to_string(),
                None => "?".to_string(),
            },
        ));
    }
    content.push_str("\n");
    content
}

/// apply the property to the source expression
/// a property is an equality between two expressions,
/// so the left and right sides of the property are matched to the source expression
pub fn apply_property(
    src_atom: &Atom,
    property: &AlgebraicProperty,
    associativities: &AssociativityHashMap,
) -> HashSet<Atom> {
    let mut new_expressions = match_and_apply(
        src_atom,
        &property.left_atom,
        &property.right_atom,
        associativities,
    );
    new_expressions.extend(match_and_apply(
        src_atom,
        &property.right_atom,
        &property.left_atom,
        associativities,
    ));
    new_expressions
}

/// try to match the src_expr to the left_expr, then generate a new expression based on right_expr
/// this returns a vector containing zero (if there is no match) or multiple (there is a match) new expressions
/// this can yield multiple new expressions, because the source expression if checked for matching, but also all
/// its 'sub-expressions' (including arguments of function calls)
fn match_and_apply(
    src: &Atom,
    left: &Atom,
    right: &Atom,
    associativities: &AssociativityHashMap,
) -> HashSet<Atom> {
    // call 'atom_expressions_match' on every expression (src_expr and sub-expression in parentheses)
    let mut new_expressions: HashSet<Atom> = HashSet::new();
    // base call on src
    match left_to_right_match(src, left, associativities) {
        // use right atom as-is
        Some(None) => {
            new_expressions.insert(right.clone());
            ()
        }
        // need to generate new atom, based on right atom
        Some(Some(mappings)) => {
            new_expressions.insert(generate_atom(right, &mappings, associativities));
        }
        None => (),
    };
    // recursively call 'match_an_apply' on all sorts of sub-expressions
    // these are: expressions, function-call-args
    // all atom types are 'penetrated' (also sequential-expressions)
    new_expressions.extend(match_and_apply_all_sub_expressions(
        src,
        left,
        right,
        associativities,
    ));

    new_expressions
}

fn match_and_apply_sub_expressions(
    src: &Atom,
    left: &Atom,
    right: &Atom,
    associativities: &AssociativityHashMap,
) -> HashSet<Atom> {
    let mut new_expressions: HashSet<Atom> = HashSet::new();
    for (sub_i, sub) in src
        .atoms
        .iter()
        .enumerate()
        .filter_map(|(atom_i, atom)| match atom {
            Atom::Parenthesized(sub_expr) => Some((atom_i, (*sub_expr).clone())),
            _ => None,
        })
    {
        new_expressions.extend(
            match_and_apply(&sub, left, right, associativities)
                .iter()
                .map(|sub_tr| {
                    // reconstruct like that : <everything b4> (new sub) <everything after>
                    // only the sub changes, compared to the src. no other atoms
                    let mut src_atoms_prefix = src.atoms[..sub_i].to_vec().clone();
                    src_atoms_prefix.push(sub_tr.clone());
                    let mut src_atoms_suffix = src.atoms[sub_i + 1..].to_vec().clone();
                    src_atoms_prefix.append(&mut src_atoms_suffix);
                    AtomExpr {
                        atoms: src_atoms_prefix,
                        operator: src.operator.clone(),
                    }
                })
                .collect::<HashSet<AtomExpr>>(),
        );
    }
    new_expressions
}

fn match_and_apply_fn_calls_args(
    src: &Atom,
    left: &Atom,
    right: &Atom,
    associativities: &AssociativityHashMap,
) -> HashSet<Atom> {
    let mut new_expressions: HashSet<Atom> = HashSet::new();
    // recursively call 'atom_expressions_match' on the function call arguments
    for (sub_i, arg_i, sub) in src
        .atoms
        .iter()
        .enumerate()
        .filter_map(|(atom_i, atom)| match atom {
            Atom::FunctionCall(fn_call_expr) => Some(
                fn_call_expr
                    .args
                    .into_iter()
                    .enumerate()
                    .map(move |(arg_i, arg)| (atom_i, arg_i, arg)),
            ),
            _ => None,
        })
        .flatten()
    {
        new_expressions.extend(
            match_and_apply(&sub, left, right, associativities)
                .iter()
                .map(|new_arg| {
                    // reconstruct like that : <everything b4> func(<args b4> (new arg) <args after>) <everything after>
                    // only the sub changes, compared to the src. no other atoms
                    let mut new_atoms = src.atoms.clone();
                    new_atoms[sub_i] = match new_atoms[sub_i].clone() {
                        Atom::FunctionCall((fn_name, mut fn_args)) => Atom::FunctionCall({
                            fn_args[arg_i] = (*new_arg).clone();
                            FunctionCallExpr {
                                name: fn_name,
                                args: fn_args,
                            }
                        }),
                        incorrect_atom => panic!(
                            "Did not find a function call at index {}: {:#?}",
                            sub_i, incorrect_atom
                        ),
                    };
                    AtomExpr {
                        atoms: new_atoms,
                        operator: src.operator.clone(),
                    }
                })
                .collect::<HashSet<AtomExpr>>(),
        );
    }
    new_expressions
}

fn match_and_apply_sequential_exprs(
    src: &Atom,
    left: &Atom,
    right: &Atom,
    associativities: &AssociativityHashMap,
) -> HashSet<Atom> {
    let mut new_expressions: HashSet<Atom> = HashSet::new();
    // recursively call 'atom_expressions_match' on the function call arguments
    for (seq_i, seq) in src
        .atoms
        .iter()
        .enumerate()
        .filter_map(|(atom_i, atom)| match atom {
            Atom::Sequential(seq) => Some((atom_i, seq.clone())),
            _ => None,
        })
    {
        new_expressions.extend(
            match_and_apply(&seq.enumerator, left, right, associativities)
                .iter()
                .map(|new_enum| {
                    let mut new_atoms = src.atoms.clone();
                    match new_atoms[seq_i].clone() {
                        Atom::Sequential(old_seq) => {
                            new_atoms[seq_i] = Atom::Sequential(Box::new(SequentialExpr {
                                operator: old_seq.operator.clone(),
                                enumerator: (*new_enum).clone(),
                                body: old_seq.body.clone(),
                            }));
                        }
                        incorrect_atom => panic!(
                            "Did not find a sequential expression at index {}: {:#?}",
                            seq_i, incorrect_atom
                        ),
                    };
                    AtomExpr {
                        atoms: new_atoms,
                        operator: src.operator.clone(),
                    }
                })
                .chain(
                    match_and_apply(&seq.body, left, right, associativities)
                        .iter()
                        .map(|new_body| {
                            let mut new_atoms = src.atoms.clone();
                            match new_atoms[seq_i].clone() {
                                Atom::Sequential(old_seq) => {
                                    new_atoms[seq_i] = Atom::Sequential(Box::new(SequentialExpr {
                                        operator: old_seq.operator.clone(),
                                        enumerator: old_seq.enumerator.clone(),
                                        body: (*new_body).clone(),
                                    }));
                                }
                                incorrect_atom => panic!(
                                    "Did not find a sequential expression at index {}: {:#?}",
                                    seq_i, incorrect_atom
                                ),
                            };
                            AtomExpr {
                                atoms: new_atoms,
                                operator: src.operator.clone(),
                            }
                        }),
                )
                .collect::<HashSet<AtomExpr>>(),
        );
    }
    new_expressions
}

fn left_to_right_match_expressions(
    src_expr: &AtomExpr,  // source expression
    dest_expr: &AtomExpr, // expression to match against
    associativities: &AssociativityHashMap,
) -> Option<Atom2AtomHashMap> {
    // mappings of atom-to-atom between src_expr and (p_expr/v_expr)
    let mut mappings: Atom2AtomHashMap = HashMap::new();
    // check the two operators
    if src_expr.operator != dest_expr.operator || src_expr.atoms.len() != dest_expr.atoms.len() {
        return None;
    }
    'main_loop: for (atom_a, atom_b) in src_expr.atoms.iter().zip(dest_expr.atoms.iter()) {
        // compare atoms
        match left_to_right_match(atom_a, atom_b, associativities) {
            Some(None) => (),
            Some(Some(par_mappings)) => {
                for (key, value) in par_mappings {
                    match mappings.insert(key, value.clone()) {
                        None => (),
                        Some(old_value) => {
                            if value != old_value {
                                // old value is different from new value
                                return None;
                            }
                        }
                    }
                }
                continue 'main_loop;
            }
            None => return None,
        }
        // check with already-existing mapping, or insert as new mapping
        match mappings.get(atom_b) {
            Some(atom_c) => {
                // atom_c should be equal to atom_a
                if atom_c != atom_a {
                    return None;
                }
            }
            None => assert_eq!(mappings.insert(atom_b.clone(), atom_a.clone()), None),
        }
        // go to next (operator, atom) pair
    }
    Some(mappings)
}

/// Is atom_a mappable on atom_b ?
///
/// e.g. (A + B) is mappable on A
/// but A is NOT mappable on (A + B)
/// going further, (A + B) is mappable on (X + Y)
///
/// limitation to overcome : the left part of 'A = 1 * A' (just 'A')
/// is not seen as mappable to anything else than another lone atom.
/// An expression should be mappable too, e.g. 'A + B = 1 * (A + B)'
fn left_to_right_match(
    atom_a: &Atom,
    atom_b: &Atom,
    associativities: &AssociativityHashMap,
) -> Option<Option<Atom2AtomHashMap>> {
    // we match the second atom !!
    match atom_b {
        Atom::Parenthesized(par_b) => match atom_a {
            Atom::Parenthesized(par_a) => {
                match left_to_right_match_expressions(par_a, par_b, associativities) {
                    Some(par_mappings) => Some(Some(par_mappings)),
                    None => None,
                }
            }
            _ => None,
        },
        Atom::Symbol(_) => Some(None),
        Atom::Value(_) if atom_a == atom_b => Some(None),
        Atom::Value(_) => None,
        Atom::FunctionCall((fn_name_b, fn_expr_args_b)) => match atom_a {
            // a function call is only mappable to the same function call, with each argument
            // being mappable to it's alter-atom
            Atom::FunctionCall((fn_name_a, fn_expr_args_a)) => {
                // It should be the same function (by name and number of arguments)
                if fn_name_b != fn_name_a || fn_expr_args_b.len() != fn_expr_args_a.len() {
                    return None;
                }
                // check 'mappability' of each argument
                let mut fn_mappings: Atom2AtomHashMap = HashMap::new();
                for (expr_arg_a, expr_arg_b) in fn_expr_args_a.iter().zip(fn_expr_args_b.iter()) {
                    match left_to_right_match_expressions(expr_arg_a, expr_arg_b, associativities) {
                        Some(arg_mappings) => {
                            // add all the mappings, but they shouldn't be any collisions
                            for (arg_key, arg_value) in arg_mappings {
                                match fn_mappings.insert(arg_key, arg_value.clone()) {
                                    Some(old_arg_value) => {
                                        // There has already been an arg_mapping for this argument,
                                        // but it has a new value this time.
                                        //
                                        // Here is an example of such a collision :
                                        // f :: A, A + B -> 2 * A + 2 * B ;
                                        // VALID: f(X, X + Y) mappable to f ?
                                        // INVALID: f(X, (2 * X) + Y) mappable to f ?
                                        // In the invalid case, (2 * X) and Y are both mappable to A and B,
                                        // but there is an argument collision, nor (2 * X) and Y are
                                        // equal to X. This would mappable to f : f(W ^ 2, (W ^ 2) + (2 * Z))
                                        // in this valid case, the mappings would be as follows :
                                        // W ^ 2 mapped to A
                                        // 2 * Z mapped to B
                                        if old_arg_value != arg_value {
                                            return None;
                                        }
                                    }
                                    _ => (),
                                }
                            }
                        }
                        None => return None,
                    }
                }
                // arguments are all mappable
                Some(Some(fn_mappings))
            }
            _ => None,
        },
        Atom::Sequential(seq_expr_b) => {
            left_to_right_match_to_sequential(atom_a, seq_expr_b, associativities)
        }
    }
}

fn left_to_right_match_to_sequential(
    atom_a: &Atom,
    seq_expr_b: &SequentialExpr,
    associativities: &AssociativityHashMap,
) -> Option<Option<Atom2AtomHashMap>> {
    match atom_a {
        Atom::Sequential(seq_expr_a) => {
            left_to_right_match_sequentials(seq_expr_a, seq_expr_b, associativities)
        }
        Atom::Parenthesized(expr_a) => {
            left_to_right_match_expr_to_sequential(expr_a, seq_expr_b, associativities)
        }
        _ => left_to_right_match_once_sequential(atom_a, seq_expr_b, associativities),
    }
}

fn left_to_right_match_once_sequential(
    atom_a: &Atom,
    seq_expr_b: &SequentialExpr,
    associativities: &AssociativityHashMap,
) -> Option<Option<Atom2AtomHashMap>> {
    match left_to_right_match(atom_a, &seq_expr_b.body, associativities) {
        Some(opt_mappings) => Some(Some({
            let mut mappings = match opt_mappings {
                Some(prev_mappings) => prev_mappings,
                None => HashMap::new(),
            };
            // add mapping for enumerator
            let enumerator = seq_expr_b.enumerator.clone();
            let new_enumerator = Atom::Value(1);
            // make sure the enumerator '1' is mappable to the enumerator of the sequence
            if left_to_right_match(&new_enumerator, &enumerator, associativities) == None {
                return None;
            };
            // TODO: check for already existing values
            mappings.insert(enumerator, new_enumerator);
            // add mapping for body
            mappings.insert(seq_expr_b.body.clone(), atom_a.clone());
            mappings
        })),
        None => None,
    }
}

fn left_to_right_match_expr_to_sequential(
    expr_a: &AtomExpr,
    seq_expr_b: &SequentialExpr,
    associativities: &AssociativityHashMap,
) -> Option<Option<Atom2AtomHashMap>> {
    // we should only match more than 2 times (included), since matching one time is already
    // handled previously (see left_to_right_match_to_sequential)
    // enumerator can only be an Atom::Symbol or Atom::Special for an expression to match against it
    if expr_a.operator != Some(seq_expr_b.operator.clone()) {
        return None;
    }
    let naked_expr_a = strip_expr_naked(&parenthesized_atom((*expr_a).clone()), associativities);
    let sequential_length = naked_expr_a.atoms.len() as i32;
    // more than two elements in the sequence
    if sequential_length < 2 {
        return None;
    }
    let repeated_atom = naked_expr_a.atoms.first().unwrap().clone();
    // all elements are equal (e.g. A * A * A => every elt is 'A')
    if naked_expr_a
        .atoms
        .clone()
        .iter()
        .skip(1)
        .any(|atom| *atom != repeated_atom)
    {
        return None;
    }
    let seq_body_atom = seq_expr_b.body.clone();
    // match the sequential-expression body with (e.g. body is not just an Atom::Symbol)
    let mut mappings = match left_to_right_match(&repeated_atom, &seq_body_atom, associativities) {
        Some(Some(prev_mappings)) => prev_mappings,
        // at least the body atom pair is present
        Some(None) => HashMap::with_capacity(1),
        None => return None,
    };
    // add body to mappings
    match mappings.insert(seq_body_atom.clone(), repeated_atom.clone()) {
        Some(old_value) if old_value != repeated_atom => panic!(
            "Value of sequential body was already present in hash map: {}:{} (old {}) in {:#?}",
            seq_body_atom, repeated_atom, old_value, mappings
        ),
        _ => (),
    };
    // check for enumerator
    let seq_enumerator_atom = seq_expr_b.enumerator.clone();
    match &seq_enumerator_atom {
        // match any number of times
        Atom::Symbol(_) => {
            // enumerator atom
            match mappings.insert(
                seq_enumerator_atom.clone(),
                Atom::Value(sequential_length),
            ) {
                Some(old_value) if old_value != seq_enumerator_atom => panic!(
                    "Value of sequential enumerator was already present in hash map: {}:{} (old {}) in {:#?}",
                    seq_enumerator_atom, sequential_length, old_value, mappings
                ),
                _ => (),
            };
            Some(Some(mappings))
        }
        // match exactly n times
        Atom::Value(n) if *n != 1 && *n == sequential_length => {
            // no enumerator mapping (it is a Atom::Special)
            Some(Some(mappings))
        }
        _ => None,
    }
}

fn left_to_right_match_sequentials(
    seq_expr_a: &SequentialExpr,
    seq_expr_b: &SequentialExpr,
    associativities: &AssociativityHashMap,
) -> Option<Option<Atom2AtomHashMap>> {
    if seq_expr_b.operator != seq_expr_a.operator {
        return None;
    }
    let mut seq_mappings: Atom2AtomHashMap = HashMap::new();
    match left_to_right_match(
        // HERE: swapped the two first args
        &seq_expr_a.enumerator,
        &seq_expr_b.enumerator,
        associativities,
    ) {
        None => return None,
        Some(Some(enum_mappings)) => {
            seq_mappings.extend(enum_mappings);
        }
        Some(None) => (),
    };
    match left_to_right_match(
        // HERE: swapped the two first args
        &seq_expr_a.body,
        &seq_expr_b.body,
        associativities,
    ) {
        None => return None,
        Some(Some(body_mappings)) => {
            seq_mappings.extend(body_mappings);
        }
        Some(None) => (),
    };
    Some(if seq_mappings.is_empty() {
        None
    } else {
        Some(seq_mappings)
    })
}

/// Generate new expression using source, value-expression and mappings
/// mappings matches atom-names from the source-expression to the ones in the value-expression (and vice-versa)
fn generate_atom(
    scheme: &Atom,
    mappings: &Atom2AtomHashMap,
    associativities: &AssociativityHashMap,
) -> Atom {
    match scheme {
        Atom::Parenthesized(expr) => generate_expression(expr, mappings, associativities),
        Atom::Value(_) => scheme.clone(),
        Atom::Symbol(_) => match mappings.get(scheme) {
            Some(gen_atom) => gen_atom.clone(),
            None => panic!(
                "Could not find mapping for scheme {} :\nMappings :\n{:#?}\n",
                scheme, mappings
            ),
        },
        Atom::FunctionCall(fn_call) => {
            let new_args = fn_call
                .args
                .iter()
                .map(|arg| generate_atom(arg, mappings, associativities))
                .collect();
            Atom::FunctionCall(FunctionCallExpr {
                name: fn_call.name.clone(),
                args: new_args,
            })
        }
        Atom::Sequential(seq) => generate_sequential(seq, mappings, associativities),
    }
}

fn generate_expression(
    scheme_expr: &AtomExpr,
    mappings: &Atom2AtomHashMap,
    associativities: &AssociativityHashMap,
) -> Atom {
    // init new atoms and operator
    let mut atoms: Vec<Atom> = Vec::new();
    let operator: Option<Operator> = scheme_expr.operator.clone();
    // match each atom of the v-expr
    let num_scheme_atoms = scheme_expr.atoms.len();
    let mut i = 0;
    //println!("Mappings:\n{:#?}\n", mappings);
    /* 'main_loop: */
    while i < num_scheme_atoms {
        let scheme = &scheme_expr.atoms[i];

        // normal mapping
        atoms.push(generate_atom(scheme, mappings, associativities));

        i += 1;
    }

    parenthesized_atom(AtomExpr { atoms, operator })
}

fn generate_sequential(
    seq: &SequentialExpr,
    mappings: &Atom2AtomHashMap,
    associativities: &AssociativityHashMap,
) -> Atom {
    let mapped_enumerator = generate_atom(&seq.enumerator.clone(), mappings, associativities);
    match mapped_enumerator.clone() {
        Atom::Value(n) if n != 0 => {
            let n_size = n as usize;
            let mut atoms: Vec<Atom> = Vec::with_capacity(n_size);
            atoms.push(generate_atom(&seq.body.clone(), mappings, associativities));
            dress_up_expr(
                &parenthesized_atom(AtomExpr {
                    atoms: atoms
                        .into_iter()
                        .cycle()
                        .take(n_size)
                        .collect::<Vec<Atom>>(),
                    operator: if n == 1 {
                        None
                    } else {
                        Some(seq.operator.clone())
                    },
                }),
                associativities,
            )
        }
        // this includes the case where the enumerator is '0'
        _ => Atom::Sequential(Box::new(SequentialExpr {
            operator: seq.operator.clone(),
            enumerator: mapped_enumerator,
            body: generate_atom(&seq.body.clone(), mappings, associativities),
        })),
    }
}

/// Strips the expression naked.
/// Removes parentheses for easier and faster comparison.
/// Operator associativity (left, right, none) is taken into account.
///
/// Here is an example of what is meant :
/// "((A + B) + C) + D" => "A + B + C + D"
/// Of course, this only works for left-associative operators.
/// The associativity of operators can be specified in the grammar :
/// "^ r :: { ... }" makes the ^ operator right-associative.
/// There are 3 choices : l, r, n (left-, right-, non- associative)
/// If no associativity is gen (e.g. "+ :: { ... }", the default is left-associativity)
/// By default, all operators are considered to be left-associative.
///
/// Here is another example: "A + (B + C)" => "A + (B + C)"
/// We cannot remove parentheses, since '+' is not right-associative
pub fn strip_expr_naked(atom: &Atom, associativities: &AssociativityHashMap) -> Atom {
    let expr = match atom {
        Atom::Parenthesized(e) => e,
        _ => return map_over_all_sub_expressions(atom, associativities, &strip_expr_naked),
    };
    // left-associative operator case (default)
    let operator = expr.operator.clone();
    // "((A + B) + C) + D" => "(A + B) + C + D" => "A + B + C + D"
    let associativity = get_operator_associativity(&operator, associativities);

    if associativity == NonAssociative {
        // only strip all the sub-expressions naked, do nothing with the current expression
        return parenthesized_atom(AtomExpr {
            atoms: expr
                .atoms
                .iter()
                .map(|atom| map_over_all_sub_expressions(atom, associativities, &strip_expr_naked))
                .collect(),
            operator: expr.operator.clone(),
        });
    }
    if associativity == LeftRightAssociative {
        return strip_expr_naked_left_right(expr, associativities);
    }

    let left_ass = associativity == LeftAssociative;
    // TODO: remove this later on
    if !left_ass {
        assert_eq!(associativity, RightAssociative);
    }

    // merge the atoms from an atom-expression (isolated) and the current expression
    // except for the isolated one, sub-expressions are NOT yet stripped naked themselves
    let new_atoms = match if left_ass {
        expr.atoms.split_first()
    } else {
        expr.atoms.split_last()
    } {
        Some((isolated, rest)) => match isolated.clone() {
            // the isolated atom is an expression, so we should remove the parentheses
            Atom::Parenthesized(mut isolated_expr) if expr.operator == isolated_expr.operator => {
                // make the isolated expression naked
                isolated_expr = atom2atom_expr(strip_expr_naked(
                    &parenthesized_atom(isolated_expr),
                    associativities,
                ));
                // merge the isolated and rest into new expression
                let mut isolated_expr_atoms = Vec::new();
                if left_ass {
                    // isolated is left, and rest is right
                    isolated_expr_atoms.append(&mut isolated_expr.atoms);
                    isolated_expr_atoms.extend(rest.to_vec());
                } else {
                    // isolated is right, and rest is left
                    isolated_expr_atoms.extend(rest.to_vec());
                    isolated_expr_atoms.append(&mut isolated_expr.atoms);
                }
                isolated_expr_atoms
            }
            // the isolated atom is not an AtomExpr (should not merge it with the others)
            _ => expr.atoms.clone(),
        },
        // wtf ??
        None => expr.atoms.clone(),
    };

    parenthesized_atom(AtomExpr {
        atoms: new_atoms
            .iter()
            .map(|atom| map_over_all_sub_expressions(atom, associativities, &strip_expr_naked))
            .collect(),
        operator: expr.operator.clone(),
    })
}

fn strip_expr_naked_left_right(expr: &AtomExpr, associativities: &AssociativityHashMap) -> Atom {
    let mut new_atoms: Vec<Atom> = Vec::with_capacity(expr.atoms.len());
    for atom in expr.atoms.clone() {
        match atom.clone() {
            Atom::Parenthesized(atom_expr) => {
                if atom_expr.operator == expr.operator {
                    // merge this expression
                    let mut new_atom_expr =
                        strip_expr_naked_left_right(&atom_expr, associativities);
                    new_atoms.append(&mut new_atom_expr.atoms);
                } else {
                    // add it as-is (not to be merged)
                    new_atoms.push(atom);
                }
            }
            _ => new_atoms.push(atom),
        }
    }

    parenthesized_atom(AtomExpr {
        atoms: new_atoms
            .iter()
            .map(|atom| map_over_all_sub_expressions(atom, associativities, &strip_expr_naked))
            .collect(),
        operator: expr.operator.clone(),
    })
}

/// Dresses the expression up.
/// Adds parentheses for easier and faster property matching.
/// Operator associativity (left, right, none) is taken into account.
///
/// Here is an example of what is meant :
/// "A + B + C + D" => "((A + B) + C) + D"
/// Of course, this only works for left-associative operators.
/// The associativity of operators can be specified in the grammar :
/// "^ r :: { ... }" makes the ^ operator right-associative.
/// There are 3 choices : l, r, n (left-, right-, non- associative)
/// If no associativity is gen (e.g. "+ :: { ... }", the default is left-associativity)
/// By default, all operators are considered to be left-associative.
pub fn dress_up_expr(atom: &Atom, associativities: &AssociativityHashMap) -> Atom {
    let expr = match atom {
        Atom::Parenthesized(e) => e,
        _ => return atom.clone(),
    };
    // left-associative operator case (default)
    let operator = expr.operator.clone();
    // "A + B + C + D" => "(A + B + C) + D" => "((A + B) + C) + D"
    let associativity = match get_operator_associativity(&operator, associativities) {
        // It is both left and right associative, so we know that :
        // A + B + C = A + (B + C) = (A + B) + C
        LeftRightAssociative => LeftAssociative,
        ass => ass,
    };

    if associativity == NonAssociative {
        // only dress up the sub-expressions naked, do nothing with the current expression
        return parenthesized_atom(AtomExpr {
            atoms: expr
                .atoms
                .iter()
                .map(|atom| map_over_all_sub_expressions(atom, associativities, &dress_up_expr))
                .collect(),
            operator: expr.operator.clone(),
        });
    }

    let left_ass = associativity == LeftAssociative;
    // TODO: remove this later on
    if !left_ass {
        assert_eq!(associativity, RightAssociative);
    }

    // group all the atoms, except for one (isolated) into a new expression
    // sub-expressions are NOT yet dressed up themselves, not even the isolated one
    let new_atoms: Vec<Atom> = match if left_ass {
        expr.atoms.split_last()
    } else {
        expr.atoms.split_first()
    } {
        Some((isolated, rest)) => {
            let num_left = rest.len();
            if num_left == 0 {
                vec![isolated.clone()]
            } else {
                let rest_expr = if num_left == 1 {
                    rest[0].clone()
                } else {
                    Atom::Parenthesized(AtomExpr {
                        atoms: rest.to_vec(),
                        operator: operator.clone(),
                    })
                };
                if left_ass {
                    vec![rest_expr, isolated.clone()]
                } else {
                    vec![isolated.clone(), rest_expr]
                }
            }
        }
        // wtf ??
        None => return (*atom).clone(),
    };
    parenthesized_atom(AtomExpr {
        atoms: new_atoms
            .iter()
            .map(|atom| map_over_all_sub_expressions(atom, associativities, &dress_up_expr))
            .collect(),
        operator: expr.operator.clone(),
    })
}

/// Returns the associativity of the operator, based on the given vector of operators
/// If operator is None, it is seen as non-associative. The default associativity for an
/// operator is the left-associativity.
fn get_operator_associativity(
    operator: &Option<Operator>,
    associativities: &AssociativityHashMap,
) -> OperatorAssociativity {
    if *operator == None {
        NonAssociative
    } else {
        let operator_char = operator.clone().unwrap().op;
        match associativities.get(&operator_char) {
            Some(associativity) if *associativity != Unknown => *associativity,
            _ => LeftAssociative,
        }
    }
}

/// Recursively maps f over all sub expressions in atoms.
/// The returned AtomExpr struct is constructed with the given operator
/// The operators argument is used in the calls to f
fn map_over_all_sub_expressions(
    atom: &Atom,
    associativities: &AssociativityHashMap,
    f: &dyn Fn(&Atom, &AssociativityHashMap) -> Atom,
) -> Atom {
    match atom {
        Atom::Parenthesized(_) => f(&atom, associativities),
        Atom::FunctionCall(fn_call) => Atom::FunctionCall(FunctionCallExpr {
            name: fn_call.fn_name.to_string(),
            // map each argument with f
            args: fn_call
                .args
                .iter()
                .map(|arg| f(arg, associativities))
                .collect(),
        }),
        Atom::Sequential(seq) => Atom::Sequential(Box::new(SequentialExpr {
            operator: seq.operator.clone(),
            enumerator: map_over_all_sub_expressions(&seq.enumerator, associativities, f),
            body: map_over_all_sub_expressions(&seq.body, associativities, f),
        })),
        _ => (*atom).clone(),
    }
}

pub fn atom2atom_expr(atom: Atom) -> AtomExpr {
    match atom {
        Atom::Parenthesized(expr) => expr.clone(),
        _ => AtomExpr {
            atoms: vec![atom],
            operator: None,
        },
    }
}
