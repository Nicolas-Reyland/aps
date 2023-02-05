// Graph Explorer

use std::{
    collections::{HashMap, HashSet},
    fmt,
    sync::mpsc::channel,
    vec,
};

use crate::clothing::strip_expr_naked;
use crate::generate::generate_atom;
use crate::parser::{
    parenthesized_atom, AlgebraicProperty, AssociativityHashMap, Atom, AtomExpr, FunctionCallExpr,
    SequentialExpr,
};
use crate::threads::*;

#[derive(Debug, Clone)]
pub struct AtomGraph {
    pub nodes: Vec<GraphNode>,
    max_depth: u8,
}

impl fmt::Display for AtomGraph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ExprGraph (max_depth: {}) {{\n", self.max_depth)?;
        for node in &self.nodes {
            write!(f, "\t{},\n", node)?;
        }
        write!(f, "}}")
    }
}

pub type GraphNodeIndex = usize;

#[derive(Debug, Clone, Hash)]
pub struct GraphNode {
    // TODO: make atom_expr a parser::Atom, since
    // one should be able to ask for a prove such as "A * B = 0"
    pub atom: Atom,
    pub transform: Option<AlgebraicProperty>,
    pub parent: GraphNodeIndex,
    pub index: GraphNodeIndex,
    pub depth: u8,
}

impl PartialEq for GraphNode {
    fn eq(&self, other: &Self) -> bool {
        // not comparing neighbours, nor depths
        self.atom == other.atom
    }
}

impl fmt::Display for GraphNode {
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

pub type Atom2AtomHashMap = HashMap<Atom, Atom>;

/// init a expression-graph
pub fn init_graph(atom: Atom) -> AtomGraph {
    let base_node = GraphNode {
        atom,
        parent: 0,
        transform: None,
        depth: 0,
        index: 0,
    };
    let graph = AtomGraph {
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
    graph: &mut AtomGraph,
    properties: &HashSet<AlgebraicProperty>,
    associativities: &AssociativityHashMap,
) -> bool {
    // apply every property to all the outer-layer nodes of the graph
    let mut new_nodes: Vec<(GraphNode, GraphNode)> = Vec::new();
    // currently running threads
    let mut handles: Vec<ExplorationHandle> = Vec::new();
    // MPSC channel
    let (sender, receiver) = channel();
    let mut early_collections: i32 = 0;
    // explore each node of the graph outer-layer
    for node in graph
        .nodes
        .iter()
        .filter(|node| node.depth == graph.max_depth)
    {
        for property in get_relevant_properties(&node, properties) {
            // wait until we can start a new thread
            early_collections += wait_for_next_thread(&receiver, graph, &mut new_nodes, &handles);
            println!("early_collections: {}", early_collections);
            // remove_deprecated_handles!(receiver, graph, new_nodes, handles, deprecated_handles);
            let mut num_threads = handles.len();
            handles = handles
                .into_iter()
                .filter_map(|handle| {
                    if handle.is_finished() {
                        println!(
                            "END THREAD {} -> {} ({})",
                            num_threads,
                            num_threads - 1,
                            early_collections
                        );
                        num_threads -= 1;
                        handle
                            .join()
                            .unwrap()
                            .expect(&format!("Could not join handle"));
                        if early_collections != 0 {
                            println!("Skipping collection ({})", num_threads);
                            early_collections -= 1;
                        } else {
                            println!("loop-collected: {}", early_collections);
                            collect_once(&receiver, &graph, &mut new_nodes);
                        }
                        None
                    } else {
                        Some(handle)
                    }
                })
                .collect();
            println!(
                "START THREAD ({}) {} -> {}",
                num_threads,
                handles.len(),
                handles.len() + 1
            );
            // start a new thread, for this property matching
            explore_property(&sender, node, property, associativities, &mut handles);
        }
    }
    collect_all_remaining_threads(&receiver, early_collections, graph, &mut new_nodes, handles);
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

/// Skip the rounds that would simply 'go back' one iteration.
/// Here is an example: let 1@"X + Y" -> 2@"Y + Z" be two connected nodes
/// The transformation from 1 to 2 is "A + B = B + A". We don't want
/// to run this transformation again on 2 ("Y + X"),
/// since it will only give 1 back ("X + Y"), where we came from.
fn get_relevant_properties(
    node: &GraphNode,
    properties: &HashSet<AlgebraicProperty>,
) -> HashSet<AlgebraicProperty> {
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
    properties
        .clone()
        .into_iter()
        .filter(|property| !node_has_transform || node_transform != *property)
        .collect()
}

pub fn print_graph_dot_format(graph: &AtomGraph) -> String {
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

fn print_node_dot_format(node: &GraphNode) -> String {
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
    let mut new_atoms = match_and_apply(
        src_atom,
        &property.left_atom,
        &property.right_atom,
        associativities,
    );
    new_atoms.extend(match_and_apply(
        src_atom,
        &property.right_atom,
        &property.left_atom,
        associativities,
    ));
    new_atoms
}

/// try to match the src_expr to the left_expr, then generate a new expression based on right_expr
/// this returns a vector containing zero (if there is no match) or multiple (there is a match) new expressions
/// this can yield multiple new expressions, because the source expression if checked for matching, but also all
/// its 'sub-expressions' (including arguments of function calls)
pub fn match_and_apply(
    src: &Atom,
    left: &Atom,
    right: &Atom,
    associativities: &AssociativityHashMap,
) -> HashSet<Atom> {
    // call 'atom_expressions_match' on every expression (src_expr and sub-expression in parentheses)
    let mut new_atoms: HashSet<Atom> = HashSet::new();
    // base call on src
    let empty_mappings: Atom2AtomHashMap = HashMap::new();
    match &left_to_right_match(src, left, associativities) {
        // use right atom as-is
        Some(None) if sufficient_mappings(right, &empty_mappings) => {
            new_atoms.insert(right.clone());
            ()
        }
        // need to generate new atom, based on right atom
        Some(Some(mappings)) if sufficient_mappings(right, mappings) => {
            new_atoms.insert(generate_atom(right, mappings, associativities));
        }
        _ => (),
    };
    // recursively call 'match_and_apply' on all sorts of sub-expressions
    // these are: expressions, function-call-args
    // all atom types are 'penetrated' (also sequential-expressions)
    new_atoms.extend(match_and_apply_all_sub_expressions(
        src,
        left,
        right,
        associativities,
    ));

    new_atoms
}

fn match_and_apply_all_sub_expressions(
    src: &Atom,
    left: &Atom,
    right: &Atom,
    associativities: &AssociativityHashMap,
) -> HashSet<Atom> {
    let mut new_atoms = HashSet::new();
    match src {
        Atom::Parenthesized(expr) => new_atoms.extend(
            expr.atoms
                .iter()
                .enumerate()
                .map(|(atom_index, expr_elt)| {
                    multiple_replace_nth_atom(
                        &expr.atoms,
                        atom_index,
                        match_and_apply(expr_elt, left, right, associativities),
                    )
                })
                .flatten()
                .map(|new_expr_atoms| {
                    Atom::Parenthesized(AtomExpr {
                        atoms: new_expr_atoms,
                        operator: expr.operator.clone(),
                    })
                }),
        ),
        Atom::FunctionCall(fn_call) => new_atoms.extend(
            fn_call
                .args
                .iter()
                .enumerate()
                .map(|(atom_index, arg)| {
                    multiple_replace_nth_atom(
                        &fn_call.args,
                        atom_index,
                        match_and_apply(arg, left, right, associativities),
                    )
                })
                .flatten()
                .map(|new_args| {
                    Atom::FunctionCall(FunctionCallExpr {
                        name: fn_call.name.to_owned(),
                        args: new_args,
                    })
                }),
        ),
        Atom::Sequential(seq) => {
            // go through enumerate
            new_atoms.extend(
                match_and_apply(&seq.enumerator, left, right, associativities)
                    .into_iter()
                    .map(|enumerator| {
                        Atom::Sequential(Box::new(SequentialExpr {
                            operator: seq.operator.clone(),
                            enumerator,
                            body: seq.body.clone(),
                        }))
                    }),
            );
            // go through body
            new_atoms.extend(
                match_and_apply(&seq.body, left, right, associativities)
                    .into_iter()
                    .map(|body| {
                        Atom::Sequential(Box::new(SequentialExpr {
                            operator: seq.operator.clone(),
                            enumerator: seq.enumerator.clone(),
                            body,
                        }))
                    }),
            );
        }
        _ => (),
    };
    new_atoms
}

/// Returns all the possible
fn multiple_replace_nth_atom(
    atoms: &Vec<Atom>,
    index: usize,
    new_atoms: HashSet<Atom>,
) -> HashSet<Vec<Atom>> {
    new_atoms
        .iter()
        .map(|new_atom| {
            let mut atoms_clone = atoms.clone();
            atoms_clone[index] = new_atom.clone();
            atoms_clone
        })
        .collect()
}

fn left_to_right_match_expressions(
    src_expr: &AtomExpr,  // source expression
    dest_expr: &AtomExpr, // expression to match against
    associativities: &AssociativityHashMap,
) -> Option<Option<Atom2AtomHashMap>> {
    // check the two operators
    if src_expr.operator != dest_expr.operator || src_expr.atoms.len() != dest_expr.atoms.len() {
        return None;
    }
    // mappings of atom-to-atom between src_expr and (p_expr/v_expr)
    let mut mappings: Atom2AtomHashMap = HashMap::new();
    // left-to-right-match atoms one-by-one
    for (sub_src_atom, sub_dest_atom) in src_expr.atoms.iter().zip(dest_expr.atoms.clone()) {
        match left_to_right_match(&sub_src_atom, &sub_dest_atom, associativities) {
            Some(None) => (),
            Some(Some(new_mappings)) => {
                for (key, value) in new_mappings {
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
            }
            None => return None,
        }
    }

    Some(if mappings.is_empty() {
        None
    } else {
        Some(mappings)
    })
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
                left_to_right_match_expressions(par_a, par_b, associativities)
            }
            _ => None,
        },
        Atom::Symbol(_) => Some(Some(
            // map atom_b to whatever atom_a is
            vec![(atom_b.clone(), atom_a.clone())].into_iter().collect(),
        )),
        Atom::Value(_) => {
            if atom_a == atom_b {
                Some(None)
            } else {
                None
            }
        }
        Atom::FunctionCall(fn_call_b) => match atom_a {
            // a function call is only mappable to the same function call, with each argument
            // being mappable to it's alter-atom
            Atom::FunctionCall(fn_call_a) => {
                // It should be the same function (by name and number of arguments)
                if fn_call_b.name != fn_call_a.name || fn_call_b.args.len() != fn_call_a.args.len()
                {
                    return None;
                }
                // check 'mappability' of each argument
                let mut fn_mappings: Atom2AtomHashMap = HashMap::new();
                for (arg_a, arg_b) in fn_call_a.args.iter().zip(fn_call_b.args.iter()) {
                    match left_to_right_match(arg_a, arg_b, associativities) {
                        Some(Some(arg_mappings)) => {
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
                        Some(None) => (),
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

/// Example : match "(A * A) * A" to "# * : N : A #"
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
    // it has to be an expression, or else there won't be two or more elements
    let naked_expr_a =
        match strip_expr_naked(&parenthesized_atom((*expr_a).clone()), associativities) {
            Atom::Parenthesized(e) => e,
            _ => return None,
        };
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

/// Make sure the mappings contain all the symbols that can be found in the scheme
/// That prevents atoms such as "0" to generate an atom "A * 0" ('A' is not `connu au bataillon`)
fn sufficient_mappings(scheme: &Atom, mappings: &Atom2AtomHashMap) -> bool {
    match scheme {
        Atom::Parenthesized(expr) => expr
            .atoms
            .iter()
            .all(|sub_atom| sufficient_mappings(sub_atom, mappings)),
        Atom::Symbol(_) => mappings.contains_key(scheme),
        Atom::Value(_) => true,
        Atom::FunctionCall(fn_call) => fn_call
            .args
            .iter()
            .all(|arg| sufficient_mappings(arg, mappings)),
        Atom::Sequential(seq) => {
            sufficient_mappings(&seq.enumerator, mappings)
                && sufficient_mappings(&seq.body, mappings)
        }
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
