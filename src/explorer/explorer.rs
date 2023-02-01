// Graph Explorer

use std::{
    collections::HashMap,
    fmt,
    thread::{self, JoinHandle},
    vec,
};

use crate::parser::{
    parenthesized_atom, /*, GeneratorElement*/
    AlgebraicFunction, AlgebraicProperty, Atom, AtomExpr, Operator,
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
    pub atom_expr: AtomExpr,
    pub transform: Option<AlgebraicProperty>,
    pub parent: ExprNodeIndex,
    pub index: ExprNodeIndex,
    pub depth: u8,
}

impl PartialEq for ExprNode {
    fn eq(&self, other: &Self) -> bool {
        // not comparing neighbours, nor depths
        self.atom_expr == other.atom_expr
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl fmt::Display for ExprNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "ExprNode {{ expr: '{}', depth: {}, index: {}, parent: {}, transform: {}",
            self.atom_expr,
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
pub fn init_graph(expr: AtomExpr) -> ExprGraph {
    let base_node = ExprNode {
        atom_expr: expr,
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
    properties: &Vec<AlgebraicProperty>,
    functions: &Vec<AlgebraicFunction>,
) -> bool {
    // apply every property to all the outer-layer nodes of the graph
    let mut new_nodes: Vec<(ExprNode, ExprNode)> = Vec::new();
    let mut handles: Vec<JoinHandle<(usize, usize, Vec<AtomExpr>)>> = Vec::new();
    for node in graph
        .nodes
        .iter()
        .filter(|node| node.depth == graph.max_depth)
    {
        // Skip the rounds that would simply 'go back' one iteration.
        // For example, let 1@"X + Y" -> 2@"Y + Z" be two connected nodes
        // The transformation from 1 to 2 is "A + B = B + A". We don't want
        // to run this transformation again on 2 ("Y + X"),
        // since it will only give 1 back ("X + Y"), where we came from.
        'properties: for (p_index, property) in properties.iter().enumerate() {
            match &node.transform {
                Some(node_transform) => {
                    if node_transform == property {
                        continue 'properties;
                    }
                }
                None => (),
            }
            // one thread per exploration
            // maybe batch jobs together using worker threads later ?
            handles.push({
                let atom_expr_clone = node.atom_expr.clone();
                let property_clone = property.clone();
                let functions_clone = functions.clone();
                let node_index = node.index;
                thread::spawn(move || {
                    (
                        p_index,
                        node_index,
                        apply_property(&atom_expr_clone, &property_clone, &functions_clone),
                    )
                })
            });
        }
    }
    // join all the handles
    for handle in handles {
        let (p_index, node_index, new_expressions) = handle.join().unwrap();
        let property = &properties[p_index];
        let node = &graph.nodes[node_index];
        new_nodes.extend(new_expressions.iter().map(|new_expr| {
            (
                node.clone(),
                ExprNode {
                    atom_expr: new_expr.clone(),
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
    content.push_str(&format!("{}", node.atom_expr));
    // end printing definition line
    content.push_str(&format!("\"];\n"));

    // start printing neighbours
    content.push_str(&format!(
        "\t{} -> {} [label=< <B> {} </B> > fontsize=7 fontcolor=darkgreen];\n",
        node.parent,
        node.index,
        match &node.transform {
            Some(x) => x.to_string(),
            None => "?".to_string(),
        },
    ));
    content.push_str("\n");
    content
}

/// apply the property to the source expression
/// a property is an equality between two expressions,
/// so the left and right sides of the property are matched to the source expression
pub fn apply_property(
    src_expr: &AtomExpr,
    property: &AlgebraicProperty,
    functions: &Vec<AlgebraicFunction>,
) -> Vec<AtomExpr> {
    let mut new_expressions = match_and_apply(
        src_expr,
        &property.atom_expr_left,
        &property.atom_expr_right,
        functions,
    );
    new_expressions.extend(match_and_apply(
        src_expr,
        &property.atom_expr_right,
        &property.atom_expr_left,
        functions,
    ));
    new_expressions
}

/// try to match the src_expr to the left_expr, then generate a new expression based on right_expr
/// this returns a vector containing zero (if there is no match) or multiple (there is a match) new experssions
/// this can yield multiple new expressions, because the source expression if checked for matching, but also all
/// its 'sub-expressions'
fn match_and_apply(
    src: &AtomExpr,
    left: &AtomExpr,
    right: &AtomExpr,
    functions: &Vec<AlgebraicFunction>,
) -> Vec<AtomExpr> {
    // call 'atom_expressions_match' on every expression (src_expr and sub-expression in parentheses)
    let mut new_expressions: Vec<AtomExpr> = Vec::new();
    // base call on src_expr
    match atom_expressions_match(src, left) {
        Some(mappings) => {
            let new_expr = generate_new_expression(right, &mappings, &functions);
            new_expressions.push(new_expr);
        }
        None => (),
    }
    // recursively call 'atom_expressions_match' on the sub-expressions
    let num_src_atoms = src.atoms.len();
    for (sub_i, sub) in src
        .atoms
        .iter()
        .enumerate()
        .filter_map(|(atom_i, atom)| match atom {
            Atom::Parenthesized(sub_expr) => Some((atom_i, (*sub_expr).clone())),
            Atom::FunctionCall(_) if num_src_atoms != 1 => {
                Some((atom_i, atom2atom_expr((*atom).clone())))
            }
            _ => None,
        })
    {
        new_expressions.extend(
            match_and_apply(&sub, left, right, functions)
                .iter()
                .map(|sub_tr| {
                    // reconstruct like that : <everything b4> (new sub) <everything after>
                    // only the sub changes, compared to the src. no other atoms
                    let mut src_atoms_prefix = src.atoms[..sub_i].to_vec().clone();
                    src_atoms_prefix.push(parenthesized_atom(sub_tr.clone()));
                    let src_atoms_suffix = src.atoms[sub_i + 1..].to_vec().clone();
                    src_atoms_prefix.extend(src_atoms_suffix);
                    AtomExpr {
                        atoms: src_atoms_prefix,
                        operator: src.operator.clone(),
                    }
                })
                .collect::<Vec<AtomExpr>>(),
        );
    }
    new_expressions
}

fn atom_expressions_match(
    src_expr: &AtomExpr,  // source expression
    dest_expr: &AtomExpr, // expression to match against
) -> Option<Atom2AtomHashMap> {
    // mappings of atom-to-atom between src_expr and (p_expr/v_expr)
    let mut mappings: Atom2AtomHashMap = HashMap::new();
    // number of atoms
    let num_src_atoms = src_expr.atoms.len();
    let num_dest_atoms = dest_expr.atoms.len();
    // check the two operators
    if src_expr.operator != dest_expr.operator {
        return None;
    }
    // Match one-by-one (cannot compare lengths bc of things like '...' or generators)
    let mut i = 0;
    'main_loop: while i < num_src_atoms && i < num_dest_atoms {
        // compare atoms
        let atom_a = &src_expr.atoms[i];
        let atom_b = &dest_expr.atoms[i];
        match left_to_right_match(atom_a, atom_b) {
            (true, None) => (),
            (true, Some(par_mappings)) => {
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
                i += 1;
                continue 'main_loop;
            }
            (false, _) => return None,
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
        i += 1;
    }
    if i != num_src_atoms || i != num_dest_atoms {
        return None;
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
///
/// TODO: convert the return type to Result<Option<A...>>, or Option<Option<A...>>
fn left_to_right_match(atom_a: &Atom, atom_b: &Atom) -> (bool, Option<Atom2AtomHashMap>) {
    // we match the second atom !!
    match atom_b {
        Atom::Parenthesized(par_b) => match atom_a {
            Atom::Parenthesized(par_a) => match atom_expressions_match(par_a, par_b) {
                Some(par_mappings) => (true, Some(par_mappings)),
                None => (false, None),
            },
            _ => (false, None),
        },
        Atom::Value(_) => match atom_a {
            Atom::Value(_) | Atom::Special(_) | Atom::Parenthesized(_) | Atom::FunctionCall(_) => {
                (true, None)
            }
            _ => todo!("Unsupported atom type for atom_a: {:?}", atom_a),
        },
        Atom::Special(_) => (atom_a == atom_b, None),
        Atom::FunctionCall((fn_name_b, fn_expr_args_b)) => match atom_a {
            // a function call is only mappable to the same function call, with each argument
            // being mappable to it's alter-atom
            Atom::FunctionCall((fn_name_a, fn_expr_args_a)) => {
                // It should be the same function (by name and number of arguments)
                if fn_name_b != fn_name_a || fn_expr_args_b.len() != fn_expr_args_a.len() {
                    return (false, None);
                }
                // check 'mappability' of each argument
                let mut fn_mappings: Atom2AtomHashMap = HashMap::new();
                for (expr_arg_a, expr_arg_b) in fn_expr_args_a.iter().zip(fn_expr_args_b.iter()) {
                    match atom_expressions_match(expr_arg_a, expr_arg_b) {
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
                                            return (false, None)
                                        }
                                    },
                                    _ => (),
                                }
                            }
                        },
                        None => return (false, None),
                    }
                }
                // arguments are all mappable
                (true, Some(fn_mappings))
            }
            _ => (false, None),
        },
        _ => todo!("Unsupported atom type for atom_b: {:?}", atom_b),
    }
}

/// Generate new expression using source, value-expression and mappings
/// mappings matches atom-names from the source-expression to the ones in the value-expression (and vice-versa)
fn generate_new_expression(
    v_expr: &AtomExpr,
    mappings: &Atom2AtomHashMap,
    functions: &Vec<AlgebraicFunction>,
) -> AtomExpr {
    // init new atoms and operator
    let mut atoms: Vec<Atom> = Vec::new();
    let operator: Option<Operator> = v_expr.operator.clone();
    // match each atom of the v-expr
    let num_v_atoms = v_expr.atoms.len();
    let mut i = 0;
    //println!("Mappings:\n{:#?}\n", mappings);
    /* 'main_loop: */
    while i < num_v_atoms {
        let atom_v = &v_expr.atoms[i];

        // normal mapping
        atoms.push(match atom_v {
            Atom::Parenthesized(par_v) => {
                parenthesized_atom(generate_new_expression(par_v, mappings, functions))
            }
            Atom::Special(_) => atom_v.clone(),
            Atom::FunctionCall((fn_name, _)) => {
                for function in functions {
                    if function.name == *fn_name {
                        return generate_new_expression(
                            &function.atom_expr_right,
                            mappings,
                            functions,
                        );
                    }
                }
                // function is not defined
                panic!(
                    "No function named \"{fn_name}\"\nFunctions :\n{:?}",
                    functions
                );
            }
            /*
            Atom::Generator(gen_expr) => {
                todo!("Generator expressions not handled yet");
                // check for iterator (must be a numeral, or we can't generate)
                let num_iterations = match mappings.get(&gen_expr.iterator) {
                    Some(Either::Left(Atom::Special(c))) => c.to_digit(10),
                    _ => panic!("Iterator value was not a special value for generator expression.\natom_v: {atom_v}, expr_v: {v_expr}\nMappings :{:#?}\n", mappings),
                }.unwrap();
                // remove the special operator ('{' or '}')
                if ! operators.is_empty() {
                    if operators.last().unwrap().op == '}' {
                        operators.pop().unwrap();
                    } else {
                        // should not be added (guard at beginning of the loop)
                        assert_eq!(v_expr.operators[i - 1].op, '{');
                    }
                }
                // get mappings for the atoms inside the generator-expression
                let vec_capacity = gen_expr.elements.len() / 2;
                let mut mapped_atoms: Vec<Atom> = Vec::with_capacity(vec_capacity);
                let mut gen_operator: Option<Operator> = None;
                for gen_element in &gen_expr.elements {
                    match gen_element {
                        GeneratorElement::GenAtom(atom) => {
                            match mappings.get(atom) {
                                Some(Either::Left(atom)) => mapped_atoms.push((*atom).clone()),
                                Some(Either::Right(atom_expr)) => mapped_atoms.push(parenthesized_atom((*atom_expr).clone())),
                                None => panic!("Could not find mapping for gen-element {}", gen_element),
                            }
                        },
                        GeneratorElement::GenOperator(op) => gen_operators.push((*op).clone()),
                    }
                }
                // generate part of the expression
                operator = gen_operator;
                for _ in 0..num_iterations {
                    atoms.extend(mapped_atoms.clone());
                }
                i += num_iterations as usize;
                continue 'main_loop;
            }
            */
            _ => match mappings.get(atom_v) {
                Some(atom) => atom.clone(),
                None => panic!(
                    "Could not find mapping for atom {} in expr '{}'\nMappings :\n{:#?}\n",
                    atom_v, v_expr, mappings
                ),
            },
        });

        i += 1;
    }
    AtomExpr { atoms, operator }
}

/// Strips the expression naked.
/// Removes parentheses for easier and faster comparison.
///
/// Here is an example of what is meant :
/// "((A + B) + C) + D" => "A + B + C + D"
/// Of course, this only works for left-associative operators.
/// The associativity of operators cannot be specified in the grammar as of yet.
/// All operators are considered to be left-associative.
///
/// Here is another example: "A + (B + C)" => "A + (B + C)"
/// We cannot remove parentheses, since '+' is not right-associative
/// (again, this should be customizable later on).
pub fn strip_expr_naked(expr: &AtomExpr) -> AtomExpr {
    // left-associative operator case (default)
    let operator = expr.operator.clone();
    // "((A + B) + C) + D" => "(A + B) + C + D" => "A + B + C + D"

    // swap 'split_first' with 'split_last' for right-associative operators (further checks needed tho)
    match expr.atoms.split_first() {
        Some((isolated, rest)) => match isolated {
            Atom::Parenthesized(sub_expr) if expr.operator == sub_expr.operator => {
                // strip this sub-expression naked
                let naked_sub_expr = strip_expr_naked(&sub_expr);
                // merge the sub-expression into the current one
                let mut before_atoms = naked_sub_expr.atoms.clone();
                before_atoms.extend(rest.to_vec());
                AtomExpr {
                    atoms: before_atoms,
                    operator,
                }
            }
            _ => AtomExpr {
                atoms: {
                    expr.atoms
                        .iter()
                        .enumerate()
                        .map(|(index, atom)| match (index, atom) {
                            (index, Atom::Parenthesized(sub_expr)) if index != 0 => {
                                Atom::Parenthesized(strip_expr_naked(&sub_expr))
                            }
                            _ => (*atom).clone(),
                        })
                        .collect()
                },
                operator,
            },
        },
        // wtf ??
        None => (*expr).clone(),
    }
}

/// Dresses the expression up.
/// Adds parentheses for easier and faster property matching.
///
/// Here is an example of what is meant :
/// "A + B + C + D" => "((A + B) + C) + D"
/// Of course, this only works for left-associative operators.
/// The associativity of operators cannot be specified in the grammar as of yet.
/// All operators are considered to be left-associative.
/// (again, this should be customizable later on).
pub fn dress_up_expr(expr: &AtomExpr) -> AtomExpr {
    // left-associative operator case (default)
    let operator = expr.operator.clone();
    // "A + B + C + D" => "(A + B + C) + D" => "((A + B) + C) + D"

    // swap 'split_last' with 'split_first' for right-associative operators (further checks needed tho)
    match expr.atoms.split_last() {
        Some((isolated, rest)) => {
            let dressed_up_isolated = match isolated {
                Atom::Parenthesized(sub_expr) => Atom::Parenthesized(dress_up_expr(sub_expr)),
                atom => (*atom).clone(),
            };
            let num_left = rest.len();
            if num_left == 0 {
                return atom2atom_expr(dressed_up_isolated);
            }
            let dressed_up_rest = if num_left == 1 {
                rest[0].clone()
            } else {
                Atom::Parenthesized(dress_up_expr(&AtomExpr {
                    atoms: rest.to_vec(),
                    operator: operator.clone(),
                }))
            };
            AtomExpr {
                // swap this vector when handling right-associative
                atoms: vec![dressed_up_rest, dressed_up_isolated],
                operator: operator.clone(),
            }
        }
        // wtf ??
        None => return (*expr).clone(),
    }
}

pub fn atom2atom_expr(atom: Atom) -> AtomExpr {
    AtomExpr {
        atoms: vec![atom],
        operator: None,
    }
}
