// Graph Explorer

use std::{collections::HashMap};

use crate::aps_parser::{self, AtomExpr, AlgebraicProperty, AlgebraicFunction, Atom, Operator};

#[derive(Debug, PartialEq, Hash)]
pub enum Either<T1, T2> {
    Left(T1),
    Right(T2),
}

#[derive(Debug)]
pub struct ExprGraph {
    nodes: Vec<ExprNode>,
    max_depth: u8,
}

#[derive(Debug, Clone, Hash)]
pub struct ExprNode {
    atom_expr: aps_parser::AtomExpr,
    neighbours: Vec<ExprNode>,
    depth: i8,
    index: i8,
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

type PropertyMapping = HashMap<Atom, Either<Atom, AtomExpr>>;

/// init a expression-graph
pub fn init_graph(expr: AtomExpr) -> ExprGraph {
    let base_node = ExprNode {
        atom_expr: expr,
        neighbours: Vec::new(),
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
    properties: Vec<AlgebraicProperty>,
    functions: Vec<AlgebraicFunction>,
) -> bool {
    let mut new_nodes: Vec<(ExprNode, ExprNode)> = Vec::new();
    for i in 0..graph.nodes.len() {
        let node = graph.nodes[i].clone();
        for property in properties.clone() {
            match apply_property(&node.atom_expr, &property, &functions) {
                Some(new_expr) => new_nodes.push((
                    node.clone(),
                    ExprNode {
                        atom_expr: new_expr,
                        neighbours: vec![],
                        depth: -1,
                        index: -1,
                    }
                )),
                None => continue,
            }
        }
    }
    let mut at_least_one_new_node = false;
    let mut new_node_index = graph.nodes.len() as i8;
    for (mut src_node, mut new_node) in new_nodes {
        if ! graph.nodes.contains(&new_node) {
            println!("New node: {:?}", new_node);
            at_least_one_new_node = true;
            // add source node to the new node's neighbours
            new_node.neighbours.push(src_node.clone());
            // add the new node to the source node's neighbours
            src_node.neighbours.push(new_node.clone());
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

pub fn print_graph_dot_format(graph: &ExprGraph) -> () {
    println!("/* DOT FORMAT START */");
    // start printing the graph
    print!("digraph G {{\n");
    for node in &graph.nodes {
        print_node_dot_format(node);
    }
    print!("}}\n");
    println!("/* DOT FORMAT END */");
}

fn print_node_dot_format(node: &ExprNode) -> () {
    // start printing definition line
    print!("\t{:?} [label=\"", node.index);
    // print label of node
    print!("{}", node.atom_expr);
    // end printing definition line
    print!("\"]\n");

    // start printing neighbours
    for neighbour in &node.neighbours {
        if node.index < neighbour.index {
            print!("\t{:?} -> {:?}\n", node.index, neighbour.index)
        }
    }
    print!("\n");
}

fn apply_property(src_expr: &AtomExpr, property: &AlgebraicProperty, functions: &Vec<AlgebraicFunction>) -> Option<AtomExpr> {
    match atom_expressions_match(
            src_expr,
            &property.atom_expr_left,
            functions)
        {
        Some(mappings) => Some(
            generate_new_expression(&property.atom_expr_right, &mappings)
        ),
        None => match atom_expressions_match(
            src_expr,
            &property.atom_expr_left,
            functions
        ) {
            Some(mappings) => Some(
                generate_new_expression(&property.atom_expr_right, &mappings)
            ),
            None => None
        }
    }
}

fn atom_expressions_match(
    src_expr: &AtomExpr, // source expression
    p_expr: &AtomExpr, // expression to match against
    functions: &Vec<AlgebraicFunction>, // functions (which might be used in the properties)
) -> Option<PropertyMapping> {
    // mappings of atom-to-atom between src_expr and (p_expr/v_expr)
    let mut mappings: PropertyMapping = HashMap::new();
    // number of atoms
    let num_src_atoms = src_expr.atoms.len();
    let num_p_atoms = p_expr.atoms.len();
    // Match one-by-one (cannot compare lengths bc of things like '...' or generators)
    let mut i = 0;
    'main_loop: while i < num_src_atoms && i < num_p_atoms
    {
        // compare operators (not for first iteration)
        if i != 0 && src_expr.operators[i - 1] != p_expr.operators[i - 1] {
            return None;
        }
        // compare atoms
        let atom_a = &src_expr.atoms[i];
        let atom_b = &p_expr.atoms[i];
        //println!("atom_a = {:?}\natom_b = {:?}\n", atom_a, atom_b);
        // map rest of src_atom to '...'
        if atom_b == &Atom::Extension {
            mappings.insert(atom_b.clone(), Either::Right(
                AtomExpr {
                    atoms: src_expr.atoms[i..].to_vec(),
                    operators: src_expr.operators[i+1..].to_vec()
                }
            ));
            if i + 1 != num_p_atoms {
                return None
            }
            return Some(mappings)
        }
        match left_to_right_match(atom_a, atom_b, functions) {
            (
                true,
                None
            ) => (),
            (
                true,
                Some(par_mappings)
            ) => {
                mappings.extend(par_mappings);
                i += 1;
                continue 'main_loop
            },
            (
                false,
                _
            ) => return None,
        }
        // check with already-existing mapping, or insert as new mapping
        match mappings.get(atom_b) {
            Some(mapped_atom) => match mapped_atom {
                Either::Left(atom_c) => {
                    // atom_c should be equal to atom_a
                    if atom_c != atom_a {
                        return None
                    }
                },
                Either::Right(_) => return None,
            },
            None => assert_eq!(
                mappings.insert(atom_b.clone(), Either::Left(atom_a.clone())),
                None
            ),
        }
        // go to next (operator, atom) pair
        i += 1;
    }
    if i != num_src_atoms {
        return None
    }
    Some(mappings)
}

/// Is the atom_a mappable on atom_b ?
/// 
/// e.g. (A + B) mappable on A
/// but A is NOT mappable on (A + B)
/// and (A + B) mappable on (X + Y)
fn left_to_right_match(
    atom_a: &Atom,
    atom_b: &Atom,
    functions: &Vec<AlgebraicFunction>
) -> (bool, Option<PropertyMapping>) {
    //println!("Map {:?} to {:?} ?\n", atom_a, atom_b);
    match atom_b { // we match the second atom !!
        Atom::Parenthesized(par_b) => match atom_a {
            Atom::Parenthesized(par_a) => match atom_expressions_match(
                par_a,
                par_b,
                functions
            ) {
                Some(par_mappings) => {
                    (true, Some(par_mappings))
                },
                None => (false, None),
            },
            _ => (false, None)
        }
        Atom::Value(_) => match atom_a {
            Atom::Value(_) |
            Atom::Special(_) |
            Atom::Parenthesized(_) => (true, None),
            _ => todo!()
            
        },
        Atom::Special(_) => (atom_a == atom_b, None),
        Atom::Extension => todo!(),
        Atom::Generator(_) => todo!(),
    }
}

/// Generate new expression using source, value-expression and mappings
fn generate_new_expression(
    v_expr: &AtomExpr,
    mappings: &PropertyMapping
) -> AtomExpr {
    // init new atoms and operators
    let mut atoms: Vec<Atom> = Vec::new();
    let mut operators: Vec<Operator> = Vec::new();
    // match each atom of the v-expr
    let num_v_atoms = v_expr.atoms.len();
    let mut i = 0;
    //println!("Mappings:\n{:#?}\n", mappings);
    while i  < num_v_atoms
    {
        let atom_v = &v_expr.atoms[i];
        // immediately add operator
        if i != 0 {
            operators.push(v_expr.operators[i - 1].clone());
        }
        // expand extension expressions
        if atom_v == &Atom::Extension {
            let sub_expr: AtomExpr = match mappings.get(atom_v) {
                Some(Either::Left(atom)) => panic!("Extension mapping is atom, not atom-expr: {:?}\n", atom),
                Some(Either::Right(expr)) => expr.clone(),
                None => panic!("Could not find mapping for extension\n")
            };
            atoms.extend(sub_expr.atoms);
            operators.extend(sub_expr.operators);
        } else {
            // normal mapping
            atoms.push(
                match atom_v {
                    Atom::Parenthesized(par_v) => {
                        Atom::Parenthesized(generate_new_expression(par_v, mappings))
                    },
                    _ => match mappings.get(atom_v) {
                        Some(Either::Left(atom)) => atom.clone(),
                        Some(Either::Right(_)) => panic!("Found extension instead of atom\n"),
                        None => panic!("Could not find mapping for atom {:?}\n", atom_v)
                    }
                }
            )
        }
        i += 1;
    }
    AtomExpr { atoms, operators }
}

#[test]
fn test() {
    let src_expr = match aps_parser::atom_expr_p::<aps_parser::ApsParserKind>(
        "(X + Y) + Z"
    ) {
        Ok(("", expr)) => expr,
        Ok((rest, parsed)) => panic!(
            "Failed to parse everything:\n'{}'\nParsed :\n{:#?}\n",
            rest,
            parsed
        ),
        Err(err) => panic!("Failed to parse expression:\n{:#?}", err)
    };
    let property = match aps_parser::root::<aps_parser::ApsParserKind>(
        "+ :: { (A + B) + C = A + (B + C) ;; }"
    ) {
        Ok(("", parsed)) => match parsed.first().unwrap() {
            aps_parser::AlgebraicObject::PropertyGroup(
                aps_parser::BraceGroup { properties, operator: _ }
            ) => properties.first().unwrap().clone(),
            _ => panic!("No a brace group:\n{:#?}\n", parsed),
        },
        Ok((rest, parsed)) => panic!(
            "Failed to parse everything:\n'{}'\nParsed :\n{:#?}\n",
            rest,
            parsed
        ),
        Err(err) => panic!("Failed to parse property:\n{:#?}", err)
    };
    let new_expr = match apply_property(&src_expr, &property, &vec![]) {
        Some(expr) => expr,
        None => panic!("Could not apply property")
    };
    println!("New Expression:\n{:#?}\n", new_expr);
}
