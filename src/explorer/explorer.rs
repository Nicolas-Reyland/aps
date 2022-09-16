// Graph Explorer

use std::{collections::HashMap, mem::discriminant};

use crate::aps_parser::{self, AtomExpr, AlgebraicProperty, AlgebraicFunction, Atom, Operator, ApsParserKind, BraceGroup, AlgebraicObject};

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

#[derive(Debug, Clone)]
pub struct ExprNode {
    atom_expr: aps_parser::AtomExpr,
    neighbours: Vec<ExprNode>,
    depth: u8,
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

/// init a expression-graph
pub fn init_graph(expr: AtomExpr) -> ExprGraph {
    let base_node = ExprNode {
        atom_expr: expr,
        neighbours: Vec::new(),
        depth: 0,
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
) -> u8 {
    let mut new_nodes: Vec<(ExprNode, ExprNode)> = Vec::new();
    for i in 0..graph.nodes.len() {
        let node = graph.nodes[i].clone();
        for property in properties.clone() {
            match apply_property(&node.atom_expr, &property, &functions) {
                Some(new_node) => new_nodes.push((node.clone(), new_node)),
                None => continue,
            }
        }
    }
    let mut num_new_nodes: u8 = 0;
    let mut at_least_one_new_node = false;
    for (mut src_node, mut new_node) in new_nodes {
        if ! graph.nodes.contains(&new_node) {
            at_least_one_new_node = true;
            // increment the number of new nodes
            num_new_nodes += 1;
            // add source node to the new node's neighbours
            new_node.neighbours.push(src_node.clone());
            // add the new node to the source node's neighbours
            src_node.neighbours.push(new_node.clone());
            // set the depth of the new node
            new_node.depth = src_node.depth + 1;
            // add new node to graph
            graph.nodes.push(new_node)
        }
    }
    if at_least_one_new_node {
        graph.max_depth += 1;
    }
    num_new_nodes
}

fn apply_property(src_expr: &AtomExpr, property: &AlgebraicProperty, functions: &Vec<AlgebraicFunction>) -> Option<ExprNode> {
    match atom_expressions_match(
            src_expr,
            &property.atom_expr_left,
            &property.atom_expr_right,
            functions)
        {
        Some(new_expr) => Some(ExprNode {
            atom_expr: new_expr,
            neighbours: Vec::new(),
            depth: 0,
        }),
        None => match atom_expressions_match(
                src_expr,
                &property.atom_expr_right,
                &property.atom_expr_left,
                functions) {
            Some(new_expr) => Some(ExprNode {
                atom_expr: new_expr,
                neighbours: Vec::new(),
                depth: 0,
            }),
            None => None,
        }
    }
}

fn atom_expressions_match(
    src_expr: &AtomExpr, // source expression
    p_expr: &AtomExpr, // expression to match against
    v_expr: &AtomExpr, // value expression (used to transform src_expr)
    functions: &Vec<AlgebraicFunction>
) -> Option<AtomExpr> {
    // mappings of atom-to-atom between src_expr and (p_expr/v_expr)
    let mut mappings: HashMap<Atom, Either<Atom, AtomExpr>> = HashMap::new();
    // number of atoms
    let num_src_atoms = src_expr.atoms.len();
    let num_p_atoms = p_expr.atoms.len();
    // number of operators
    let num_src_operators = num_src_atoms - 1;
    let num_p_operators = num_p_atoms - 1;
    // Match one-by-one (cannot compare lengths bc of things like '...' or generators)
    let mut i = 0;
    while i < num_src_atoms && i < num_p_atoms
    {
        // compare operators (not for first iteration)
        if i != 0 && src_expr.operators[i - 1] != p_expr.operators[i - 1] {
            return None;
        }
        // compare atoms
        let atom_a = &src_expr.atoms[i];
        let atom_b = &p_expr.atoms[i];
        println!("atom_a = {:?}\natom_b = {:?}\n", atom_a, atom_b);
        // map rest of src_atom to '...'
        if discriminant(atom_b) == discriminant(&Atom::Extension) {
            mappings.insert(atom_b.clone(), Either::Right(
                AtomExpr {
                    atoms: src_expr.atoms[i..].to_vec(),
                    operators: src_expr.operators[i+1..].to_vec()
                }
            ));
            return Some(generate_new_expression(src_expr, v_expr, &mappings))
        }
        if ! left_to_right_match(atom_a, atom_b) {
            return None;
        }
        mappings.insert(atom_b.clone(), Either::Left(atom_a.clone()));
        // go to next (operator, atom) pair
        i += 1;
    }
    Some(generate_new_expression(src_expr, v_expr, &mappings))
}

/// Is the atom_a mappable on atom_b ?
/// 
/// e.g. (A + B) mappable on A
/// but A is NOT mappable on (A + B)
fn left_to_right_match(atom_a: &Atom, atom_b: &Atom) -> bool{
    println!("Map {:?} to {:?} ?", atom_a, atom_b);
    match atom_b { // we match the second atom !!
        Atom::Parenthesized(par_b) => todo!(),
        Atom::Value(_) => match atom_a {
            Atom::Value(_) |
            Atom::Special(_) |
            Atom::Parenthesized(_) => true,
            _ => todo!()
            
        },
        Atom::Special(_) => atom_a == atom_b,
        Atom::Extension => todo!(),
        Atom::Generator(_) => todo!(),
    }
}

/// Generate new expression using source, value-expression and mappings
fn generate_new_expression(
    src_expr: &AtomExpr,
    v_expr: &AtomExpr,
    mappings: &HashMap<Atom, Either<Atom, AtomExpr>>
) -> AtomExpr {
    // init new atoms and operators
    let mut atoms: Vec<Atom> = Vec::new();
    let mut operators: Vec<Operator> = Vec::new();
    // match each atom of the v-expr
    let num_v_atoms = v_expr.atoms.len();
    let mut i = 0;
    println!("Mappings:\n{:#?}\n", mappings);
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
            match atom_v {
                Atom::Parenthesized(par_v) => {
                    todo!()
                },
                _ => atoms.push(
                    match mappings.get(atom_v) {
                        Some(Either::Left(atom)) => atom.clone(),
                        Some(Either::Right(_)) => panic!("Found extension instead of atom\n"),
                        None => panic!("Could not find mapping for atom {:?}\n", atom_v)
                    }
                )
            }
        }
        i += 1;
    }

    AtomExpr { atoms, operators }
}

#[test]
fn test() {
    let src_expr = match aps_parser::atom_expr_p::<ApsParserKind>(
        "(A + B) + C + D + E"
    ) {
        Ok(("", expr)) => expr,
        Ok((rest, parsed)) => panic!(
            "Failed to parse everything:\n'{}'\nParsed :\n{:#?}\n",
            rest,
            parsed
        ),
        Err(err) => panic!("Failed to parse expression:\n{:#?}", err)
    };
    let property = match aps_parser::root::<ApsParserKind>(
        "+ :: { A + B + ... = B + A + ... ;; }"
    ) {
        Ok(("", parsed)) => match parsed.first().unwrap() {
            AlgebraicObject::PropertyGroup(
                BraceGroup { properties, operator: _ }
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
