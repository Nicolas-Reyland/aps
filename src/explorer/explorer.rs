// Graph Explorer

use std::{collections::HashMap, fmt};

use crate::aps_parser::{self, AtomExpr, AlgebraicProperty, AlgebraicFunction, Atom, Operator, parenthesized_atom};

#[derive(Debug, PartialEq, Hash)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L, R> fmt::Display for Either<L, R>
where
    L: fmt::Display,
    R: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Either::Left(x) => x.fmt(f),
            Either::Right(x) => x.fmt(f),
        }
    }
}

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
    pub atom_expr: aps_parser::AtomExpr,
    pub neighbours: Vec<ExprNodeIndex>,
    pub transforms: Vec<AlgebraicProperty>,
    depth: i8,
    pub index: ExprNodeIndex,
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
            "ExprNode {{ expr: '{}', depth: {}, index: {}, neighbours: [",
            self.atom_expr,
            self.depth,
            self.index
        )?;
        for neighbour in &self.neighbours {
            write!(f, "{} ", neighbour)?;
        }
        write!(f, "], transforms: [")?;
        for tr in &self.transforms {
            write!(f, "{} ", tr)?;
        }
        write!(f, "] }}")
    }
}

type PropertyMapping = HashMap<Atom, Either<Atom, AtomExpr>>;

/// init a expression-graph
pub fn init_graph(expr: AtomExpr) -> ExprGraph {
    let base_node = ExprNode {
        atom_expr: expr,
        neighbours: Vec::new(),
        transforms: Vec::new(),
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
    let mut new_nodes: Vec<(ExprNode, ExprNode)> = Vec::new();
    for i in 0..graph.nodes.len() {
        let node = graph.nodes[i].clone();
        for property in properties.clone() {
            let new_expressions = apply_property(&node.atom_expr, &property, functions);
            new_nodes.extend(
                new_expressions.iter().map(|new_expr| (
                    node.clone(),
                    ExprNode {
                        atom_expr: new_expr.clone(),
                        neighbours: vec![],
                        transforms: vec![property.clone()],
                        depth: 0,
                        index: 0,
                    }
                ))
            );
        }
    }
    let mut at_least_one_new_node = false;
    let mut new_node_index = graph.nodes.len();
    for (mut src_node, mut new_node) in new_nodes {
        if ! graph.nodes.contains(&new_node) {
            at_least_one_new_node = true;
            // add source node to the new node's neighbours
            new_node.neighbours.push(src_node.index);
            // add the new node to the source node's neighbours
            src_node.neighbours.push(new_node.index);
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
        if node.index == 0 {"red"} else {"blue"}
    ));
    // print label of node
    content.push_str(&format!("{}", node.atom_expr));
    // end printing definition line
    content.push_str(&format!("\"];\n"));

    // start printing neighbours
    for (nb_index, neighbour) in node.neighbours.iter().enumerate() {
        content.push_str(&format!(
            "\t{} -> {} [label=< <B> {} </B> > fontsize=7 fontcolor=darkgreen];\n",
            neighbour,
            node.index,
            node.transforms[nb_index as usize]
        ));
    }
    content.push_str("\n");
    content
}

fn apply_property(src_expr: &AtomExpr, property: &AlgebraicProperty, functions: &Vec<AlgebraicFunction>) -> Vec<AtomExpr> {
    let mut new_expressions = apply_machted_substitution(
        src_expr,
        &property.atom_expr_left,
        &property.atom_expr_right,
        functions
    );
    new_expressions.extend(
        apply_machted_substitution(
            src_expr,
            &property.atom_expr_right,
            &property.atom_expr_left,
            functions
        )
    );
    return new_expressions;
}

/// try to match the src_expr to the left_expr, then generate a new expression based on right_expr
fn apply_machted_substitution(src: &AtomExpr, left: &AtomExpr, right: &AtomExpr, functions: &Vec<AlgebraicFunction>) -> Vec<AtomExpr>
{
    // call 'atom_expressions_match' on every expression (src_expr and sub-expression in parentheses)
    let mut new_expressions: Vec<AtomExpr> = Vec::new();
    // base call on src_expr
    match atom_expressions_match(src, left, functions) {
        Some(mappings) => {
            let new_expr = generate_new_expression(right, &mappings);
            new_expressions.push(new_expr);
        },
        None => (),
    }
    // recursively call 'atom_expressions_match' on the sub-expressions
    for (sub_i, sub) in src.atoms.iter().enumerate().filter_map(|(atom_i, atom)| match atom {
        Atom::Parenthesized(par) => Some((atom_i, par)),
        _ => None,
    }) {
        new_expressions.extend(
            apply_machted_substitution(sub, left, right, functions)
                .iter()
                .map(
                    |sub_tr| {
                        // reconstruct like that : <everything b4> (new sub) <everything after>
                        // only the sub changes, compared to the src. no other atoms
                        let mut src_atoms_prefix = src.atoms[..sub_i].to_vec().clone();
                        src_atoms_prefix.push(parenthesized_atom(sub_tr.clone()));
                        let src_atoms_suffix = src.atoms[sub_i+1..].to_vec().clone();
                        src_atoms_prefix.extend(src_atoms_suffix);
                        AtomExpr {
                            atoms: src_atoms_prefix,
                            operators: src.operators.clone(),
                        }
                    }
                )
                .collect::<Vec<AtomExpr>>()
        );
    }
    return new_expressions;
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
                    operators: src_expr.operators[i..].to_vec()
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
    if i != num_src_atoms || i != num_p_atoms {
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
            Atom::Parenthesized(_) |
            Atom::FunctionCall(_) => (true, None),
            _ => todo!()
            
        },
        Atom::Special(_) => (atom_a == atom_b, None),
        Atom::Extension => todo!(),
        Atom::FunctionCall(_) => todo!(),
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
            let sub_expr: AtomExpr = match mappings.get(&Atom::Extension) {
                Some(Either::Left(atom)) => panic!("Extension mapping is atom, not atom-expr: {}\n", atom),
                Some(Either::Right(expr)) => expr.clone(),
                None => panic!(
                    "Could not find mapping for extension:\nMapping :\n{:#?}\nExpr-V :\n{}\nAtom-V :\n{}\n",
                    &mappings,
                    &v_expr,
                    &atom_v,
                )
            };
            atoms.extend(sub_expr.atoms);
            operators.extend(sub_expr.operators);
        } else {
            // normal mapping
            atoms.push(
                match atom_v {
                    Atom::Parenthesized(par_v) => {
                        aps_parser::parenthesized_atom(generate_new_expression(par_v, mappings))
                    },
                    Atom::Special(_) => atom_v.clone(),
                    _ => match mappings.get(atom_v) {
                        Some(Either::Left(atom)) => atom.clone(),
                        Some(Either::Right(_)) => panic!("Found extension instead of atom\n"),
                        None => panic!("Could not find mapping for atom {:?} in expr '{}'\nMappings :\n{:#?}\n", atom_v, v_expr, mappings)
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
    let new_expressions = apply_property(&src_expr, &property, &vec![]);
    println!("New Expressions :\n{:#?}\n", new_expressions);
}
