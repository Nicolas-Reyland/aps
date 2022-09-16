/* Parser for Algebraic Proofing System Language */

use std::{env, fs};

use aps_parser::{AlgebraicProperty, AlgebraicFunction, KProperty, AlgebraicObject, AtomExpr};
use explorer::{explore_graph, print_graph_dot_format};
use solution::solve_equality;

#[path = "aps_parser/aps_parser.rs"] mod aps_parser;
#[path = "explorer/explorer.rs"] mod explorer;
#[path = "explorer/solution.rs"] mod solution;

static MAX_GRAPH_EXPLORATION_DEPTH: u8 = 10;
static MAX_NODES_PER_GRAPH: usize = 10_000;

fn main() {
    let input_str = match fs::read_to_string("test.aps") {
        Ok(content) => content,
        Err(err) => panic!("{}", err)
    };
    // parse input rules
    let alg_objects = match aps_parser::root::<aps_parser::ApsParserKind>(
        &input_str
    ) {
        Ok(("", expr)) => expr,
        Ok((rest, parsed)) => panic!(
            "Failed to parse everything:\n'{}'\nParsed (root) :\n{:#?}\n",
            rest,
            parsed
        ),
        Err(err) => panic!("Failed to parse expression:\n{:#?}", err)
    };
    // parse input expression
    let args: Vec<String> = env::args().collect();
    let left_expr = str2atom_expr(&args[1]);
    let right_expr = str2atom_expr(&args[2]);
    // => "D + ((A + B) + C)", "(C + (A + B)) + D", "((B + A) + C) + D", etc
    //     ^^^^^                 ^^^^^^^^^^^^          ^^^^^^

    // create graph and stuff
    match solve_equality(
        alg_objects,
        &left_expr,
        &right_expr
    ) {
        Some(route) => {
            println!("Solution found for '{left_expr} = {right_expr}' :");
            for point in route {
                println!("-> {}", point)
            }
        },
        None => eprintln!("No stolution found for '{left_expr} = {right_expr}' :(")
    }
    /*
    let mut graph = explorer::init_graph(src_expr);
    // explore the graph a few times
    for _ in 0..String::from(&args[1]).parse::<usize>().unwrap() {
        // print_graph_dot_format(&graph);
        explore_graph(&mut graph, &properties, &functions);
    }
    print_graph_dot_format(&graph)
    */
}

fn str2atom_expr(input: &str) -> AtomExpr {
    match aps_parser::atom_expr_p::<aps_parser::ApsParserKind>(
        input
    ) {
        Ok(("", expr)) => expr,
        Ok((rest, parsed)) => panic!(
            "Failed to parse whole expression:\n'{}'\nParsed (expr) :\n{:#?}\n",
            rest,
            parsed
        ),
        Err(err) => panic!("Failed to parse expression:\n{:#?}", err)
    }
}
