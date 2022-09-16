/* Parser for Algebraic Proofing System Language */

use std::{env, fs};

use aps_parser::{AlgebraicProperty, AlgebraicFunction, KProperty, AlgebraicObject};
use explorer::{explore_graph, print_graph_dot_format};

#[path = "aps_parser/aps_parser.rs"] mod aps_parser;
#[path = "explorer/explorer.rs"] mod explorer;

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
    let src_expr = match aps_parser::atom_expr_p::<aps_parser::ApsParserKind>(
        // "A * (B + C) + (D + 0) * 1" => "A * B + A * C + D"
        &args[1] // => "D + ((A + B) + C)", "(C + (A + B)) + D", "((B + A) + C) + D", etc
        //                               ^^^^^                 ^^^^^^^^^^^^          ^^^^^^
    ) {
        Ok(("", expr)) => expr,
        Ok((rest, parsed)) => panic!(
            "Failed to parse whole expression:\n'{}'\nParsed (expr) :\n{:#?}\n",
            rest,
            parsed
        ),
        Err(err) => panic!("Failed to parse expression:\n{:#?}", err)
    };
    // create graph and stuff
    let (
        properties,
        functions,
        _k_properties,
    ) = split_algebraic_objects(alg_objects);
    let mut graph = explorer::init_graph(src_expr);
    // explore the graph a few times
    for _ in 0..5 {
        // print_graph_dot_format(&graph);
        explore_graph(&mut graph, properties.clone(), functions.clone());
    }
    print_graph_dot_format(&graph);




    /*
    let operations = match aps_parser::root::<aps_parser::ApsParserKind>(&input) {
        Ok(("", value)) => value,
        Ok((rest, parsed)) => panic!(
            "unfinished parse: '{}'\nParsed content:\n{:#?}\n",
            rest,
            parsed
        ),
        Err(err) => panic!("{:#?}", err)
    };
    print!(
        "parsing result:\n{:#?}\n",
        operations
    )
    */
}

fn split_algebraic_objects(
    alg_objects: Vec<AlgebraicObject>
) -> (Vec<AlgebraicProperty>, Vec<AlgebraicFunction>, Vec<KProperty>) {
    let mut properties: Vec<AlgebraicProperty> = Vec::new();
    let mut functions: Vec<AlgebraicFunction> = Vec::new();
    let mut k_properties: Vec<KProperty> = Vec::new();
    for obj in alg_objects {
        match obj {
            AlgebraicObject::KProperty(kp) => k_properties.push(kp),
            AlgebraicObject::PropertyGroup(bg) => properties.extend(bg.properties),
            AlgebraicObject::Function(f) => functions.push(f),
        }
    }
    (properties, functions, k_properties)
}

