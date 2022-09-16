/* Parser for Algebraic Proofing System Language */

use std::fs;

#[path = "aps_parser/aps_parser.rs"] mod aps_parser;
#[path = "explorer/explorer.rs"] mod explorer;

fn main() {
    let input_str = match fs::read_to_string("test.aps") {
        Ok(content) => content,
        Err(err) => panic!("{}", err)
    };
    let algebraic_objects = aps_parser::root::<aps_parser::ApsParserKind>(
        &input_str
    );
    let src_expr = match aps_parser::atom_expr_p::<aps_parser::ApsParserKind>(
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
    let mut graph = explorer::init_graph(src_expr);
    



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
