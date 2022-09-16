/* Parser for Algebraic Proofing System Language */

#[path = "aps_parser/aps_parser.rs"] mod aps_parser;

fn main() {
    let input: &'static str = "";
    print!(
        "parsing result:\n{:#?}\n",
        aps_parser::root::<aps_parser::ApsParserKind>(input)
    )
}
