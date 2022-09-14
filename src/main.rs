use nom::{
    Err,
    IResult,
    Parser,
    bytes::complete::{tag},
    combinator::opt,
    sequence::tuple,
};

fn parse_hello(input: &str) -> IResult<&str, &str, ()> {
    match input.strip_prefix("Hello") {
        Some(tail) => Ok((tail, "Hello")),
        None => Err(nom::Err::Error(())),
    }
}

fn parse_tag<'i: 't, 't>(tag: &'t str) -> impl Parser<&'i str, &'i str, ()> + 't {
    move |input: &'i str| {
        match input.strip_prefix(tag) {
            Some(tail) => Ok((tail, &input[..tag.len()])),
            None => Err(nom::Err::Error(())),
        }
    }
}

fn parse_comma_tags<'i: 't, 't>(
    tag1: &'t str,
    tag2: &'t str
) -> impl Parser<&'i str, (&'i str, &'i str), ()> + 't {
    let mut parse_tag1 = parse_tag(tag1);
    let mut parse_separator = parse_tag(", ");
    let mut parse_tag2 = parse_tag(tag2);

    move |input: &'i str| {
        let (tail, value1) = parse_tag1.parse(input)?;
        let (tail, _) = parse_separator.parse(tail)?;
        let (tail, value2) = parse_tag2.parse(tail)?;
        Ok((tail, (value1, value2)))
    }
}

fn parse_separated<'i>(
    mut parse_tag1: impl Parser<&'i str, &'i str, ()>,
    mut parse_separator: impl Parser<&'i str, &'i str, ()>,
    mut parse_tag2: impl Parser<&'i str, &'i str, ()>,
) -> impl Parser<&'i str, (&'i str, &'i str), ()> {
    move |input: &'i str| {
        let (tail, value1) = parse_tag1.parse(input)?;
        let (tail, _) = parse_separator.parse(tail)?;
        let (tail, value2) = parse_tag2.parse(tail)?;
        Ok((tail, (value1, value2)))
    }
}

fn atom_symbol_parser<'i : 't, 't>(char: &'t str) -> impl Parser<&'i str, &'i str, ()> {
    let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    move |input: &'i str| {
        if chars.contains(input.chars().next().unwrap()) {
            Ok((&input[1..], &input[..1]))
        } else {
            Err(nom::Err::Error(()))
        }
    }
}

#[test]
fn test() {
    /*assert_eq!(
        parse_tag("Hello").parse("Hello, World!").unwrap(),
        (", World!", "Hello")
    );*/
    /*assert_eq!(
        parse_comma_tags("Hello", "World")
            .parse("Hello, World!")
            .unwrap(),
        ("!", ("Hello", "World"))
    )*/
    let mut parse_hello_world = parse_separated(
        parse_tag("Hello"),
        parse_tag(", "),
        parse_tag("World")
    );
    assert_eq!(parse_hello_world.parse("Hello, World!").unwrap(), ("!", ("Hello", "World")))
}

fn main() {
    let input: &'static str = "+ :: {\n    A + B = B + A\n}\n";
    // NL := '\n'
    let mut nl_parser = tag("\n");
    // OP := [+-*/.@^]
    let mut op_parser = nom::branch::alt(
        (tag("+"), tag("-"), tag("*"), tag("/"), tag("."), tag("@"), tag("^"))
    );
    // FN_NAME := [a-z_][a-z_0-9]*

    // DEF := '::'
    let mut def_parser = tag("::");
    // FN_DEF_SYMBOL := '->'
    let fn_def_symbol_parser = tag("->");
    // ATOM_SYMBOL := [A-Z]
    let atom_symbol_parser;
    // SPECIAL_SYMBOL := [0-9]
    let special_synmbol; // = nom::character::is_digit
    // EQU
    let equ_parser = tag("=");

    // definition
    let brace_def_parser;
    let fn_def_parser;
    let k_def_parser;
    let definition_parser = tuple(
        (nom::branch::alt((brace_def_parser, fn_def_parser, k_def_parser)), nl_parser)
    );
    // brace definitions
    let property_list_parser;
    brace_def_parser = tuple(
        (
            nom::branch::alt((op_parser, tag("_"))),
            def_parser,
            tag("{"),
            property_list_parser,
            tag("}"),
        )
    );
    // function definition
    let fn_name_parser;
    let atom_form_parser;
    fn_def_parser = tuple(
        (fn_name_parser, def_parser, atom_form_parser, fn_def_symbol_parser, atom_form_parser)
    );
    // K definition
    let k_group_parser;
    k_def_parser = tuple(
        (tag("K"), def_parser, opt(tag("?")), opt(k_group_parser))
    );

    // atom
    let atom_parser = nom::branch::alt(
        (
            atom_symbol_parser,
            special_synmbol,
            tuple(
                (tag("("), atom_form_parser, tag(")"))
            )
        )
    );
    // atom form
    atom_form_parser = tuple((
        atom_parser,
        nom::multi::many0(
            tuple((
                op_parser, atom_parser
            ))
        )
    ));

    // property list
    let property_parser;
    property_list_parser = nom::multi::many0(
        tuple((
            property_parser,
            nl_parser
        ))
    );
    // property
    let gen_form_parser;
    property_parser = tuple((
        atom_form_parser,
        equ_parser,
        nom::multi::many1((
            nom::branch::alt((
                atom_form_parser, gen_form_parser
            ))
        ))
    ));
    // gen form
    gen_form_parser = tuple((
        tag("$"),
        nom::multi::many0(
            nom::branch::alt((
                op_parser, atom_parser
            ))
        ),
        tag("$"),
        opt(
            tuple((
                tag("#"), atom_symbol_parser
            ))
        )
    ));
}
