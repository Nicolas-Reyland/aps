use nom::{
    Err,
    IResult,
    Parser,
    bytes::complete::{tag}
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
    // atom_symbol_parser
    // SPECIAL_SYMBOL := [0-9]
    // let mut special_synmbol = nom::character::is_digit
    // EQU
    let equ_parser = tag("=");
    
    // definition
    let definition_parser = 




}
