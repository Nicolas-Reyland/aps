use nom::{Err, IResult, Parser};

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
    let mut line_parser = nom::bytes::complete::take_until("\n");
    let mut parser = nom::multi::separated_list0(
        nom::bytes::complete::tag("\n"),
        line_parser
    );
}
