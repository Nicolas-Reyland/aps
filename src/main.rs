use std::str;

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_while},
    character::{complete::{alphanumeric1 as alphanumeric, char, one_of, satisfy}, is_alphabetic, is_digit},
    combinator::{cut, map, opt, value, recognize},
    error::{context, convert_error, ContextError, ErrorKind, ParseError, VerboseError},
    multi::{separated_list0, many0},
    number::complete::double,
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    Err, IResult, Parser,
};

#[derive(Debug, PartialEq)]
enum Atom<'a> {
    Parenthesized(AtomExpr<'a>),
    Value(char),
    Special(char),
}

#[derive(Debug, PartialEq)]
struct Operator {
    op: char,
}

#[derive(Debug, PartialEq)]
struct AtomExpr<'af> {
    // e.g. A + (B + C)
    atoms: Vec<Atom<'af>>,
    operators: Vec<Operator>,
}

#[derive(Debug, PartialEq)]
struct AlgebraicProperty<'ap> {
    atom_expr_left: AtomExpr<'ap>,
    atom_expr_right: AtomExpr<'ap>,
    // equality: Equality<'ap>
}

#[derive(Debug, PartialEq)]
struct AlgebraicFunction<'f> {
    atom_expr_left: AtomExpr<'f>,
    atom_expr_right: AtomExpr<'f>,
}

fn sp_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(input)
}

fn sp_preceded_p<'i, E: ParseError<&'i str>>(parser: impl Parser<&'i str, &'i str, E>) {
    preceded(sp_p, parser)
}

fn op_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Operator, E> {
    let chars = "+-*/.@^";
    map(
        one_of(chars),
        |atom_symbol| Operator {op: atom_symbol}
    )(input)
}

fn fn_name_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    recognize(
        tuple((
            satisfy(|c| c == '_' || c.is_lowercase()),
            many0(
                satisfy(|c| c == '_' || c.is_lowercase() || c.is_numeric())
            )
        ))
    )(input)
}

fn def_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    tag("::")(input)
}

fn fn_def_symbol_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    tag("->")(input)
}

fn atom_symbol_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Atom<'i>, E> {
    map(
        satisfy(|c| c.is_alphabetic() && c.is_uppercase()),
        |atom_symbol| Atom::Value(atom_symbol)
    )(input)
}

fn special_symbol_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Atom<'i>, E> {
    map(
        satisfy(is_digit),
        |atom_symbol| Atom::Value(atom_symbol)
    )(input)
}

fn equ_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    recognize(char('='))(input)
}

fn end_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    tag(";;")(input)
}


fn definition_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    context(
        "definition",
        terminated(
            alt((
                brace_def_p,
                fn_def_p,
                k_def_p,
            )),
            sp_p
        )
    ).parse(input)
}

fn brace_def_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    context(
        "brace def",
        tuple((
            alt((op_p, satisfy(|c| c == '_'))),
            sp_preceded_p(def_p),
            char('{'),
            property_list_p,
            char('}'),
        ))
    )(input)
}

fn fn_def_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    context(
        "fn def",
        tuple((
            fn_name_p,
            def_p,
            atom_expr_p,
            fn_def_symbol_p,
            atom_expr_p,
            end_p,
        ))
    )(input)
}

fn k_def_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    context(
        "K def",
        tuple((
            char('K'),
            def_p,
            alt(char('?')),
            alt(k_group_p),
        ))
    )(input)
}


fn atom_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Atom<'i>, E> {
    context(
        "atom",
        alt((
            map(
                atom_symbol_p,
                |symbol| Atom::Value(symbol)
            ),
            map(
                special_symbol_p,
                |symbol| Atom::Special(symbol)
            ),
            map(
                preceded(
                    char('('),
                    terminated(
                        atom_expr_p,
                        char(')')
                    )
                ),
                |atom_expr| Atom::Parenthesized(atom_expr)
            )
        ))
    )(input)
}
fn atom_expr_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, AtomExpr<'i>, E> {
    context(
        "atom expr",
        //
    )(input)
}


fn main() {
    let input: &'static str = "+ :: {\n    A + B = B + A\n}\n";


    
}
