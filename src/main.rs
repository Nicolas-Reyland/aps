use std::str;

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_while},
    character::{complete::{alphanumeric1 as alphanumeric, char, one_of, satisfy}, is_alphabetic, is_digit},
    combinator::{cut, map, opt, value, recognize},
    error::{context, convert_error, ContextError, ErrorKind, ParseError, VerboseError},
    multi::{separated_list0, many0, separated_list1},
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
    name: &'f str,
    atom_expr_left: AtomExpr<'f>,
    atom_expr_right: AtomExpr<'f>,
}

#[derive(Debug, PartialEq)]
struct KProperty {
    undefined_property: bool,
    base: char,
}


fn sp_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(input)
}

fn sp_preceded_p<'i, E: ParseError<&'i str>>(parser: impl Parser<&'i str, &'i str, E>) {
    preceded(sp_p, parser)
}

fn sp_terminated_p<'i, E: ParseError<&'i str>>(parser: impl Parser<&'i str, &'i str, E>) {
    terminated(parser, sp_p)
}

fn op_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Operator, E> {
    let chars = "+-*/.@^";
    map(
        sp_terminated_p(one_of(chars)),
        |atom_symbol| Operator {op: atom_symbol}
    )(input)
}

fn fn_name_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    recognize(
        sp_terminated_p(
            tuple((
                satisfy(|c| c == '_' || c.is_lowercase()),
                many0(
                    satisfy(|c| c == '_' || c.is_lowercase() || c.is_numeric())
                )
            ))
        )
    )(input)
}

fn def_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    sp_terminated_p(tag("::"))(input)
}

fn fn_def_symbol_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    sp_terminated_p(tag("->"))(input)
}

fn atom_symbol_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Atom<'i>, E> {
    map(
        sp_terminated_p(
            satisfy(|c| c.is_alphabetic() && c.is_uppercase())
        ),
        |atom_symbol| Atom::Value(atom_symbol)
    )(input)
}

fn special_symbol_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Atom<'i>, E> {
    map(
        sp_terminated_p(
            satisfy(is_digit)
        ),
        |atom_symbol| Atom::Value(atom_symbol)
    )(input)
}

fn equ_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    sp_terminated_p(recognize(char('=')))(input)
}

fn end_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    sp_terminated_p(tag(";;"))(input)
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

fn brace_def_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Vec<AlgebraicProperty>, E> {
    context(
        "brace def",
        map(
            tuple((
                alt((
                    op_p,
                    map(
                        satisfy(|c| c == '_'),
                        |op| Operator { op }
                    )
                )),
                sp_preceded_p(def_p),
                char('{'),
                property_list_p,
                char('}'),
            )),
            |_op, _, _, properties, _| properties
        )
    )(input)
}

fn fn_def_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, AlgebraicFunction, E> {
    context(
        "fn def",
        map(
            tuple((
                fn_name_p,
                def_p,
                atom_expr_p,
                fn_def_symbol_p,
                atom_expr_p,
                end_p,
            )),
            |name, _, atom_expr_left, _, atom_expr_right, _| AlgebraicFunction { name, atom_expr_left, atom_expr_right }
        )
    )(input)
}

fn k_def_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, KProperty, E> {
    context(
        "K def",
        map(
            tuple((
                char('K'),
                def_p,
                alt(char('?')),
                alt(k_group_p),
            )),
            |(), (), c, group| ()
        )
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

fn atom_expr_map_f<'ae>((first, rest): (Atom, Vec<(Operator, Atom)>)) -> AtomExpr<'ae> {
    let num_operators = rest.len();
    let num_atoms = num_operators + 1;
    let atoms: Vec<Atom> = Vec::with_capacity(num_atoms);
    let operators: Vec<Operator> = Vec::with_capacity(num_operators);
    // fill the atoms & operator vectors
    atoms.push(first);
    for (atom, op) in rest {
        atoms.push(atom);
        operators.push(op);
    }
    // return atom expr
    AtomExpr { atoms, operators }
}

fn atom_expr_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, AtomExpr<'i>, E> {
    context(
        "atom expr",
        map(
            tuple((
                atom_p,
                many0(tuple((
                    op_p,
                    atom_p,
                )))
            )),
            atom_expr_map_f
        )
    )(input)
}

fn property_list_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Vec<AlgebraicProperty>, E> {
    context(
        "property list",
        many0(
            terminated(property_p, sp_p)
        )
    )(input)
}

fn property_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, AlgebraicProperty<'i>, E> {
    context(
        "property",
        map(
            tuple((
                atom_expr_p,
                equ_p,
                atom_expr_p,
                end_p,
            )),
            |atom_expr_left, _, atom_expr_right, _| AlgebraicProperty { atom_expr_left, atom_expr_right  }
        )
    )
}


fn k_group_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, (char, i8), E> {
    context(
        "k group",
        map(
            tuple((
                satisfy(char::is_uppercase),
                opt(nom::number::complete::be_i8)
            )),
            |e: char, dim: Option<i8>| (e, match dim {
                Some(value) => value,
                None => 1
            })
        )
    )(input)
}



fn main() {
    let input: &'static str = "+ :: {\n    A + B = B + A\n}\n";


    
}
