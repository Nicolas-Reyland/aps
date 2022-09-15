/* Parser for Algebraic Structure Proofs Lang */

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::{complete::{char, one_of, satisfy}},
    combinator::{map, opt, value, recognize},
    error::{context, ContextError, ErrorKind, ParseError},
    multi::{many0, fold_many0},
    sequence::{preceded, terminated, tuple}, IResult,
};

mod parser_tests;

#[derive(Debug, PartialEq, Clone)]
enum Atom {
    Parenthesized(AtomExpr),
    Value(char),
    Special(char),
}

#[derive(Debug, PartialEq, Clone)]
struct Operator {
    op: char,
}

#[derive(Debug, PartialEq, Clone)]
struct AtomExpr {
    // e.g. A + (B + C)
    atoms: Vec<Atom>,
    operators: Vec<Operator>,
}

#[derive(Debug, PartialEq, Clone)]
struct AlgebraicProperty {
    atom_expr_left: AtomExpr,
    atom_expr_right: AtomExpr,
    // equality: Equality<'ap>
}

#[derive(Debug, PartialEq)]
struct AlgebraicFunction<'af> {
    name: &'af str,
    atom_expr_left: AtomExpr,
    atom_expr_right: AtomExpr,
}

#[derive(Debug, PartialEq)]
struct KProperty {
    undefined_property: bool,
    base: char,
    dim: i8,
}

#[derive(Debug, PartialEq)]
enum AlgebraicObject<'ao> {
    KProperty(KProperty),
    Function(AlgebraicFunction<'ao>),
    Property(AlgebraicProperty),
}

macro_rules! sp_preceded {
    ($parser:expr) => {
        preceded(sp_p, $parser)
    };
}

macro_rules! sp_terminated {
    ($parser:expr) => {
        terminated($parser, sp_p)
    };
}

fn sp_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(input)
}

fn op_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Operator, E> {
    let chars = "+-*/.@^";
    map(
        sp_terminated!(one_of(chars)),
        |op| Operator { op }
    )(input)
}

fn fn_name_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    recognize(
        sp_terminated!(
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
    sp_terminated!(tag("::"))(input)
}

fn fn_def_symbol_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    sp_terminated!(tag("->"))(input)
}

fn atom_symbol_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Atom, E> {
    map(
        sp_terminated!(
            satisfy(|c| c.is_alphabetic() && c.is_uppercase())
        ),
        Atom::Value
    )(input)
}

fn special_symbol_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Atom, E> {
    map(
        sp_terminated!(
            satisfy(|c: char| c.is_numeric())
        ),
        Atom::Special
    )(input)
}

fn parenthesized_atom_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, Atom, E> {
    context(
        "parenthesized atom",
        map(
            sp_terminated!(
                preceded(
                    char('('),
                    terminated(
                        atom_expr_p,
                        char(')')
                    )
                )
            ),
            Atom::Parenthesized
        )
    )(input)
}

fn equ_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    sp_terminated!(recognize(char('=')))(input)
}

fn end_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    sp_terminated!(tag(";;"))(input)
}


fn definition_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, Vec<AlgebraicObject>, E> {
    context(
        "definition",
        terminated(
            alt((
                map(
                    brace_def_p,
                    |properties| properties
                        .iter()
                        .map(|property| AlgebraicObject::Property(property.clone()))
                        .collect()
                ),
                map(
                    fn_def_p,
                    |fn_def| vec![AlgebraicObject::Function(fn_def)]
                ),
                map(
                    k_def_p,
                    |k_def| vec![AlgebraicObject::KProperty(k_def)]
                ),
            )),
            sp_p
        )
    )(input)
}

fn brace_def_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, Vec<AlgebraicProperty>, E> {
    context(
        "brace def",
        map(
            tuple((
                alt((
                    op_p,
                    sp_terminated!(
                        map(
                            satisfy(|c| c == '_'),
                            |op| Operator { op }
                        )
                    )
                )),
                def_p,
                sp_terminated!(char('{')),
                property_list_p,
                sp_terminated!(char('}')),
            )),
            |(_op, _, _, properties, _)| properties
        )
    )(input)
}

// TESTED
fn fn_def_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, AlgebraicFunction<'i>, E> {
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
            |(name, _, atom_expr_left, _, atom_expr_right, _)| AlgebraicFunction { name, atom_expr_left, atom_expr_right }
        )
    )(input)
}

fn k_def_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, KProperty, E> {
    context(
        "K def",
        map(
            tuple((
                sp_terminated!(char('K')),
                def_p,
                opt(value(true, char('?'))),
                opt(k_group_p),
            )),
            |(_, _, c, group)| {
                let undefined_property = match c {
                    Some(v) => v, // should be true
                    None => false
                };
                match group {
                    Some((base, dim)) => KProperty {
                        undefined_property,
                        base,
                        dim,
                    },
                    None => KProperty {
                        undefined_property,
                        base: 'K',
                        dim: 1,
                    }
                }
            }
        )
    )(input)
}


fn atom_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, Atom, E> {
    context(
        "atom",
        alt((
            atom_symbol_p,
            special_symbol_p,
            parenthesized_atom_p,
        ))
    )(input)
}

fn atom_expr_map_f<'ae>((first, rest): (Atom, Vec<(Operator, Atom)>)) -> AtomExpr {
    let num_operators = rest.len();
    let num_atoms = num_operators + 1;
    let mut atoms: Vec<Atom> = Vec::with_capacity(num_atoms);
    let mut operators: Vec<Operator> = Vec::with_capacity(num_operators);
    // fill the atoms & operator vectors
    atoms.push(first);
    for (op, atom) in rest {
        atoms.push(atom);
        operators.push(op);
    }
    // return atom expr
    AtomExpr { atoms, operators }
}


fn atom_expr_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, AtomExpr, E> {
    context(
        "atom expr",
        map(
            tuple((
                atom_p,
                many0(
                    tuple((
                        op_p,
                        atom_p,
                    ))
                )
            )),
            atom_expr_map_f
        )
    )(input)
}

fn property_list_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, Vec<AlgebraicProperty>, E> {
    context(
        "property list",
        many0(
            sp_terminated!(property_p)
        )
    )(input)
}

fn property_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, AlgebraicProperty, E> {
    context(
        "property",
        map(
            tuple((
                atom_expr_p,
                equ_p,
                atom_expr_p,
                end_p,
            )),
            |(atom_expr_left, _, atom_expr_right, _)| AlgebraicProperty {
                atom_expr_left,
                atom_expr_right
            }
        )
    )(input)
}


fn k_group_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, (char, i8), E> {
    context(
        "k group",
        map(
            tuple((
                satisfy(char::is_uppercase),
                sp_terminated!(
                    opt(
                        satisfy(|c| c.is_digit(10))
                    )
                ),
            )),
            |(e, dim)| (e, match dim {
                Some(value) => value as i8 - '0' as i8,
                None => 1
            })
        )
    )(input)
}

fn root<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, Vec<AlgebraicObject<'i>>, E> {
    context(
        "all",
        fold_many0(
            sp_preceded!(definition_p),
            Vec::new,
            |mut acc: Vec<AlgebraicObject>, mut item| {
                acc.append(&mut item);
                acc
            }
        )
    )(input)
}


fn main() {
    let input: &'static str = "+ :: {\n    A + B = B + A ;; \n}-::{A=A;;}\n";
    print!(
        "parsing result:\n{:#?}\n",
        root::<(&str, ErrorKind)>(input)
    )
}
