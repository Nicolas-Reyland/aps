// APS Lang Parser

use std::boxed::Box;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::{complete::{char, one_of, satisfy}},
    combinator::{map, opt, value, recognize},
    error::{context, ContextError, ErrorKind, ParseError},
    multi::{many0, many1},
    sequence::{preceded, terminated, tuple}, IResult,
};

#[path = "aps_parser_tests.rs"] pub(crate) mod aps_parser_tests;

pub type ApsParserKind<'i> = (&'i str, ErrorKind);

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Parenthesized(AtomExpr),
    Value(char),
    Special(char),
    Generator(GeneratorExpr),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Operator {
    op: char,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AtomExpr {
    atoms: Vec<Atom>,
    operators: Vec<Operator>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum GeneratorElement {
    GenOperator(Operator),
    GenAtom(Atom),
}

#[derive(Debug, PartialEq, Clone)]
pub struct GeneratorExpr {
    elements: Vec<GeneratorElement>,
    iterator: Box<Atom>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BraceGroup {
    operator: Operator,
    properties: Vec<AlgebraicProperty>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AlgebraicProperty {
    atom_expr_left: AtomExpr,
    atom_expr_right: AtomExpr,
    // relation: Relation (=, <=>, =>, >, <, ...)
}

#[derive(Debug, PartialEq)]
pub struct AlgebraicFunction<'af> {
    name: &'af str,
    atom_expr_left: AtomExpr,
    atom_expr_right: AtomExpr,
}

#[derive(Debug, PartialEq)]
pub struct KProperty {
    undefined_property: bool,
    base: char,
    dim: i8,
}

#[derive(Debug, PartialEq)]
pub enum AlgebraicObject<'ao> {
    KProperty(KProperty),
    PropertyGroup(BraceGroup),
    Function(AlgebraicFunction<'ao>),
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

/// sp : [ \t\r\n]*
fn sp_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(input)
}

/// op : [+-*/.@^] sp
fn op_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Operator, E> {
    let chars = "+-*/.@^";
    map(
        sp_terminated!(one_of(chars)),
        |op| Operator { op }
    )(input)
}

/// fn_name : [a-z_][a-z0-9_] sp
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

/// def : '::' sp
fn def_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    sp_terminated!(tag("::"))(input)
}

/// fn_def : '->' sp
fn fn_def_symbol_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    sp_terminated!(tag("->"))(input)
}

/// atom_symbol : [A-Z] sp
fn atom_symbol_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Atom, E> {
    map(
        sp_terminated!(
            satisfy(|c| c.is_alphabetic() && c.is_uppercase())
        ),
        Atom::Value
    )(input)
}

/// special_symbol : [0-9] sp
fn special_symbol_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Atom, E> {
    map(
        sp_terminated!(
            satisfy(|c: char| c.is_numeric())
        ),
        Atom::Special
    )(input)
}

/// parenthesized_atom : '(' atom_expr ')' sp
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

/// generator_expr : '$' sp (op | atom)+ '$' sp '#' sp (atom_symbol | special_symbol)
fn generator_expr_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, Atom, E> {
    context(
        "generator expr",
        map(
            tuple((
                preceded(
                    sp_terminated!(char('$')),
                    many1(
                        alt((
                            map(op_p, GeneratorElement::GenOperator),
                            map(atom_p, GeneratorElement::GenAtom),
                        ))
                    )
                ),
                preceded(
                    sp_terminated!(char('$')),
                    preceded(
                        sp_terminated!(char('#')),
                        alt((atom_symbol_p, special_symbol_p))
                    )
                )
            )),
            |(elements, iterator)| -> Atom {
                Atom::Generator(GeneratorExpr { elements, iterator: Box::new(iterator) })
            }
        )
    )(input)
}

/// equ : '=' sp
fn equ_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    sp_terminated!(recognize(char('=')))(input)
}

/// end : ';;' sp
fn end_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    sp_terminated!(tag(";;"))(input)
}


/// definition : brace_def | fn_def | k_def
fn definition_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, AlgebraicObject, E> {
    context(
        "definition",
        terminated(
            alt((
                map(
                    k_def_p,
                    AlgebraicObject::KProperty
                ),
                map(
                    brace_def_p,
                    AlgebraicObject::PropertyGroup
                ),
                map(
                    fn_def_p,
                    AlgebraicObject::Function
                ),
            )),
            sp_p
        )
    )(input)
}

/// brace_def : (op | _ sp) def '{' sp property_list '}' sp
pub fn brace_def_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, BraceGroup, E> {
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
            |(operator, _, _, properties, _)| BraceGroup { operator, properties }
        )
    )(input)
}

/// fn_def : fn_name def atom_expr fn_def atom_expr end
pub fn fn_def_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, AlgebraicFunction<'i>, E> {
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

/// k_def : 'K' sp def ('?'? k_group | '?')
pub fn k_def_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, KProperty, E> {
    context(
        "K def",
        map(
            tuple((
                sp_terminated!(char('K')),
                def_p,
                alt((
                    tuple((
                        opt(value(true, char('?'))),
                        map(k_group_p, Some),
                    )),
                    value((Some(true), Some(('K', 1))), char('?'))
                ))
            )),
            |(_, _, (c, group))| {
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


/// atom : atom_symbol | special_symbol | parenthesized_atom | gen_expr
fn atom_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, Atom, E> {
    context(
        "atom",
        alt((
            atom_symbol_p,
            special_symbol_p,
            parenthesized_atom_p,
            generator_expr_p,
        ))
    )(input)
}

pub fn atom_expr_map_f<'ae>((first, rest): (Atom, Vec<(Operator, Atom)>)) -> AtomExpr {
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


/// atom_expr : atom (op atom)*
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

/// property_list : (property sp)*
fn property_list_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, Vec<AlgebraicProperty>, E> {
    context(
        "property list",
        many0(
            sp_terminated!(property_p)
        )
    )(input)
}

/// property : atom_expr equ atom_expr end
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


/// k_group : [A-Z] [0-9]? sp
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

/// root : (sp definition)*
pub fn root<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, Vec<AlgebraicObject<'i>>, E> {
    context(
        "all",
        many0(
            sp_preceded!(definition_p)
        )
    )(input)
}
