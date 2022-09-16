// APS Lang Parser

use std::{boxed::Box, fmt};

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

#[derive(Debug, Clone, Hash, Eq)]
pub enum Atom {
    Parenthesized(AtomExpr),
    Value(char),
    Special(char),
    Extension,
    Generator(GeneratorExpr),
}

impl PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Parenthesized(expr_a),
                Self::Parenthesized(expr_b)
            ) => expr_a == expr_b,
            (
                Self::Value(val_a), Self::Value(val_b)
            ) => val_a == val_b,
            (
                Self::Special(spe_a), Self::Special(spe_b)
            ) => spe_a == spe_b,
            (
                Self::Generator(a),
                Self::Generator(b)
            ) => a == b,
            (
                Self::Extension,
                Self::Extension
            ) => true,
            _ => false,
        }
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::Parenthesized(x) => write!(f, "({})", x),
            Atom::Value(x) => write!(f, "{}", x),
            Atom::Special(x) => write!(f, "{}", x),
            Atom::Extension => write!(f, "..."),
            Atom::Generator(x) => write!(f, "{}", x),
        }
    }
}

pub fn parenthesized_atom(expr: AtomExpr) -> Atom {
    if expr.atoms.len() == 1 {
        return expr.atoms[0].clone();
    }
    Atom::Parenthesized(expr)
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Operator {
    op: char,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.op)
    }
}

#[derive(Debug, Clone, Hash, Eq)]
pub struct AtomExpr {
    pub atoms: Vec<Atom>,
    pub operators: Vec<Operator>,
}

impl PartialEq for AtomExpr {
    fn eq(&self, other: &Self) -> bool {
        let num_atoms = self.atoms.len();
        if num_atoms != other.atoms.len() {
            return false;
        }
        for i in 0..num_atoms-1 {
            if self.atoms[i] != other.atoms[i] ||
                self.operators[i] != other.operators[i]
            {
                return false;
            }
        }
        self.atoms[num_atoms - 1] == other.atoms[num_atoms - 1]
    }
}

impl fmt::Display for AtomExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let num_operators = self.operators.len();
        write!(f, "{}", self.atoms[0])?;
        for i in 0..num_operators {
            write!(f, " {} {}", self.operators[i], self.atoms[i + 1])?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum GeneratorElement {
    GenOperator(Operator),
    GenAtom(Atom),
}

impl fmt::Display for GeneratorElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GeneratorElement::GenOperator(x) => write!(f, "{}", x),
            GeneratorElement::GenAtom(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct GeneratorExpr {
    elements: Vec<GeneratorElement>,
    iterator: Box<Atom>,
}

impl fmt::Display for GeneratorExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "$ ")?;
        for element in &self.elements {
            write!(f, "{} ", element)?;
        }
        write!(f, "$ # {}", self.iterator)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BraceGroup {
    pub operator: Operator,
    pub properties: Vec<AlgebraicProperty>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AlgebraicProperty {
    pub atom_expr_left: AtomExpr,
    pub atom_expr_right: AtomExpr,
    // relation: Relation (=, <=>, =>, >, <, ...)
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug)]
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

/// op : [+-*/@^] sp
fn op_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Operator, E> {
    let chars = "+-*/@^";
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

/// atom_symbol : ([A-Z] | '...') sp
fn atom_symbol_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Atom, E> {
    sp_terminated!(
        alt((
            map(
                satisfy(|c| c.is_alphabetic() && c.is_uppercase()),
                Atom::Value
            ),
            value(Atom::Extension, tag("..."))
        ))
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
            parenthesized_atom
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
    // TODO: fix the case 'K :: ? ;;' (doesn't work)
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
                )),
                end_p,
            )),
            |(_, _, (c, group), _)| {
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
pub fn atom_expr_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(input: &'i str) -> IResult<&'i str, AtomExpr, E> {
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

