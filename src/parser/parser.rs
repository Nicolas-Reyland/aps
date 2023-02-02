// APS Lang Parser

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;

use crate::parser::OperatorAssociativity::{
    LeftAssociative, LeftRightAssociative, NonAssociative, RightAssociative, Unknown,
};
use nom::multi::separated_list1;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{char as char_p, one_of, satisfy},
    combinator::{map, opt, recognize, value},
    error::{context, ContextError, ErrorKind, ParseError},
    multi::{many0, many1},
    sequence::{preceded, terminated, tuple},
    IResult,
};

pub type ApsParserKind<'i> = (&'i str, ErrorKind);
pub type AssociativityHashMap = HashMap<char, OperatorAssociativity>;

/* Types */
#[derive(Debug, Clone, Hash, Eq)]
pub enum Atom {
    Parenthesized(AtomExpr),
    Symbol(String),
    Special(i32),
    FunctionCall((String, Vec<AtomExpr>)),
    Sequential(SequentialExpr),
}

impl PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Parenthesized(expr_a), Self::Parenthesized(expr_b)) => expr_a == expr_b,
            (Self::Symbol(val_a), Self::Symbol(val_b)) => val_a == val_b,
            (Self::Special(spe_a), Self::Special(spe_b)) => spe_a == spe_b,
            (Self::FunctionCall(fn_call_a), Self::FunctionCall(fn_call_b)) => {
                fn_call_a == fn_call_b
            }
            (Self::Sequential(seq_a), Self::Sequential(seq_b)) => seq_a == seq_b,
            _ => false,
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::Parenthesized(x) => write!(f, "({})", x),
            Atom::Symbol(x) => write!(f, "{}", x),
            Atom::Special(x) => write!(f, "{}", x),
            Atom::FunctionCall((name, args)) => {
                write!(f, "{}({}", name, args.first().unwrap())?;
                for arg in args.iter().skip(1) {
                    write!(f, ", {}", arg)?;
                }
                write!(f, ")")
            }
            Atom::Sequential(x) => write!(f, "{}", x),
        }
    }
}

pub fn parenthesized_atom(expr: AtomExpr) -> Atom {
    if expr.atoms.len() == 1 {
        return expr.atoms.first().unwrap().clone();
    }
    Atom::Parenthesized(expr)
}

#[derive(Debug, Clone, Hash, Eq)]
pub struct Operator {
    pub op: char,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.op)
    }
}

impl PartialEq for Operator {
    fn eq(&self, other: &Self) -> bool {
        self.op == other.op
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum OperatorAssociativity {
    Unknown,
    LeftAssociative,
    RightAssociative,
    LeftRightAssociative,
    NonAssociative,
}

impl fmt::Display for OperatorAssociativity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Unknown => "default",
                LeftAssociative => "left-associative",
                RightAssociative => "right-associative",
                LeftRightAssociative => "left-right-associative",
                NonAssociative => "non-associative",
            }
        )
    }
}

#[derive(Debug, Clone, Hash, Eq)]
pub struct AtomExpr {
    pub atoms: Vec<Atom>,
    // There is only one operator : A + B * C is actually A + (B * C)
    // since operator precedence is not inherently known
    pub operator: Option<Operator>,
}

impl PartialEq for AtomExpr {
    fn eq(&self, other: &Self) -> bool {
        let num_atoms = self.atoms.len();
        if num_atoms != other.atoms.len() || self.operator != other.operator {
            return false;
        }
        for i in 0..num_atoms - 1 {
            if self.atoms[i] != other.atoms[i] {
                return false;
            }
        }
        self.atoms[num_atoms - 1] == other.atoms[num_atoms - 1]
    }
}

impl fmt::Display for AtomExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.atoms[0])?;
        let operator_char = match &self.operator {
            Some(operator) => operator.op,
            None => '?',
        };
        for atom in self.atoms.iter().skip(1) {
            write!(f, " {} {}", operator_char, atom)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SequentialExpr {
    pub operator: Operator,
    pub enumerator: AtomExpr,
    pub body: AtomExpr,
}

impl fmt::Display for SequentialExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "# {} : {} : {} #",
            self.operator, self.enumerator, self.body
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BraceGroup {
    pub operator: Operator,
    pub associativity: Option<OperatorAssociativity>,
    pub properties: Vec<AlgebraicProperty>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AlgebraicProperty {
    pub atom_expr_left: AtomExpr,
    pub atom_expr_right: AtomExpr,
    // relation: Relation (=, <=>, =>, >, <, ...)
}

impl fmt::Display for AlgebraicProperty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.atom_expr_left, self.atom_expr_right)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct AlgebraicFunction {
    pub name: String,
    pub atom_expr_args: Vec<AtomExpr>,
    pub atom_expr_right: AtomExpr,
}

impl fmt::Display for AlgebraicFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} :: {}",
            self.name,
            self.atom_expr_args.first().unwrap()
        )?;
        for left_expr in self.atom_expr_args.iter().skip(1) {
            write!(f, ", {}", left_expr)?;
        }
        write!(f, " -> {}", self.atom_expr_right)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct KProperty {
    pub undefined_property: bool,
    pub base: char,
    pub dim: i8,
}

impl fmt::Display for KProperty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "K :: {}{}{}",
            if self.undefined_property { "?" } else { "" },
            self.base,
            self.dim
        )
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum AlgebraicObject {
    KProperty(KProperty),
    PropertyGroup(BraceGroup),
    Function(AlgebraicFunction),
}

/* Parsers */
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

/// op : [+-*/@^$%] sp
fn op_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Operator, E> {
    map(sp_terminated!(one_of("+-*/@^$%")), |op| Operator { op })(input)
}

/// fn_name : [a-z_][a-z0-9_] sp
fn fn_name_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    sp_terminated!(recognize(tuple((
        satisfy(|c| c == '_' || c.is_lowercase()),
        many0(satisfy(|c| c == '_' || c.is_lowercase() || c.is_numeric()))
    ))))(input)
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
    sp_terminated!(map(
        satisfy(|c| c.is_alphabetic() && c.is_uppercase()),
        |c| Atom::Symbol(String::from(c))
    ))(input)
}

/// special_symbol : [0-9]+ sp
fn special_symbol_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Atom, E> {
    map(
        sp_terminated!(many1(map(satisfy(|c: char| c.is_numeric()), |c: char| c))),
        |s| Atom::Special(s.into_iter().collect::<String>().parse().unwrap()),
    )(input)
}

/// parenthesized_atom : '(' atom_expr ')' sp
fn parenthesized_atom_expr_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, Atom, E> {
    context(
        "parenthesized atom",
        map(
            sp_terminated!(preceded(char_p('('), terminated(atom_expr_p, char_p(')')))),
            parenthesized_atom,
        ),
    )(input)
}

/// fn_call_p : fn_name '(' sp atom_expr (sp ',' sp atom_expr)* sp ')' sp
pub fn fn_call_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, Atom, E> {
    context(
        "function call atom",
        map(
            sp_terminated!(tuple((
                fn_name_p,
                preceded(
                    sp_terminated!(char_p('(')),
                    terminated(
                        separated_list1(
                            // separator
                            sp_preceded!(sp_terminated!(char_p(','))),
                            atom_expr_p
                        ),
                        sp_preceded!(char_p(')'))
                    )
                )
            ))),
            |(fn_name, args)| Atom::FunctionCall((fn_name.to_string(), args)),
        ),
    )(input)
}

/// sequential_expr : '#' sp op sp ':' sp atom_expr sp ':' sp atom_expr sp '#' sp
pub fn sequential_expr_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, Atom, E> {
    context(
        "sequential expr",
        // ... sp
        map(
            sp_terminated!(tuple((
                // '#' sp op
                preceded(sp_terminated!(char_p('#')), op_p),
                // sp ':' sp atom
                preceded(sp_terminated!(sp_preceded!(char_p(':'))), atom_expr_p),
                // sp ':' sp atom sp '#'
                preceded(
                    sp_terminated!(sp_preceded!(char_p(':'))),
                    terminated(atom_expr_p, sp_preceded!(char_p('#'))),
                ),
            ))),
            |(operator, enumerator, body)| {
                Atom::Sequential(SequentialExpr {
                    operator,
                    enumerator,
                    body,
                })
            },
        ),
    )(input)
}

/// equ : '=' sp
fn equ_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    sp_terminated!(recognize(char_p('=')))(input)
}

/// end : ';' sp
fn end_p<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    sp_terminated!(recognize(char_p(';')))(input)
}

/// definition : brace_def | fn_def | k_def
fn definition_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, AlgebraicObject, E> {
    context(
        "definition",
        terminated(
            alt((
                map(k_def_p, AlgebraicObject::KProperty),
                map(brace_def_p, AlgebraicObject::PropertyGroup),
                map(fn_def_p, AlgebraicObject::Function),
            )),
            sp_p,
        ),
    )(input)
}

/// brace_def : (op | _ sp) (('l' | 'r' | 'n' | ("lr" | "rl")) sp)? def '{' sp property_list '}' sp
pub fn brace_def_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, BraceGroup, E> {
    context(
        "brace def",
        map(
            tuple((
                alt((
                    op_p,
                    sp_terminated!(map(satisfy(|c| c == '_'), |op| Operator { op })),
                )),
                opt(sp_terminated!(alt((
                    map(alt((tag("lr"), tag("rl"),)), |_| LeftRightAssociative,),
                    map(satisfy(|c| c == 'l'), |_| LeftAssociative),
                    map(satisfy(|c| c == 'r'), |_| RightAssociative),
                    map(satisfy(|c| c == 'n'), |_| NonAssociative),
                )))),
                def_p,
                sp_terminated!(char_p('{')),
                property_list_p,
                sp_terminated!(char_p('}')),
            )),
            |(operator, associativity, _, _, properties, _)| BraceGroup {
                operator,
                associativity,
                properties,
            },
        ),
    )(input)
}

/// fn_def : fn_name def atom_expr (sp ',' sp atom_expr) fn_def atom_expr end
pub fn fn_def_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, AlgebraicFunction, E> {
    context(
        "fn def",
        map(
            tuple((
                fn_name_p,
                def_p,
                separated_list1(
                    sp_terminated!(sp_preceded!(char_p(','))),
                    simple_atom_expr_p,
                ),
                fn_def_symbol_p,
                simple_atom_expr_p,
                end_p,
            )),
            |(name, _, atom_expr_args, _, atom_expr_right, _)| AlgebraicFunction {
                name: name.to_owned(),
                atom_expr_args,
                atom_expr_right,
            },
        ),
    )(input)
}

/// k_def : 'K' sp def ('?'? k_group | '?')
pub fn k_def_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, KProperty, E> {
    // TODO: fix the case 'K :: ? ;;' (doesn't work)
    context(
        "K def",
        map(
            tuple((
                sp_terminated!(char_p('K')),
                def_p,
                alt((
                    tuple((opt(value(true, char_p('?'))), map(k_group_p, Some))),
                    value((Some(true), Some(('K', 1))), char_p('?')),
                )),
                end_p,
            )),
            |(_, _, (c, group), _)| {
                let undefined_property = match c {
                    Some(v) => v, // should be true
                    None => false,
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
                    },
                }
            },
        ),
    )(input)
}

/// atom : atom_symbol | special_symbol | parenthesized_atom_expr | fn_call | sequential_expr
fn atom_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, Atom, E> {
    context(
        "atom",
        alt((
            atom_symbol_p,
            special_symbol_p,
            parenthesized_atom_expr_p,
            fn_call_p,
            sequential_expr_p,
        )),
    )(input)
}

/// big_atom : atom_expr_start* atom atom_expr_end*
fn big_atom_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, (Vec<Atom>, Option<Operator>), E> {
    context(
        "big atom",
        map(
            tuple((
                many0(atom_expr_start_p::<E>),
                atom_p,
                many0(atom_expr_end_p::<E>),
            )),
            |(starts, middle, ends)| {
                let mut atoms: Vec<Atom> = Vec::new();
                let mut operator: Option<Operator> = None;
                // stick the vectors together
                for (atom, op) in starts.clone() {
                    atoms.push(atom);
                    if operator == None {
                        operator = Some(op);
                    } else if operator != Some(op.clone()) {
                        panic!(
                            "Two types of operators in the same AtomExpr {:?} and {:?} in {:?}",
                            operator,
                            op.clone(),
                            starts
                        );
                    }
                }
                atoms.push(middle);
                for (op, atom) in ends.clone() {
                    atoms.push(atom);
                    if operator == None {
                        operator = Some(op);
                    } else if operator != Some(op.clone()) {
                        panic!(
                            "Two types of operators in the same AtomExpr {:?} and {:?} in {:?}",
                            operator,
                            op.clone(),
                            ends
                        );
                    }
                }
                (atoms, operator)
            },
        ),
    )(input)
}

/// atom_expr_start : (atom op)
/// OLD: atom_expr_start : (atom op) | generator_expr_start
fn atom_expr_start_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, (Atom, Operator), E> {
    context("atom expr start", tuple((atom_p, op_p)))(input)
}

/// atom_expr_end : (op atom)
/// OLD: atom_expr_end : (op atom) | generator_expr_end
fn atom_expr_end_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, (Operator, Atom), E> {
    context("atom expr end", tuple((op_p, atom_p)))(input)
}

/// atom_expr : big_atom (op big_atom)*
/// NOW: equivalent to simple_atom_expr
pub fn atom_expr_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, AtomExpr, E> {
    context(
        "atom expr",
        map(
            tuple((big_atom_p, many0(tuple((op_p, big_atom_p))))),
            |((mut atoms, mut operator), rest)| {
                // stick everything together
                for (op, (mut atoms2, operator2)) in rest.clone() {
                    // Add the atoms
                    atoms.append(&mut atoms2);
                    // Check for an operator update or operator error
                    if operator == None {
                        operator = Some(op);
                    } else if operator != Some(op.clone()) || operator != operator2 {
                        panic!(
                            "Two types of operators in the same AtomExpr {:?} and {:?} in {:?}",
                            operator, op, rest
                        );
                    }
                }
                AtomExpr { atoms, operator }
            },
        ),
    )(input)
}

/// simple_atom_expr : atom_p (op atom_p)*
pub fn simple_atom_expr_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, AtomExpr, E> {
    context(
        "simple atom expr",
        map(
            tuple((atom_p, many0(tuple((op_p, atom_p))))),
            |(first_atom, rest)| {
                let mut atoms: Vec<Atom> = vec![first_atom];
                let mut operator: Option<Operator> = None;
                // stick everything together
                for (op, atom) in rest.clone() {
                    atoms.push(atom);
                    // Check for an operator update or operator error
                    if operator == None {
                        operator = Some(op);
                    } else if operator != Some(op.clone()) {
                        panic!(
                            "Two types of operators in the same AtomExpr {:?} and {:?} in {:?}",
                            operator, op, rest
                        );
                    }
                }
                AtomExpr { atoms, operator }
            },
        ),
    )(input)
}

/// property_list : (property sp)*
fn property_list_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, Vec<AlgebraicProperty>, E> {
    context("property list", many0(sp_terminated!(property_p)))(input)
}

/// property : atom_expr_p equ atom_expr_p end
pub fn property_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, AlgebraicProperty, E> {
    context(
        "property",
        map(
            tuple((atom_expr_p, equ_p, atom_expr_p, end_p)),
            |(atom_expr_left, _, atom_expr_right, _)| AlgebraicProperty {
                atom_expr_left,
                atom_expr_right,
            },
        ),
    )(input)
}

/// k_group : [A-Z] [0-9]? sp
fn k_group_p<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, (char, i8), E> {
    context(
        "k group",
        map(
            tuple((
                satisfy(char::is_uppercase),
                sp_terminated!(opt(satisfy(|c| c.is_digit(10)))),
            )),
            |(e, dim)| {
                (
                    e,
                    match dim {
                        Some(value) => value as i8 - '0' as i8,
                        None => 1,
                    },
                )
            },
        ),
    )(input)
}

/// root : (sp definition)*
pub fn root<'i, E: ParseError<&'i str> + ContextError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, Vec<AlgebraicObject>, E> {
    context("all", many0(sp_preceded!(definition_p)))(input)
}

/* Additional functions on AlgebraicObjects */
pub fn split_algebraic_objects(
    alg_objects: Vec<AlgebraicObject>,
) -> (
    HashSet<AlgebraicProperty>,
    HashSet<AlgebraicFunction>,
    HashSet<KProperty>,
    AssociativityHashMap,
) {
    let mut properties: HashSet<AlgebraicProperty> = HashSet::new();
    let mut functions: HashSet<AlgebraicFunction> = HashSet::new();
    let mut k_properties: HashSet<KProperty> = HashSet::new();
    let mut operators: AssociativityHashMap = HashMap::new();
    for obj in alg_objects {
        match obj {
            AlgebraicObject::KProperty(kp) => {
                k_properties.insert(kp);
                ()
            }
            AlgebraicObject::PropertyGroup(bg) => {
                properties.extend(bg.properties);
                if bg.operator.op != '_' && bg.associativity != None {
                    let old_assoc = operators.insert(bg.operator.op, bg.associativity.unwrap());
                    match old_assoc {
                        Some(assoc) if old_assoc != bg.associativity => {
                            panic!(
                                "Two different associativities of operator {}: {} (old) and {} (new)",
                                bg.operator.op, assoc, bg.associativity.unwrap(),
                            )
                        }
                        _ => (),
                    }
                }
            }
            AlgebraicObject::Function(f) => {
                functions.insert(f);
                ()
            }
        };
    }
    (properties, functions, k_properties, operators)
}
