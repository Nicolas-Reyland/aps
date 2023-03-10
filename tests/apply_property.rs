use aps::parser::OperatorAssociativity::*;
#[cfg(test)]
use aps::{explorer::*, parser::*, repl::*};
use std::collections::{HashMap, HashSet};

fn default_associativities() -> AssociativityHashMap {
    let mut hash_map: AssociativityHashMap = HashMap::with_capacity(5);
    for (k, v) in vec![
        ('+', Unknown),
        ('*', LeftAssociative),
        ('^', RightAssociative),
        ('@', LeftRightAssociative),
        ('-', NonAssociative),
    ] {
        hash_map.insert(k, v);
    }
    hash_map
}

macro_rules! parse_property {
    ($expression:expr) => {
        match property_p::<ApsParserKind>($expression) {
            Ok(("", expr)) => expr,
            Ok((rest, parsed)) => panic!(
                "Failed to parse property:\n'{}'\nParsed :\n{:#?}\n",
                rest, parsed,
            ),
            Err(err) => panic!("Failed to parse property:\n{:#?}", err),
        }
    };
}

macro_rules! show_atom_iterable {
    ($atoms:expr) => {{
        let mut s = String::new();
        for atom in &$atoms {
            s.push_str(&format!(" - {}\n", format_toplevel_atom(atom)));
        }
        s
    }};
}

macro_rules! assert_eq_property_appliance {
    ($src_expr:expr, $property:expr, $expected:expr) => {
        let src_expr = str2atom($src_expr);
        let property = parse_property!($property);
        let actual = apply_property(&src_expr, &property, &default_associativities());
        let expected: HashSet<Atom> = $expected
            .iter()
            .map(|expr_str| str2atom(expr_str))
            .collect();
        assert_eq!(
            actual.len(),
            expected.len(),
            "lengths differ (actual != expected): {} != {}\nActual:\n{}Expected:\n{}",
            actual.len(),
            expected.len(),
            show_atom_iterable!(actual),
            show_atom_iterable!(expected)
        );
        assert_eq!(
            actual.clone(),
            expected.clone(),
            "HashSets differ\nActual:\n{}Expected:\n{}",
            show_atom_iterable!(actual),
            show_atom_iterable!(expected)
        );
    };
}

macro_rules! assert_eq_match_and_apply {
    ($src_expr:expr, $property:expr, $expected:expr) => {
        let src_expr = str2atom($src_expr);
        let property = parse_property!($property);
        let actual = match_and_apply(
            &src_expr,
            &property.left_atom,
            &property.right_atom,
            &default_associativities(),
        );
        let expected: HashSet<Atom> = $expected
            .iter()
            .map(|expr_str| str2atom(expr_str))
            .collect();
        assert_eq!(
            actual.len(),
            expected.len(),
            "lengths differ (actual != expected): {} != {}\nActual:\n{}Expected:\n{}",
            actual.len(),
            expected.len(),
            show_atom_iterable!(actual),
            show_atom_iterable!(expected)
        );
        assert_eq!(
            actual.clone(),
            expected.clone(),
            "HashSets differ\nActual:\n{}Expected:\n{}",
            show_atom_iterable!(actual),
            show_atom_iterable!(expected)
        );
    };
}

#[test]
fn match_and_apply_basic() {
    // Most basic case, no edge-cases
    assert_eq_match_and_apply!("A + B", "A + B = B + A ;", vec!["B + A",]);
}

#[test]
fn match_and_apply_sub_expression() {
    // Sub-expression
    assert_eq_match_and_apply!("(A + B) * C", "A + B = B + A ;", vec!["(B + A) * C",]);
}

#[test]
fn match_and_apply_fn_call_args() {
    // fn-call args
    assert_eq_match_and_apply!(
        "f(A + B, C + D)",
        "A + B = B + A ;",
        vec!["f(B + A, C + D)", "f(A + B, D + C)",]
    );
}

#[test]
fn match_and_apply_ambiguity() {
    // fn-call args
    assert_eq_match_and_apply!(
        "A + B",
        "A = A * 1 ;",
        vec!["(A * 1) + B", "A + (B * 1)", "(A + B) * 1",]
    );
}

#[test]
fn match_and_apply_sequential() {
    // fn-call args
    assert_eq_match_and_apply!(
        "# + : 4 : A + B #",
        "A + B = B + A ;",
        vec!["# + : 4 : B + A #",]
    );
}

#[test]
fn match_and_apply_into_sequential() {
    // fn-call args
    assert_eq_match_and_apply!(
        "(X * X) * X",
        "# * : N : A # = A ^ N ;",
        vec![
            "(X ^ 2) * X",
            "(X ^ 3)",
            "((X ^ 1) * X) * X",
            "(X * (X ^ 1)) * X",
            "(X * X) * (X ^ 1)",
        ]
    );
}

#[test]
fn apply_property_ambiguity() {
    // fn-call args
    assert_eq_property_appliance!(
        "A + B",
        "A = A * 1 ;",
        vec!["(A * 1) + B", "A + (B * 1)", "(A + B) * 1",]
    );
}

#[test]
fn apply_property_into_sequential() {
    // fn-call args
    assert_eq_property_appliance!(
        "(X * X) * X",
        "A ^ N = # * : N : A # ;",
        vec![
            "(X ^ 2) * X",
            "(X ^ 3)",
            "((X ^ 1) * X) * X",
            "(X * (X ^ 1)) * X",
            "(X * X) * (X ^ 1)",
        ]
    );
}
