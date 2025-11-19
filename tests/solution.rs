use std::collections::HashSet;

use aps::clothing::dress_up_expr;
use aps::parser::{
    self, split_algebraic_objects, AlgebraicFunction, AlgebraicProperty, ApsParserKind,
    AssociativityHashMap, Atom, FunctionCallExpr, KProperty,
};
use aps::repl::str2atom;
use aps::solution::solve_equality;

fn parse_algebra(
    src: &str,
) -> (
    HashSet<AlgebraicProperty>,
    HashSet<KProperty>,
    AssociativityHashMap,
) {
    let (_, objects) =
        parser::root::<ApsParserKind>(src).expect("failed to parse algebra definition");
    let (mut properties, functions, k_properties, associativities) =
        split_algebraic_objects(objects);
    properties.extend(functions_to_properties(&functions));
    (properties, k_properties, associativities)
}

fn functions_to_properties(functions: &HashSet<AlgebraicFunction>) -> HashSet<AlgebraicProperty> {
    functions
        .iter()
        .map(|function| AlgebraicProperty {
            left_atom: Atom::FunctionCall(FunctionCallExpr {
                name: function.name.clone(),
                args: function.arg_atoms.clone(),
            }),
            right_atom: function.value_atom.clone(),
        })
        .collect()
}

#[test]
fn solve_commutative_chain() {
    let (properties, k_properties, associativities) = parse_algebra(
        r#"
        + lr :: {
            A + B = B + A ;
            (A + B) + C = A + (B + C) ;
        }
        "#,
    );

    let left = str2atom("A + B + C");
    let right = str2atom("C + B + A");

    let solution = solve_equality(
        properties,
        k_properties,
        &associativities,
        &left,
        &right,
        true,
    )
    .expect("expected proof for permutation of sum");

    assert_eq!(
        solution.first().unwrap().0,
        dress_up_expr(&left, &associativities)
    );
    assert_eq!(
        solution.last().unwrap().0,
        dress_up_expr(&right, &associativities)
    );
    assert!(
        solution.len() > 1,
        "route should include transformation steps"
    );
    assert!(solution.iter().any(|(_, rule, _)| rule.is_some()));
}

#[test]
fn solve_requires_identity_axiom() {
    let (properties, k_properties, associativities) = parse_algebra(
        r#"
        + :: {
            A + B = B + A ;
        }
        "#,
    );

    let left = str2atom("A + 0");
    let right = str2atom("A");

    let result = solve_equality(
        properties,
        k_properties,
        &associativities,
        &left,
        &right,
        true,
    );

    assert!(result.is_none(), "missing identity should prevent a proof");
}

#[test]
fn solve_distributive_expansion() {
    let (properties, k_properties, associativities) = parse_algebra(
        r#"
        + lr :: {
            A + B = B + A ;
            (A + B) + C = A + (B + C) ;
        }

        * lr :: {
            A * B = B * A ;
            (A * B) * C = A * (B * C) ;
        }

        _ :: {
            A * (B + C) = (A * B) + (A * C) ;
        }
        "#,
    );

    let left = str2atom("(X + Y) * Z");
    let right = str2atom("(X * Z) + (Y * Z)");

    let solution = solve_equality(
        properties,
        k_properties,
        &associativities,
        &left,
        &right,
        true,
    )
    .expect("expected proof via distributivity");

    assert_eq!(
        solution.first().unwrap().0,
        dress_up_expr(&left, &associativities)
    );
    assert_eq!(
        solution.last().unwrap().0,
        dress_up_expr(&right, &associativities)
    );
    assert!(
        solution.len() >= 3,
        "route should include distributive and commutative steps"
    );
}

#[test]
fn solve_function_definition_expansion() {
    let (properties, k_properties, associativities) = parse_algebra(
        r#"
        + lr :: {
            A + B = B + A ;
        }

        double :: A -> A + A ;
        "#,
    );

    let left = str2atom("double(M)");
    let right = str2atom("M + M");

    let solution = solve_equality(
        properties,
        k_properties,
        &associativities,
        &left,
        &right,
        true,
    )
    .expect("expected function-derived property to enable proof");

    assert_eq!(
        solution.first().unwrap().0,
        dress_up_expr(&left, &associativities)
    );
    assert_eq!(
        solution.last().unwrap().0,
        dress_up_expr(&right, &associativities)
    );
    let expected_rule = parser::property_p::<ApsParserKind>("double(A) = A + A ;")
        .map(|(_, property)| property)
        .unwrap();
    assert!(
        solution
            .iter()
            .any(|(_, rule, _)| rule.as_ref() == Some(&expected_rule)),
        "proof should apply the function definition property"
    );
}

#[test]
fn solve_sequential_power_expansion() {
    let (properties, k_properties, associativities) = parse_algebra(
        r#"
        * lr :: {
            A * B = B * A ;
            (A * B) * C = A * (B * C) ;
        }

        _ :: {
            A ^ N = # * : N : A # ;
        }
        "#,
    );

    let left = str2atom("X ^ 3");
    let right = str2atom("(X * X) * X");

    let solution = solve_equality(
        properties,
        k_properties,
        &associativities,
        &left,
        &right,
        true,
    )
    .expect("expected sequential power expansion proof");

    assert_eq!(
        solution.first().unwrap().0,
        dress_up_expr(&left, &associativities)
    );
    assert_eq!(
        solution.last().unwrap().0,
        dress_up_expr(&right, &associativities)
    );
    let expected_rule = parser::property_p::<ApsParserKind>("A ^ N = # * : N : A # ;")
        .map(|(_, property)| property)
        .unwrap();
    assert!(
        solution
            .iter()
            .any(|(_, rule, _)| rule.as_ref() == Some(&expected_rule)),
        "proof should rely on the sequential power rule"
    );
}

#[test]
fn invalid_function_arguments_are_ignored() {
    let (properties, k_properties, associativities) = parse_algebra(
        r#"
        bad :: (A + B) -> A ;
        "#,
    );

    let left = str2atom("bad(X + Y)");
    let right = str2atom("X");

    let result = solve_equality(
        properties,
        k_properties,
        &associativities,
        &left,
        &right,
        true,
    );

    assert!(
        result.is_none(),
        "invalid function should not generate a proof"
    );
}
