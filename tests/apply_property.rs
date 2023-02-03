#[cfg(test)]
use apsl_lang::{explorer::*, parser::*, repl::*};
use std::collections::HashSet;

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

macro_rules! assert_eq_property_appliance {
    ($src_expr:expr, $property:expr, $expected:expr, $context:ident) => {
        let src_expr = str2atom($src_expr);
        let property = parse_property!($property);
        let actual = apply_property(&src_expr, &property, &$context.associativities);
        let expected: HashSet<Atom> = $expected
            .iter()
            .map(|expr_str| str2atom(expr_str))
            .collect();
        assert_eq!(
            actual.len(),
            expected.len(),
            "lengths differ (actual != expected): {} != {}\nActual: {:#?}\nExpected: {:#?}",
            actual.len(),
            expected.len(),
            actual,
            expected,
        );
        assert_eq!(
            actual, expected,
            "HashSets differ\nActual: {:#?}\nExpected: {:#?}",
            actual, expected,
        );
    };
}

#[test]
fn apply_property_basic() {
    let mut context = init_context();
    import_into_context(&mut context, "examples/plus.apsl");
    // Most basic case, no edge-cases
    assert_eq_property_appliance!("A + B", "A + B = B + A ;", vec!["B + A",], context);
}

#[test]
fn apply_property_sub_expression() {
    let mut context = init_context();
    import_into_context(&mut context, "examples/plus.apsl");
    // Sub-expression
    assert_eq_property_appliance!(
        "(A + B) * C",
        "A + B = B + A ;",
        vec!["(B + A) * C",],
        context
    );
}

#[test]
fn apply_property_fn_call_args() {
    let mut context = init_context();
    import_into_context(&mut context, "examples/plus.apsl");
    // fn-call args
    assert_eq_property_appliance!(
        "f(A + B, C + D)",
        "A + B = B + A ;",
        vec!["f(B + A, C + D)", "f(A + B, D + C)",],
        context
    );
}

#[test]
fn apply_property_sequential() {
    let mut context = init_context();
    import_into_context(&mut context, "examples/plus.apsl");
    // fn-call args
    assert_eq_property_appliance!(
        "# + : 4 : A + B #",
        "A + B = B + A ;",
        vec!["# + : 4 : B + A #",],
        context
    );
}

#[test]
fn apply_property_into_sequential() {
    let mut context = init_context();
    import_into_context(&mut context, "examples/plus.apsl");
    // fn-call args
    assert_eq_property_appliance!(
        "(X * X) * X",
        "A ^ N = # * : N : A # ;",
        vec!["# * : 3 : X #",],
        context
    );
}

#[test]
fn apply_property_ambiguity() {
    let mut context = init_context();
    import_into_context(&mut context, "examples/plus.apsl");
    // fn-call args
    assert_eq_property_appliance!(
        "A + B",
        "A = A * 1 ;",
        vec!["(A * 1) + B", "A + (B * 1)",],
        context
    );
}
