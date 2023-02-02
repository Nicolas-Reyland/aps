#[cfg(test)]
use apsl_lang::explorer::*;
use apsl_lang::parser::OperatorAssociativity::*;
use apsl_lang::parser::*;
use apsl_lang::repl::str2atom;
use std::collections::HashMap;

fn default_operators() -> AssociativityHashMap {
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

// strip_naked / dress_up
macro_rules! assert_eq_cloth {
    ($src_expression:expr, $dst_expression:expr, $f:ident, $operators:expr) => {
        let source_atom = str2atom($src_expression);
        let destination_atom = str2atom($dst_expression);
        let actual_atom = $f(&source_atom, $operators);
        assert_eq!(
            actual_atom,
            destination_atom,
            "Assertion of cloth equality failed: {}(\"{}\") != \"{}\".\nLeft (actual): {}\nRight (expected): {}",
            stringify!($f),
            $src_expression,
            $dst_expression,
            actual_atom,
            destination_atom
        );
    };
}

#[test]
fn test_expr_stripping() {
    assert_eq_cloth!(
        "(A + B) + C",
        "A + B + C",
        strip_expr_naked,
        &default_operators()
    );
    // Most basic test, no edge-cases
    assert_eq_cloth!(
        "((A + B) + C) + D",
        "A + B + C + D",
        strip_expr_naked,
        &default_operators()
    );
    // Only one sub-expression to strip, should not strip last sub-expression
    assert_eq_cloth!(
        "X + ((A * B) * C) + (D + E)",
        "X + (A * B * C) + (D + E)",
        strip_expr_naked,
        &default_operators()
    );
    // Right-associative
    assert_eq_cloth!(
        "A ^ (B ^ (C ^ D))",
        "A ^ B ^ C ^ D",
        strip_expr_naked,
        &default_operators()
    );
    // Left-Right-Associative
    assert_eq_cloth!(
        "A @ ((B @ C) @ D) @ E",
        "A @ B @ C @ D @ E",
        strip_expr_naked,
        &default_operators()
    );
    // Non-associative
    assert_eq_cloth!(
        "(A - B) - C - (D - (E ^ (F ^ G)))",
        "(A - B) - C - (D - (E ^ F ^ G))",
        strip_expr_naked,
        &default_operators()
    );
}

#[test]
fn test_expr_fn_call_stripping() {
    // Most basic test, no edge-cases
    assert_eq_cloth!(
        "func((A * B) * C)",
        "func(A * B * C)",
        strip_expr_naked,
        &default_operators()
    );
    // Across multiple args
    assert_eq_cloth!(
        "(f((A + B) + C, A ^ (B ^ C), A * (B * C)) + F) + G",
        "f(A + B + C, A ^ B ^ C, A * (B * C)) + F + G",
        strip_expr_naked,
        &default_operators()
    );
    // Nested function calls
    assert_eq_cloth!(
        "f(g(h(0 ^ (2 ^ o(1)))))",
        "f(g(h(0 ^ 2 ^ o(1))))",
        strip_expr_naked,
        &default_operators()
    );
}

#[test]
fn test_expr_dressing_up() {
    // Most basic test, no edge-cases
    assert_eq_cloth!(
        "A + B + C + D",
        "((A + B) + C) + D",
        dress_up_expr,
        &default_operators()
    );
    // Mix of operators
    assert_eq_cloth!(
        "X + (A * B * C) + (D + E)",
        "(X + ((A * B) * C)) + (D + E)",
        dress_up_expr,
        &default_operators()
    );
    // Right-associative
    assert_eq_cloth!(
        "A ^ B ^ (C + D) ^ E",
        "A ^ (B ^ ((C + D) ^ E))",
        dress_up_expr,
        &default_operators()
    );
    // Left-Right-Associative
    assert_eq_cloth!(
        "A @ B @ C @ D @ E",
        "(((A @ B) @ C) @ D) @ E",
        dress_up_expr,
        &default_operators()
    );
    // Non-associative
    assert_eq_cloth!(
        // This expression is illegal (right?) if '-' is non-associative, but that's not our problem
        "(A - B) - C - (D - (E ^ F ^ G))",
        "(A - B) - C - (D - (E ^ (F ^ G)))",
        dress_up_expr,
        &default_operators()
    );
    /*
    assert_eq_cloth!(
        "",
        "",
        dress_up_expr,
        &default_operators()
    );
    */
}

#[test]
fn test_expr_fn_call_dressing_up() {
    // Most basic test, no edge-cases
    assert_eq_cloth!(
        "func(A * B * C)",
        "func((A * B) * C)",
        dress_up_expr,
        &default_operators()
    );
    // Across multiple args
    assert_eq_cloth!(
        "f(A + B + C, A ^ B ^ C, A * (B * C)) + F + G",
        "(f((A + B) + C, A ^ (B ^ C), A * (B * C)) + F) + G",
        dress_up_expr,
        &default_operators()
    );
    // Nested function calls
    assert_eq_cloth!(
        "f(g(h(0 ^ 2 ^ o(1))))",
        "f(g(h(0 ^ (2 ^ o(1)))))",
        dress_up_expr,
        &default_operators()
    );
}