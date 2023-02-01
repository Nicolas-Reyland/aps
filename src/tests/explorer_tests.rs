#[cfg(test)]
use crate::explorer::*;
use crate::parser::OperatorAssociativity::*;
use crate::parser::*;

fn default_operators() -> Vec<Operator> {
    vec![
        Operator {
            op: '+',
            associativity: Unknown,
        },
        Operator {
            op: '*',
            associativity: LeftAssociative,
        },
        Operator {
            op: '^',
            associativity: RightAssociative,
        },
        Operator {
            op: '-',
            associativity: NonAssociative,
        },
    ]
}

// strip_naked / dress_up
macro_rules! assert_eq_cloth {
    ($src_expression:expr, $dst_expression:expr, $f:ident, $operators:expr) => {
        let source_expr = match atom_expr_p::<ApsParserKind>($src_expression) {
            Ok(("", expr)) => expr,
            Ok((rest, parsed)) => panic!(
                "Failed to parse source:\n'{}'\nParsed :\n{:#?}\n",
                rest, parsed,
            ),
            Err(err) => panic!("Failed to parse expression:\n{:#?}", err),
        };
        let destination_expr = match atom_expr_p::<ApsParserKind>($dst_expression) {
            Ok(("", expr)) => expr,
            Ok((rest, parsed)) => panic!(
                "Failed to parse destination:\n'{}'\nParsed :\n{:#?}\n",
                rest, parsed,
            ),
            Err(err) => panic!("Failed to parse expression:\n{:#?}", err),
        };
        let actual_expr = $f(&source_expr, $operators);
        assert_eq!(
            actual_expr,
            destination_expr,
            "Assertion of cloth equality failed: {}(\"{}\") != \"{}\".\nLeft (actual): {}\nRight (expected): {}",
            stringify!($f),
            $src_expression,
            $dst_expression,
            actual_expr,
            destination_expr
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
    // Non-associative
    assert_eq_cloth!(
        "(A - B) - C - (D - (E ^ (F ^ G)))",
        "(A - B) - C - (D - (E ^ F ^ G))",
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
    // Non-associative
    assert_eq_cloth!(
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
fn test_some_things() {
    let src_expr = match atom_expr_p::<ApsParserKind>("(X + Y) + Z") {
        Ok(("", expr)) => expr,
        Ok((rest, parsed)) => panic!(
            "Failed to parse everything:\n'{}'\nParsed :\n{:#?}\n",
            rest, parsed,
        ),
        Err(err) => panic!("Failed to parse expression:\n{:#?}", err),
    };
    let property = match root::<ApsParserKind>("+ :: { (A + B) + C = A + (B + C) ; }") {
        Ok(("", parsed)) => match parsed.first().unwrap() {
            AlgebraicObject::PropertyGroup(BraceGroup {
                properties,
                operator: _,
            }) => properties.first().unwrap().clone(),
            _ => panic!("No a brace group:\n{:#?}\n", parsed),
        },
        Ok((rest, parsed)) => panic!(
            "Failed to parse everything:\n'{}'\nParsed :\n{:#?}\n",
            rest, parsed
        ),
        Err(err) => panic!("Failed to parse property:\n{:#?}", err),
    };
    let new_expressions = apply_property(&src_expr, &property, &vec![]);
    println!("New Expressions :\n{:#?}\n", new_expressions);
}
