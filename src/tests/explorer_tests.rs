#[cfg(test)]
use crate::explorer::*;
use crate::parser::OperatorAssociativity::*;
use crate::parser::*;

macro_rules! assert_eq_atom_expr {
    ($actual_expr:expr, $expected_expr:expr) => {
        let left_expr = $actual_expr;
        let right_expr = $expected_expr;
        assert_eq!(
            // ((A + B) + C) + D
            left_expr,
            right_expr,
            "Assertion of equality for AtomExpr failed.\nLeft: {:#?}\nRight: {:#?}",
            left_expr,
            right_expr,
        );
    };
}

#[test]
fn test_expr_stripping() {
    let expr_a = match atom_expr_p::<ApsParserKind>("((A + B) + C) + D") {
        Ok(("", expr)) => expr,
        Ok((rest, parsed)) => panic!(
            "Failed to parse everything:\n'{}'\nParsed :\n{:#?}\n",
            rest, parsed,
        ),
        Err(err) => panic!("Failed to parse expression:\n{:#?}", err),
    };
    let expr_b = match atom_expr_p::<ApsParserKind>("X + ((A * B) * C) + (D + E)") {
        Ok(("", expr)) => expr,
        Ok((rest, parsed)) => panic!(
            "Failed to parse everything:\n'{}'\nParsed :\n{:#?}\n",
            rest, parsed,
        ),
        Err(err) => panic!("Failed to parse expression:\n{:#?}", err),
    };
    let expr_c = match atom_expr_p::<ApsParserKind>("A ^ (B ^ (D ^ C))") {
        Ok(("", expr)) => expr,
        Ok((rest, parsed)) => panic!(
            "Failed to parse everything:\n'{}'\nParsed :\n{:#?}\n",
            rest, parsed,
        ),
        Err(err) => panic!("Failed to parse expression:\n{:#?}", err),
    };
    let operators: Vec<Operator> = vec![
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
    ];
    // Most basic test, no edge-cases
    assert_eq_atom_expr!(
        strip_expr_naked(&expr_a, &operators),
        AtomExpr {
            atoms: vec![
                Atom::Value("A".to_string()),
                Atom::Value("B".to_string()),
                Atom::Value("C".to_string()),
                Atom::Value("D".to_string()),
            ],
            operator: Some(Operator {
                op: '+',
                associativity: Unknown
            }),
        }
    );
    // Only one sub-expression to strip, should not strip last sub-expression
    assert_eq!(
        // X + ((A * B) * C) + D
        strip_expr_naked(&expr_b, &operators),
        AtomExpr {
            atoms: vec![
                Atom::Value("X".to_string()),
                Atom::Parenthesized(AtomExpr {
                    atoms: vec![
                        Atom::Value("A".to_string()),
                        Atom::Value("B".to_string()),
                        Atom::Value("C".to_string()),
                    ],
                    operator: Some(Operator {
                        op: '*',
                        associativity: Unknown
                    })
                },),
                Atom::Parenthesized(AtomExpr {
                    atoms: vec![Atom::Value("D".to_string()), Atom::Value("E".to_string()),],
                    operator: Some(Operator {
                        op: '+',
                        associativity: Unknown
                    })
                },),
            ],
            operator: Some(Operator {
                op: '+',
                associativity: Unknown
            }),
        }
    );
    // Right-associativity
    assert_eq!(
        // A ^ (B ^ (D ^ C))
        strip_expr_naked(&expr_c, &operators),
        AtomExpr {
            atoms: vec![
                Atom::Value("A".to_string()),
                Atom::Value("B".to_string()),
                Atom::Value("C".to_string()),
                Atom::Value("D".to_string()),
            ],
            operator: Some(Operator {
                op: '^',
                associativity: Unknown
            })
        }
    )
}

#[test]
fn test_expr_dressing_up() {
    let expr_a = match atom_expr_p::<ApsParserKind>("A + B + C + D") {
        Ok(("", expr)) => expr,
        Ok((rest, parsed)) => panic!(
            "Failed to parse everything:\n'{}'\nParsed :\n{:#?}\n",
            rest, parsed,
        ),
        Err(err) => panic!("Failed to parse expression:\n{:#?}", err),
    };
    let expr_b = match atom_expr_p::<ApsParserKind>("X + (A * B * C) + D") {
        Ok(("", expr)) => expr,
        Ok((rest, parsed)) => panic!(
            "Failed to parse everything:\n'{}'\nParsed :\n{:#?}\n",
            rest, parsed,
        ),
        Err(err) => panic!("Failed to parse expression:\n{:#?}", err),
    };
    let operators: Vec<Operator> = vec![
        Operator {
            op: '+',
            associativity: OperatorAssociativity::Unknown,
        },
        Operator {
            op: '*',
            associativity: OperatorAssociativity::LeftAssociative,
        },
        Operator {
            op: '^',
            associativity: OperatorAssociativity::RightAssociative,
        },
    ];
    println!("expr_a = {}", expr_a);
    println!("dress_up(expr_a) = {}", dress_up_expr(&expr_a, &operators));
    println!("expr_b = {}", expr_b);
    println!("dress_up(expr_b) = {}", dress_up_expr(&expr_b, &operators));
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
