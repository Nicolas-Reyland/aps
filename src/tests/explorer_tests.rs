#[cfg(test)]
use crate::explorer::*;
use crate::parser::OperatorAssociativity::*;
use crate::parser::*;

// strip_naked / dress_up
macro_rules! assert_eq_cloth {
    ($expression:expr, $expected:expr, $f:ident, $operators:expr) => {
        let source_expr = match atom_expr_p::<ApsParserKind>($expression) {
            Ok(("", expr)) => expr,
            Ok((rest, parsed)) => panic!(
                "Failed to parse everything:\n'{}'\nParsed :\n{:#?}\n",
                rest, parsed,
            ),
            Err(err) => panic!("Failed to parse expression:\n{:#?}", err),
        };
        let actual_expr = $f(&source_expr, $operators);
        let expected_expr = $expected;
        assert_eq!(
            actual_expr,
            expected_expr,
            "Assertion of cloth equality failed: {}(\"{}\").\nLeft (actual): {:#?}\nRight (expected): {:#?}",
            stringify!($f),
            $expression,
            actual_expr,
            expected_expr
        );
    };
}

#[test]
fn test_expr_stripping() {
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
    assert_eq_cloth!(
        "(A + B) + C",
        AtomExpr {
            atoms: vec![
                Atom::Value("A".to_string()),
                Atom::Value("B".to_string()),
                Atom::Value("C".to_string()),
            ],
            operator: Some(Operator {
                op: '+',
                associativity: Unknown
            }),
        },
        strip_expr_naked,
        &operators
    );
    // Most basic test, no edge-cases
    assert_eq_cloth!(
        "((A + B) + C) + D",
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
        },
        strip_expr_naked,
        &operators
    );
    // Only one sub-expression to strip, should not strip last sub-expression
    assert_eq_cloth!(
        "X + ((A * B) * C) + (D + E)",
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
        },
        strip_expr_naked,
        &operators
    );
    // Right-associativity
    assert_eq_cloth!(
        "A ^ (B ^ (C ^ D))",
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
        },
        strip_expr_naked,
        &operators
    );
}

#[test]
fn test_expr_dressing_up() {
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
    assert_eq_cloth!(
        "A + B + C + D",
        AtomExpr {
            atoms: vec![
                Atom::Parenthesized(AtomExpr {
                    atoms: vec![
                        Atom::Parenthesized(AtomExpr {
                            atoms: vec![Atom::Value("A".to_string()), Atom::Value("B".to_string()),],
                            operator: Some(Operator {
                                op: '+',
                                associativity: Unknown
                            }),
                        }),
                        Atom::Value("C".to_string()),
                    ],
                    operator: Some(Operator {
                        op: '+',
                        associativity: Unknown
                    }),
                }),
                Atom::Value("D".to_string()),
            ],
            operator: Some(Operator {
                op: '+',
                associativity: Unknown
            }),
        },
        dress_up_expr,
        &operators
    );
    // Mix of operators
    assert_eq_cloth!(
        "X + (A * B * C) + (D + E)",
        AtomExpr {
            atoms: vec![
                Atom::Parenthesized(AtomExpr {
                    atoms: vec![
                        Atom::Value("X".to_string()),
                        Atom::Parenthesized(AtomExpr {
                            atoms: vec![
                                Atom::Parenthesized(AtomExpr {
                                    atoms: vec![
                                        Atom::Value("A".to_string()),
                                        Atom::Value("B".to_string()),
                                    ],
                                    operator: Some(Operator {
                                        op: '*',
                                        associativity: Unknown
                                    }),
                                }),
                                Atom::Value("C".to_string()),
                            ],
                            operator: Some(Operator {
                                op: '*',
                                associativity: Unknown
                            })
                        },),
                    ],
                    operator: Some(Operator {
                        op: '+',
                        associativity: Unknown
                    })
                }),
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
        },
        dress_up_expr,
        &operators
    );
    // Right associativity
    assert_eq_cloth!(
        "A ^ B ^ (C + D)",
        AtomExpr {
            atoms: vec![
                Atom::Value("A".to_string(),),
                Atom::Parenthesized(AtomExpr {
                    atoms: vec![
                        Atom::Value("B".to_string(),),
                        Atom::Parenthesized(AtomExpr {
                            atoms: vec![
                                Atom::Value("C".to_string(),),
                                Atom::Value("D".to_string(),),
                            ],
                            operator: Some(Operator {
                                op: '+',
                                associativity: Unknown,
                            },),
                        },),
                    ],
                    operator: Some(Operator {
                        op: '^',
                        associativity: Unknown,
                    },),
                },),
            ],
            operator: Some(Operator {
                op: '^',
                associativity: Unknown,
            },),
        },
        dress_up_expr,
        &operators
    );
    /*
    assert_eq_cloth!(
        "A + B + C + D",
        AtomExpr {
            atoms: vec![],
            operator: None,
        },
        dress_up_expr,
        &operators
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
