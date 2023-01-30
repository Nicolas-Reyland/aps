#[cfg(test)]
use crate::parser::*;

#[test]
fn test_brace_def_p() {
    assert_eq!(
        brace_def_p::<ApsParserKind>("+ :: { A = A ; B = C ; } "),
        Ok((
            "",
            BraceGroup {
                operator: Operator { op: '+' },
                properties: vec![
                    AlgebraicProperty {
                        atom_expr_left: AtomExpr {
                            atoms: vec![Atom::Value('A',),],
                            operator: None,
                        },
                        atom_expr_right: AtomExpr {
                            atoms: vec![Atom::Value('A',),],
                            operator: None,
                        },
                    },
                    AlgebraicProperty {
                        atom_expr_left: AtomExpr {
                            atoms: vec![Atom::Value('B',),],
                            operator: None,
                        },
                        atom_expr_right: AtomExpr {
                            atoms: vec![Atom::Value('C',),],
                            operator: None,
                        },
                    },
                ],
            },
        ),)
    )
}

#[test]
fn test_fn_def_p() {
    assert_eq!(
        fn_def_p::<ApsParserKind>("square :: A -> A ^ 2 ; "),
        Ok((
            "",
            AlgebraicFunction {
                name: "square".to_string(),
                atom_expr_left: AtomExpr {
                    atoms: vec![Atom::Value('A',),],
                    operator: None,
                },
                atom_expr_right: AtomExpr {
                    atoms: vec![Atom::Value('A',), Atom::Special('2',),],
                    operator: Some(Operator { op: '^' }),
                },
            },
        ),)
    )
}

#[test]
fn test_k_def_p() {
    assert_eq!(
        k_def_p::<ApsParserKind>("K :: ?N5 ; "),
        Ok((
            "",
            KProperty {
                undefined_property: true,
                base: 'N',
                dim: 5,
            },
        ),)
    )
}

#[test]
fn test_fn_call_p() {
    assert_eq!(
        fn_call_p::<ApsParserKind>("exp(A ^ 2) "),
        Ok((
            "",
            Atom::FunctionCall((
                "exp".to_string(),
                AtomExpr {
                    atoms: vec![Atom::Value('A',), Atom::Special('2',),],
                    operator: Some(Operator { op: '^' }),
                }
            ))
        ))
    )
}
