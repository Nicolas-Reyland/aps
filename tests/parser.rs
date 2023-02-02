use apsl_lang::parser::OperatorAssociativity::NonAssociative;
use apsl_lang::parser::*;

#[test]
fn brace_def() {
    assert_eq!(
        brace_def_p::<ApsParserKind>("+ n :: { A = 3 ; B = C ; } "),
        Ok((
            "",
            BraceGroup {
                operator: Operator { op: '+' },
                associativity: Some(NonAssociative),
                properties: vec![
                    AlgebraicProperty {
                        left_atom: Atom::Parenthesized(AtomExpr {
                            atoms: vec![Atom::Symbol("A".to_string(),),],
                            operator: None,
                        }),
                        right_atom: Atom::Parenthesized(AtomExpr {
                            atoms: vec![Atom::Value(3,),],
                            operator: None,
                        }),
                    },
                    AlgebraicProperty {
                        left_atom: Atom::Parenthesized(AtomExpr {
                            atoms: vec![Atom::Symbol("B".to_string(),),],
                            operator: None,
                        }),
                        right_atom: Atom::Parenthesized(AtomExpr {
                            atoms: vec![Atom::Symbol("C".to_string(),),],
                            operator: None,
                        }),
                    },
                ],
            },
        ),)
    )
}

#[test]
fn fn_def() {
    assert_eq!(
        fn_def_p::<ApsParserKind>("pow :: A, N -> A ^ 2 ; "),
        Ok((
            "",
            AlgebraicFunction {
                name: "square".to_string(),
                arg_atoms: vec![Atom::Symbol("A".to_string(),),Atom::Symbol("N".to_string(),),],
                value_atom: Atom::Parenthesized(AtomExpr {
                    atoms: vec![Atom::Symbol("A".to_string(),), Atom::Value(2,),],
                    operator: Some(Operator { op: '^' }),
                }),
            },
        ),)
    )
}

#[test]
fn fn_def_many_args() {
    assert_eq!(
        fn_def_p::<ApsParserKind>("square :: A -> A ^ 2 ; "),
        Ok((
            "",
            AlgebraicFunction {
                name: "square".to_string(),
                arg_atoms: vec![Atom::Symbol("A".to_string(),),],
                value_atom: Atom::Parenthesized(AtomExpr {
                    atoms: vec![Atom::Symbol("A".to_string(),), Atom::Value(2,),],
                    operator: Some(Operator { op: '^' }),
                }),
            },
        ),)
    )
}

#[test]
fn k_def() {
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
fn fn_call() {
    assert_eq!(
        fn_call_p::<ApsParserKind>("exp(A ^ 2) "),
        Ok((
            "",
            Atom::FunctionCall(FunctionCallExpr {
                name: "exp".to_string(),
                args: vec![Atom::Parenthesized(AtomExpr {
                    atoms: vec![Atom::Symbol("A".to_string(),), Atom::Value(2,),],
                    operator: Some(Operator { op: '^' }),
                }),],
            })
        ))
    )
}
