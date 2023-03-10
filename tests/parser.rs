use aps::parser::OperatorAssociativity::NonAssociative;
use aps::parser::*;

#[test]
fn atom_symbol() {
    assert_eq!(
        atom_symbol_p::<ApsParserKind>("X "),
        Ok(("", Atom::Symbol("X".to_string())))
    );
}

#[test]
fn atom_value() {
    assert_eq!(
        atom_value_p::<ApsParserKind>("8 "),
        Ok(("", Atom::Value(8)))
    );
    assert_eq!(
        atom_value_p::<ApsParserKind>("12 "),
        Ok(("", Atom::Value(12)))
    );
}

#[test]
fn op() {
    assert_eq!(op_p::<ApsParserKind>("+ "), Ok(("", Operator { op: '+' })));
}

#[test]
fn atom() {
    // symbol
    assert_eq!(
        atom_p::<ApsParserKind>("A "),
        Ok(("", Atom::Symbol("A".to_string())))
    );
    // value
    assert_eq!(atom_p::<ApsParserKind>("3 "), Ok(("", Atom::Value(3))));
    // atom-expression (with parentheses)
    assert_eq!(
        atom_p::<ApsParserKind>("( 1 + 1 ) "),
        Ok((
            "",
            Atom::Parenthesized(AtomExpr {
                atoms: vec![Atom::Value(1), Atom::Value(1),],
                operator: Some(Operator { op: '+' })
            })
        ))
    );
}

#[test]
fn toplevel_atom() {
    // atom-expression (without parentheses)
    assert_eq!(
        toplevel_atom_p::<ApsParserKind>("1 + 1  "),
        Ok((
            "",
            Atom::Parenthesized(AtomExpr {
                atoms: vec![Atom::Value(1), Atom::Value(1),],
                operator: Some(Operator { op: '+' })
            })
        ))
    );
}

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
                        left_atom: Atom::Symbol("A".to_string(),),
                        right_atom: Atom::Value(3,),
                    },
                    AlgebraicProperty {
                        left_atom: Atom::Symbol("B".to_string(),),
                        right_atom: Atom::Symbol("C".to_string(),),
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
                name: "pow".to_string(),
                arg_atoms: vec![
                    Atom::Symbol("A".to_string(),),
                    Atom::Symbol("N".to_string(),),
                ],
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
