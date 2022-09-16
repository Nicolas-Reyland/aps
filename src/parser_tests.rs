#[cfg(test)]
use super::*;

#[test]
fn test_atom_expr_p() {
    assert_eq!(
        atom_expr_p::<(&str, ErrorKind)>(
            "A + 0 * (C ^ (2 / Q))"
        ),
        Ok(
            (
                "",
                AtomExpr {
                    atoms: vec![
                        Atom::Value(
                            'A',
                        ),
                        Atom::Special(
                            '0',
                        ),
                        Atom::Parenthesized(
                            AtomExpr {
                                atoms: vec![
                                    Atom::Value(
                                        'C',
                                    ),
                                    Atom::Parenthesized(
                                        AtomExpr {
                                            atoms: vec![
                                                Atom::Special(
                                                    '2',
                                                ),
                                                Atom::Value(
                                                    'Q',
                                                ),
                                            ],
                                            operators: vec![
                                                Operator {
                                                    op: '/',
                                                },
                                            ],
                                        },
                                    ),
                                ],
                                operators: vec![
                                    Operator {
                                        op: '^',
                                    },
                                ],
                            },
                        ),
                    ],
                    operators: vec![
                        Operator {
                            op: '+',
                        },
                        Operator {
                            op: '*',
                        },
                    ],
                },
            ),
        )
    );
}

#[test]
fn test_brace_def_p() {
    assert_eq!(
        brace_def_p::<(&str, ErrorKind)>(
            "+ :: { A = A ;; B = C ;; } "
        ),
        Ok(
            (
                "",
                BraceGroup {
                    operator: Operator {
                        op: '+',
                    },
                    properties: vec![
                        AlgebraicProperty {
                            atom_expr_left: AtomExpr {
                                atoms: vec![
                                    Atom::Value(
                                        'A',
                                    ),
                                ],
                                operators: vec![],
                            },
                            atom_expr_right: AtomExpr {
                                atoms: vec![
                                    Atom::Value(
                                        'A',
                                    ),
                                ],
                                operators: vec![],
                            },
                        },
                        AlgebraicProperty {
                            atom_expr_left: AtomExpr {
                                atoms: vec![
                                    Atom::Value(
                                        'B',
                                    ),
                                ],
                                operators: vec![],
                            },
                            atom_expr_right: AtomExpr {
                                atoms: vec![
                                    Atom::Value(
                                        'C',
                                    ),
                                ],
                                operators: vec![],
                            },
                        },
                    ],
                },
            ),
        )
    )
}

#[test]
fn test_fn_def_p() {
    assert_eq!(
        fn_def_p::<(&str, ErrorKind)>(
            "square :: A -> A ^ 2 ;; "
        ),
        Ok(
            (
                "",
                AlgebraicFunction {
                    name: "square ",
                    atom_expr_left: AtomExpr {
                        atoms: vec![
                            Atom::Value(
                                'A',
                            ),
                        ],
                        operators: vec![],
                    },
                    atom_expr_right: AtomExpr {
                        atoms: vec![
                            Atom::Value(
                                'A',
                            ),
                            Atom::Special(
                                '2',
                            ),
                        ],
                        operators: vec![
                            Operator {
                                op: '^',
                            },
                        ],
                    },
                },
            ),
        )
    )
}

#[test]
fn test_k_def_p() {
    assert_eq!(
        k_def_p::<(&str, ErrorKind)>(
            "K :: ?N5 ;; "
        ),
        Ok(
            (
                ";; ",
                KProperty {
                    undefined_property: true,
                    base: 'N',
                    dim: 5,
                },
            ),
        )
    )
}

#[test]
fn test_generator_expr_p() {
    assert_eq!(
        generator_expr_p::<(&str, ErrorKind)>(
            "$ * A + B $ # C "
        ),
        Ok(
            (
                "",
                Atom::Generator(
                    GeneratorExpr {
                        elements: vec![
                            GeneratorElement::GenOperator(
                                Operator {
                                    op: '*',
                                },
                            ),
                            GeneratorElement::GenAtom(
                                Atom::Value(
                                    'A',
                                ),
                            ),
                            GeneratorElement::GenOperator(
                                Operator {
                                    op: '+',
                                },
                            ),
                            GeneratorElement::GenAtom(
                                Atom::Value(
                                    'B',
                                ),
                            ),
                        ],
                        iterator: Box::new(
                            Atom::Value(
                                'C',
                            )
                        ),
                    },
                ),
            ),
        )
    )
}
