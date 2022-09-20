#[cfg(test)]

use super::*;
use crate::{solution::solve_equality, repl::str2atom_expr, aps_parser::{AtomExpr, Operator, Atom, AlgebraicProperty}};

#[test]
fn test_x_y_z_solution() {
    let mut context = init_context();
    import_into_context(&mut context, "examples/numbers.aps");
    let left = str2atom_expr("X + Y + Z");
    let right = str2atom_expr("Z + Y + X");
    assert_eq!(
        solve_equality(
            context.properties,
            context.functions,
            context.k_properties,
            &left,
            &right,
            context.auto_break,
        ),
        Some(
            vec![
                (
                    AtomExpr {
                        atoms: vec![
                            Atom::Value(
                                'X',
                            ),
                            Atom::Value(
                                'Y',
                            ),
                            Atom::Value(
                                'Z',
                            ),
                        ],
                        operators: vec![
                            Operator {
                                op: '+',
                            },
                            Operator {
                                op: '+',
                            },
                        ],
                    },
                    None,
                ),
                (
                    AtomExpr {
                        atoms: vec![
                            Atom::Parenthesized(
                                AtomExpr {
                                    atoms: vec![
                                        Atom::Value(
                                            'X',
                                        ),
                                        Atom::Value(
                                            'Y',
                                        ),
                                    ],
                                    operators: vec![
                                        Operator {
                                            op: '+',
                                        },
                                    ],
                                },
                            ),
                            Atom::Value(
                                'Z',
                            ),
                        ],
                        operators: vec![
                            Operator {
                                op: '+',
                            },
                        ],
                    },
                    Some(
                        AlgebraicProperty {
                            atom_expr_left: AtomExpr {
                                atoms: vec![
                                    Atom::Value(
                                        'A',
                                    ),
                                    Atom::Value(
                                        'B',
                                    ),
                                    Atom::Extension,
                                ],
                                operators: vec![
                                    Operator {
                                        op: '+',
                                    },
                                    Operator {
                                        op: '+',
                                    },
                                ],
                            },
                            atom_expr_right: AtomExpr {
                                atoms: vec![
                                    Atom::Parenthesized(
                                        AtomExpr {
                                            atoms: vec![
                                                Atom::Value(
                                                    'A',
                                                ),
                                                Atom::Value(
                                                    'B',
                                                ),
                                            ],
                                            operators: vec![
                                                Operator {
                                                    op: '+',
                                                },
                                            ],
                                        },
                                    ),
                                    Atom::Extension,
                                ],
                                operators: vec![
                                    Operator {
                                        op: '+',
                                    },
                                ],
                            },
                        },
                    ),
                ),
                (
                    AtomExpr {
                        atoms: vec![
                            Atom::Value(
                                'Z',
                            ),
                            Atom::Parenthesized(
                                AtomExpr {
                                    atoms: vec![
                                        Atom::Value(
                                            'X',
                                        ),
                                        Atom::Value(
                                            'Y',
                                        ),
                                    ],
                                    operators: vec![
                                        Operator {
                                            op: '+',
                                        },
                                    ],
                                },
                            ),
                        ],
                        operators: vec![
                            Operator {
                                op: '+',
                            },
                        ],
                    },
                    Some(
                        AlgebraicProperty {
                            atom_expr_left: AtomExpr {
                                atoms: vec![
                                    Atom::Value(
                                        'A',
                                    ),
                                    Atom::Value(
                                        'B',
                                    ),
                                ],
                                operators: vec![
                                    Operator {
                                        op: '+',
                                    },
                                ],
                            },
                            atom_expr_right: AtomExpr {
                                atoms: vec![
                                    Atom::Value(
                                        'B',
                                    ),
                                    Atom::Value(
                                        'A',
                                    ),
                                ],
                                operators: vec![
                                    Operator {
                                        op: '+',
                                    },
                                ],
                            },
                        },
                    ),
                ),
                (
                    AtomExpr {
                        atoms: vec![
                            Atom::Value(
                                'Z',
                            ),
                            Atom::Parenthesized(
                                AtomExpr {
                                    atoms: vec![
                                        Atom::Value(
                                            'Y',
                                        ),
                                        Atom::Value(
                                            'X',
                                        ),
                                    ],
                                    operators: vec![
                                        Operator {
                                            op: '+',
                                        },
                                    ],
                                },
                            ),
                        ],
                        operators: vec![
                            Operator {
                                op: '+',
                            },
                        ],
                    },
                    Some(
                        AlgebraicProperty {
                            atom_expr_left: AtomExpr {
                                atoms: vec![
                                    Atom::Value(
                                        'A',
                                    ),
                                    Atom::Value(
                                        'B',
                                    ),
                                ],
                                operators: vec![
                                    Operator {
                                        op: '+',
                                    },
                                ],
                            },
                            atom_expr_right: AtomExpr {
                                atoms: vec![
                                    Atom::Value(
                                        'B',
                                    ),
                                    Atom::Value(
                                        'A',
                                    ),
                                ],
                                operators: vec![
                                    Operator {
                                        op: '+',
                                    },
                                ],
                            },
                        },
                    ),
                ),
                (
                    AtomExpr {
                        atoms: vec![
                            Atom::Parenthesized(
                                AtomExpr {
                                    atoms: vec![
                                        Atom::Value(
                                            'Z',
                                        ),
                                        Atom::Value(
                                            'Y',
                                        ),
                                    ],
                                    operators: vec![
                                        Operator {
                                            op: '+',
                                        },
                                    ],
                                },
                            ),
                            Atom::Value(
                                'X',
                            ),
                        ],
                        operators: vec![
                            Operator {
                                op: '+',
                            },
                        ],
                    },
                    Some(
                        AlgebraicProperty {
                            atom_expr_left: AtomExpr {
                                atoms: vec![
                                    Atom::Parenthesized(
                                        AtomExpr {
                                            atoms: vec![
                                                Atom::Value(
                                                    'A',
                                                ),
                                                Atom::Value(
                                                    'B',
                                                ),
                                            ],
                                            operators: vec![
                                                Operator {
                                                    op: '+',
                                                },
                                            ],
                                        },
                                    ),
                                    Atom::Value(
                                        'C',
                                    ),
                                ],
                                operators: vec![
                                    Operator {
                                        op: '+',
                                    },
                                ],
                            },
                            atom_expr_right: AtomExpr {
                                atoms: vec![
                                    Atom::Value(
                                        'A',
                                    ),
                                    Atom::Parenthesized(
                                        AtomExpr {
                                            atoms: vec![
                                                Atom::Value(
                                                    'B',
                                                ),
                                                Atom::Value(
                                                    'C',
                                                ),
                                            ],
                                            operators: vec![
                                                Operator {
                                                    op: '+',
                                                },
                                            ],
                                        },
                                    ),
                                ],
                                operators: vec![
                                    Operator {
                                        op: '+',
                                    },
                                ],
                            },
                        },
                    ),
                ),
                (
                    AtomExpr {
                        atoms: vec![
                            Atom::Value(
                                'Z',
                            ),
                            Atom::Value(
                                'Y',
                            ),
                            Atom::Value(
                                'X',
                            ),
                        ],
                        operators: vec![
                            Operator {
                                op: '+',
                            },
                            Operator {
                                op: '+',
                            },
                        ],
                    },
                    Some(
                        AlgebraicProperty {
                            atom_expr_left: AtomExpr {
                                atoms: vec![
                                    Atom::Value(
                                        'A',
                                    ),
                                    Atom::Value(
                                        'B',
                                    ),
                                    Atom::Extension,
                                ],
                                operators: vec![
                                    Operator {
                                        op: '+',
                                    },
                                    Operator {
                                        op: '+',
                                    },
                                ],
                            },
                            atom_expr_right: AtomExpr {
                                atoms: vec![
                                    Atom::Parenthesized(
                                        AtomExpr {
                                            atoms: vec![
                                                Atom::Value(
                                                    'A',
                                                ),
                                                Atom::Value(
                                                    'B',
                                                ),
                                            ],
                                            operators: vec![
                                                Operator {
                                                    op: '+',
                                                },
                                            ],
                                        },
                                    ),
                                    Atom::Extension,
                                ],
                                operators: vec![
                                    Operator {
                                        op: '+',
                                    },
                                ],
                            },
                        },
                    ),
                ),
            ],
        )
    );
}