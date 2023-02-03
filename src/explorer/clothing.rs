use crate::explorer::atom2atom_expr;
use crate::parser::OperatorAssociativity::*;
use crate::parser::*;

/// Strips the expression naked.
/// Removes parentheses for easier and faster comparison.
/// Operator associativity (left, right, none) is taken into account.
///
/// Here is an example of what is meant :
/// "((A + B) + C) + D" => "A + B + C + D"
/// Of course, this only works for left-associative operators.
/// The associativity of operators can be specified in the grammar :
/// "^ r :: { ... }" makes the ^ operator right-associative.
/// There are 3 choices : l, r, n (left-, right-, non- associative)
/// If no associativity is gen (e.g. "+ :: { ... }", the default is left-associativity)
/// By default, all operators are considered to be left-associative.
///
/// Here is another example: "A + (B + C)" => "A + (B + C)"
/// We cannot remove parentheses, since '+' is not right-associative
pub fn strip_expr_naked(atom: &Atom, associativities: &AssociativityHashMap) -> Atom {
    let expr = match atom {
        Atom::Parenthesized(e) => e,
        _ => return map_over_all_sub_expressions(atom, associativities, &strip_expr_naked),
    };
    // left-associative operator case (default)
    let operator = expr.operator.clone();
    // "((A + B) + C) + D" => "(A + B) + C + D" => "A + B + C + D"
    let associativity = get_operator_associativity(&operator, associativities);

    if associativity == NonAssociative {
        // only strip all the sub-expressions naked, do nothing with the current expression
        return parenthesized_atom(AtomExpr {
            atoms: expr
                .atoms
                .iter()
                .map(|atom| map_over_all_sub_expressions(atom, associativities, &strip_expr_naked))
                .collect(),
            operator: expr.operator.clone(),
        });
    }
    if associativity == LeftRightAssociative {
        return strip_expr_naked_left_right(expr, associativities);
    }

    let left_ass = associativity == LeftAssociative;
    // merge the atoms from an atom-expression (isolated) and the current expression
    // except for the isolated one, sub-expressions are NOT yet stripped naked themselves
    let new_atoms = match if left_ass {
        expr.atoms.split_first()
    } else {
        expr.atoms.split_last()
    } {
        Some((isolated, rest)) => match isolated.clone() {
            // the isolated atom is an expression, so we should remove the parentheses
            Atom::Parenthesized(mut isolated_expr) if expr.operator == isolated_expr.operator => {
                // make the isolated expression naked
                isolated_expr = atom2atom_expr(strip_expr_naked(
                    &parenthesized_atom(isolated_expr),
                    associativities,
                ));
                // merge the isolated and rest into new expression
                let mut isolated_expr_atoms = Vec::new();
                if left_ass {
                    // isolated is left, and rest is right
                    isolated_expr_atoms.append(&mut isolated_expr.atoms);
                    isolated_expr_atoms.extend(rest.to_vec());
                } else {
                    // isolated is right, and rest is left
                    isolated_expr_atoms.extend(rest.to_vec());
                    isolated_expr_atoms.append(&mut isolated_expr.atoms);
                }
                isolated_expr_atoms
            }
            // the isolated atom is not an AtomExpr (should not merge it with the others)
            _ => expr.atoms.clone(),
        },
        // wtf ??
        None => expr.atoms.clone(),
    };

    parenthesized_atom(AtomExpr {
        atoms: new_atoms
            .iter()
            .map(|atom| map_over_all_sub_expressions(atom, associativities, &strip_expr_naked))
            .collect(),
        operator: expr.operator.clone(),
    })
}

fn strip_expr_naked_left_right(expr: &AtomExpr, associativities: &AssociativityHashMap) -> Atom {
    let mut new_atoms: Vec<Atom> = Vec::with_capacity(expr.atoms.len());
    for atom in expr.atoms.clone() {
        match atom.clone() {
            Atom::Parenthesized(atom_expr) => {
                if atom_expr.operator == expr.operator {
                    // merge this expression
                    match strip_expr_naked_left_right(&atom_expr, associativities) {
                        Atom::Parenthesized(mut new_atom_expr) => {
                            new_atoms.append(&mut new_atom_expr.atoms)
                        }
                        new_atom => new_atoms.push(new_atom),
                    }
                } else {
                    // add it as-is (not to be merged)
                    new_atoms.push(atom);
                }
            }
            _ => new_atoms.push(atom),
        }
    }

    parenthesized_atom(AtomExpr {
        atoms: new_atoms
            .iter()
            .map(|atom| map_over_all_sub_expressions(atom, associativities, &strip_expr_naked))
            .collect(),
        operator: expr.operator.clone(),
    })
}

/// Dresses the expression up.
/// Adds parentheses for easier and faster property matching.
/// Operator associativity (left, right, none) is taken into account.
///
/// Here is an example of what is meant :
/// "A + B + C + D" => "((A + B) + C) + D"
/// Of course, this only works for left-associative operators.
/// The associativity of operators can be specified in the grammar :
/// "^ r :: { ... }" makes the ^ operator right-associative.
/// There are 3 choices : l, r, n (left-, right-, non- associative)
/// If no associativity is gen (e.g. "+ :: { ... }", the default is left-associativity)
/// By default, all operators are considered to be left-associative.
pub fn dress_up_expr(atom: &Atom, associativities: &AssociativityHashMap) -> Atom {
    let expr = match atom {
        Atom::Parenthesized(e) => e,
        _ => return map_over_all_sub_expressions(atom, associativities, &dress_up_expr),
    };
    // left-associative operator case (default)
    let operator = expr.operator.clone();
    // "A + B + C + D" => "(A + B + C) + D" => "((A + B) + C) + D"
    let associativity = match get_operator_associativity(&operator, associativities) {
        // It is both left and right associative, so we know that :
        // A + B + C = A + (B + C) = (A + B) + C
        LeftRightAssociative => LeftAssociative,
        ass => ass,
    };

    if associativity == NonAssociative {
        // only dress up the sub-expressions naked, do nothing with the current expression
        return parenthesized_atom(AtomExpr {
            atoms: expr
                .atoms
                .iter()
                .map(|atom| map_over_all_sub_expressions(atom, associativities, &dress_up_expr))
                .collect(),
            operator: expr.operator.clone(),
        });
    }

    let left_ass = associativity == LeftAssociative;
    // group all the atoms, except for one (isolated) into a new expression
    // sub-expressions are NOT yet dressed up themselves, not even the isolated one
    let new_atoms: Vec<Atom> = match if left_ass {
        expr.atoms.split_last()
    } else {
        expr.atoms.split_first()
    } {
        Some((isolated, rest)) => {
            let num_left = rest.len();
            if num_left == 0 {
                vec![isolated.clone()]
            } else {
                let rest_expr = if num_left == 1 {
                    rest[0].clone()
                } else {
                    parenthesized_atom(AtomExpr {
                        atoms: rest.to_vec(),
                        operator: operator.clone(),
                    })
                };
                if left_ass {
                    vec![rest_expr, isolated.clone()]
                } else {
                    vec![isolated.clone(), rest_expr]
                }
            }
        }
        // wtf ??
        None => return (*atom).clone(),
    };
    parenthesized_atom(AtomExpr {
        atoms: new_atoms
            .iter()
            .map(|atom| map_over_all_sub_expressions(atom, associativities, &dress_up_expr))
            .collect(),
        operator: expr.operator.clone(),
    })
}

/// Returns the associativity of the operator, based on the given vector of operators
/// If operator is None, it is seen as non-associative. The default associativity for an
/// operator is the left-associativity.
fn get_operator_associativity(
    operator: &Option<Operator>,
    associativities: &AssociativityHashMap,
) -> OperatorAssociativity {
    if *operator == None {
        NonAssociative
    } else {
        let operator_char = operator.clone().unwrap().op;
        match associativities.get(&operator_char) {
            Some(associativity) if *associativity != Unknown => *associativity,
            _ => LeftAssociative,
        }
    }
}

/// Recursively maps f over all sub expressions in atoms.
/// The returned AtomExpr struct is constructed with the given operator
/// The operators argument is used in the calls to f
fn map_over_all_sub_expressions(
    atom: &Atom,
    associativities: &AssociativityHashMap,
    f: &dyn Fn(&Atom, &AssociativityHashMap) -> Atom,
) -> Atom {
    match atom {
        Atom::Parenthesized(_) => f(&atom, associativities),
        Atom::FunctionCall(fn_call) => Atom::FunctionCall(FunctionCallExpr {
            name: fn_call.name.to_string(),
            // map each argument with f
            args: fn_call
                .args
                .iter()
                .map(|arg| f(arg, associativities))
                .collect(),
        }),
        Atom::Sequential(seq) => Atom::Sequential(Box::new(SequentialExpr {
            operator: seq.operator.clone(),
            enumerator: map_over_all_sub_expressions(&seq.enumerator, associativities, f),
            body: map_over_all_sub_expressions(&seq.body, associativities, f),
        })),
        _ => (*atom).clone(),
    }
}
