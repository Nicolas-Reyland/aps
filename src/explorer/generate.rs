use crate::{clothing::dress_up_expr, explorer::*, parser::*};

/// Generate new expression using scheme and mappings
/// mappings matches atoms (only symbols) from the source to the ones in the value-expression
pub fn generate_atom(
    scheme: &Atom,
    mappings: &Atom2AtomHashMap,
    associativities: &AssociativityHashMap,
) -> Atom {
    match scheme {
        Atom::Parenthesized(expr) => generate_expression(expr, mappings, associativities),
        Atom::Value(_) => scheme.clone(),
        Atom::Symbol(_) => match mappings.get(scheme) {
            Some(gen_atom) => gen_atom.clone(),
            None => panic!(
                "Could not find mapping for scheme {} :\nMappings :\n{:#?}\n",
                scheme, mappings
            ),
        },
        Atom::FunctionCall(fn_call) => {
            let new_args = fn_call
                .args
                .iter()
                .map(|arg| generate_atom(arg, mappings, associativities))
                .collect();
            Atom::FunctionCall(FunctionCallExpr {
                name: fn_call.name.clone(),
                args: new_args,
            })
        }
        Atom::Sequential(seq) => generate_sequential(seq, mappings, associativities),
    }
}

fn generate_expression(
    scheme_expr: &AtomExpr,
    mappings: &Atom2AtomHashMap,
    associativities: &AssociativityHashMap,
) -> Atom {
    // init new atoms and operator
    let mut atoms: Vec<Atom> = Vec::new();
    // match each atom of the v-expr
    let num_scheme_atoms = scheme_expr.atoms.len();
    let mut i = 0;
    // println!("Mappings:\n{:#?}\n", mappings);
    while i < num_scheme_atoms {
        let scheme = &scheme_expr.atoms[i];
        // normal mapping
        atoms.push(generate_atom(scheme, mappings, associativities));
        // this will surely change ...
        i += 1;
    }

    parenthesized_atom(AtomExpr {
        atoms,
        operator: scheme_expr.operator.clone(),
    })
}

fn generate_sequential(
    seq: &SequentialExpr,
    mappings: &Atom2AtomHashMap,
    associativities: &AssociativityHashMap,
) -> Atom {
    let mapped_enumerator = generate_atom(&seq.enumerator.clone(), mappings, associativities);
    match mapped_enumerator.clone() {
        Atom::Value(n) if n != 0 => {
            let n_size = n as usize;
            let mut atoms: Vec<Atom> = Vec::with_capacity(n_size);
            atoms.push(generate_atom(&seq.body.clone(), mappings, associativities));
            dress_up_expr(
                &parenthesized_atom(AtomExpr {
                    atoms: atoms
                        .into_iter()
                        .cycle()
                        .take(n_size)
                        .collect::<Vec<Atom>>(),
                    operator: if n == 1 {
                        None
                    } else {
                        Some(seq.operator.clone())
                    },
                }),
                associativities,
            )
        }
        // Non-numeric enumerator: leave as-is (non-expandable) to avoid generating invalid sequences
        _ => Atom::Sequential(Box::new(SequentialExpr {
            operator: seq.operator.clone(),
            enumerator: mapped_enumerator,
            body: generate_atom(&seq.body.clone(), mappings, associativities),
        })),
    }
}
