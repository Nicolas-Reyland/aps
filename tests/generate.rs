use apsl_lang::explorer::Atom2AtomHashMap;
#[cfg(test)]
use apsl_lang::generate::*;
use apsl_lang::parser::AssociativityHashMap;
use apsl_lang::parser::OperatorAssociativity::*;
use apsl_lang::repl::str2atom;
use std::collections::HashMap;

fn default_associativities() -> AssociativityHashMap {
    let mut hash_map: AssociativityHashMap = HashMap::with_capacity(5);
    for (k, v) in vec![
        ('+', Unknown),
        ('*', LeftAssociative),
        ('^', RightAssociative),
        ('@', LeftRightAssociative),
        ('-', NonAssociative),
    ] {
        hash_map.insert(k, v);
    }
    hash_map
}

macro_rules! assert_eq_generation {
    ($scheme:expr, $expected:expr, $pairs:expr, $associativities:expr) => {
        let scheme_atom = str2atom($scheme);
        let expected_atom = str2atom($expected);
        let mappings: Atom2AtomHashMap = $pairs
            .iter()
            .map(|(k, v)| (str2atom(k), str2atom(v)))
            .collect();
        let actual_atom = generate_atom(
            &scheme_atom,
            &mappings,
            $associativities,
        );
        assert_eq!(
            actual_atom,
            expected_atom,
            "Assertion of generation equality failed:\n{} = gen(\"{}\") != \"{}\".\nLeft (actual): {:#?}\nRight (expected): {:#?}\nMappings: {:#?}",
            actual_atom,
            scheme_atom,
            expected_atom,
            actual_atom,
            scheme_atom,
            mappings
        );
    }
}

#[test]
fn generate_simple() {
    assert_eq_generation!("X", "1", vec![("X", "1"),], &default_associativities());
    assert_eq_generation!("X", "Y", vec![("X", "Y"),], &default_associativities());
}

#[test]
fn generate_expr() {
    assert_eq_generation!(
        "X + Y",
        "1 + 2",
        vec![("X", "1"), ("Y", "2"),],
        &default_associativities()
    );
    assert_eq_generation!(
        "X + (Y + 3)",
        "1 + (2 + 3)",
        vec![("X", "1"), ("Y", "2"),],
        &default_associativities()
    );
}
