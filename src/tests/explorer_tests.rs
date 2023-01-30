#[cfg(test)]
use crate::explorer::*;
use crate::parser::*;

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
    let expr_b = match atom_expr_p::<ApsParserKind>("X + ((A * B) * C) + D") {
        Ok(("", expr)) => expr,
        Ok((rest, parsed)) => panic!(
            "Failed to parse everything:\n'{}'\nParsed :\n{:#?}\n",
            rest, parsed,
        ),
        Err(err) => panic!("Failed to parse expression:\n{:#?}", err),
    };
    println!("expr_a = {}", expr_a);
    println!("strip(expr_a) = {}", strip_expr_naked(&expr_a));
    println!("expr_b = {}", expr_b);
    println!("strip(expr_b) = {}", strip_expr_naked(&expr_b));
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
    println!("expr_a = {}", expr_a);
    println!("dress_up(expr_a) = {}", dress_up_expr(&expr_a));
    println!("expr_b = {}", expr_b);
    println!("dress_up(expr_b) = {}", dress_up_expr(&expr_b));
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
    let property =
        match root::<ApsParserKind>("+ :: { (A + B) + C = A + (B + C) ; }") {
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