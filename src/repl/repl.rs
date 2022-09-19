// APS Repl

use crate::{
    aps_parser::{
        self,
        AtomExpr,
        AlgebraicFunction,
        AlgebraicProperty,
        KProperty,
        split_algebraic_objects
    },
    solution::solve_equality, explorer::{init_graph, explore_graph, print_graph_dot_format}
};
use std::fs;
use reedline_repl_rs::{
    Repl,
    Result,
    clap::{
        Command,
        ArgMatches,
        Arg, Values
    }
};

#[derive(Default, Clone)]
pub struct ReplContext {
    pub properties: Vec<AlgebraicProperty>,
    pub functions: Vec<AlgebraicFunction>,
    pub k_properties: Vec<KProperty>,
    pub auto_break: bool,
}

pub fn init_context() -> ReplContext {
    ReplContext {
        properties: Vec::new(),
        functions: Vec::new(),
        k_properties: Vec::new(),
        auto_break: true,
    }
}

pub fn repl(context: ReplContext) {
    let mut repl = Repl::new(context)
        .with_name("Algebraic Proofing System ")
        .with_version("v0.1.0")
        .with_description("Prove algebraic statements using a set of predefined rules")
        .with_banner("REPL for the Algebraic Proofing System Language")
        .with_command(
            Command::new("prove")
                .arg(Arg::with_name("property")
                    .required(true)
                    .min_values(1)
                )
                .about("Prove a property using the current context"),
            prove_callback
        )
        .with_command(
            Command::new("import")
            .arg(Arg::with_name("files")
                .required(true)
                .min_values(1)
            )
            .about("Import rules from a list of files"),
            import_callback
        )
        .with_command(
            Command::new("graph")
            .arg(Arg::with_name("num explorations")
                .required(true)
            )
            .arg(Arg::with_name("expression")
                .required(true)
                .min_values(1)
            )
            .about("Explore a graph a number of times and print it in dot format"),
            graph_callback
        )
        .with_command(
            Command::new("def")
            .arg(Arg::with_name("body")
                .required(true)
                .min_values(1)
                .allow_hyphen_values(true)
            )
            .about("Add a definition to the current context"),
            rule_callback
        )
        .with_command(
            Command::new("ctx")
            .arg(Arg::with_name("break-status")
                .required(false)
                .takes_value(false)
            )
            .about("Print the current context"),
            ctx_callback
        );
    repl.run().unwrap();
}

fn ctx_callback(args: ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    // look for sub-command
    let break_status_id: &'static str = "break-status";
    let break_status_change = args.is_present(break_status_id);
    if break_status_change {
        let auto_break = match args.value_of(break_status_id).unwrap() {
            "auto-break" => true,
            "no-break" => false,
            s => return Ok(Some(format!(" Break-status '{}' is not valid", s)))
        };
        if auto_break {
            // activating auto-break
            context.auto_break = true;
            return Ok(Some(" Activated auto-break.".to_string()))
        }
        // deactivating auto-break (no-break)
        context.auto_break = false;
        return Ok(Some(" Deactivated auto-break.".to_string()))
    }
    // print the context contents
    let mut content = " Properties :\n".to_owned();
    // properties
    for property in &context.properties {
        content.push_str(&format!(" | {}\n", property));
    }
    // functions
    content.push_str("\n Functions :\n");
    for function in &context.functions {
        content.push_str(&format!(" | {}\n", function));
    }
    // k properties
    content.push_str("\n K Properties :\n");
    for k_property in &context.k_properties {
        content.push_str(&format!(" | {}\n", k_property));
    }
    // break status
    content.push_str(&format!(
        "\n Auto break : {}\n", context.auto_break
    ));
    Ok(Some(content))
}

fn rule_callback(args: ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    let body_str = concat_args(args.values_of("body").unwrap());
    let (
        mut properties,
        mut functions,
        mut k_properties,
    ) = split_algebraic_objects(
        match aps_parser::root::<aps_parser::ApsParserKind>(&body_str) {
            Ok(("", objects)) => objects,
            Ok((rest, _)) => return Ok(Some(format!(" Error: Could not parse '{}'", rest))),
            Err(err) => return Ok(Some(format!(" Error occured while parsing :{}\n", err))),
        }
    );
    // extend context
    context.properties.append(&mut properties);
    context.functions.append(&mut functions);
    context.k_properties.append(&mut k_properties); 
    Ok(Some(" Added object(s) to context.".to_owned()))
}

fn graph_callback(args: ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    // concat args of expression
    let expression_str = concat_args(args.values_of("expression").unwrap());
    let expression = str2atom_expr(&expression_str);
    let mut graph = init_graph(expression);
    // number of explorations
    let depth = args.value_of("num explorations").unwrap().parse::<u8>()?;
    for _ in 0..depth {
        if ! explore_graph(&mut graph, &context.properties, &context.functions) {
            break;
        }
    }
    Ok(Some(print_graph_dot_format(&graph)))
}

fn import_callback(args: ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    // import all the files
    let mut report = "".to_owned();
    for filename in args.values_of("files").unwrap() {
        import_into_context(context, filename);
        report.push_str(&format!(" imported '{}'\n", filename));
    }
    Ok(Some(report))
}

fn prove_callback(args: ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    // concat args
    let mut property_str = concat_args(args.values_of("property").unwrap());
    // add ';;' suffix, if needed
    if ! property_str.ends_with(";; ") {
        property_str.push_str(";;");
    }
    // parse the expression
    let property = match aps_parser::property_p::<aps_parser::ApsParserKind>(&property_str) {
        Ok((_, property)) => property,
        Err(err) => {
            eprint!(" Error in parsing property: {}", err);
            return Ok(None)
        }
    };
    let solution = match solve_equality(
        context.properties.clone(),
        context.functions.clone(),
        context.k_properties.clone(),
        &property.atom_expr_left,
        &property.atom_expr_right,
        context.auto_break,
    ) {
        Some(solution) => solution,
        None => return Ok(Some(format!(" No solution found for {}", property)))
    };
    // build solution string
    let mut solution_str = format!(" Solution found for '{}' :\n", property);
    let (first_expr, _) = solution.first().unwrap(); // no transform for the base expr
    solution_str.push_str(&format!(
        "  {}\n", first_expr
    ));
    for (step_expr, step_tr) in solution.iter().skip(1) {
        solution_str.push_str(
            &format!(
                " = {}\t\t|\t{}\n",
                step_expr,
                match step_tr {
                    Some(tr) => tr.to_string(),
                    None => "?".to_string()
                }
            )
        );
    }
    Ok(Some(solution_str))
}

pub fn import_into_context(context: &mut ReplContext, filename: &str) {
    // read file
    let content = fs::read_to_string(filename).unwrap();
    let content_box: Box<String> = Box::new(content);
    // parse input context
    let alg_objects = match aps_parser::root::<aps_parser::ApsParserKind>(
        &content_box
    ) {
        Ok(("", algebraic_objects)) => algebraic_objects,
        Ok((rest, algebraic_objects)) => panic!(
            " Failed to parse everything:\n'{}'\nParsed (root) :\n{:#?}\n",
            rest,
            algebraic_objects
        ),
        Err(err) => panic!("Failed to parse expression:\n{:#?}", err)
    };
    // split objects
    let (
        mut properties,
        mut functions,
        mut k_properties
    ) = split_algebraic_objects(alg_objects);
    // extend context
    context.properties.append(&mut properties);
    context.functions.append(&mut functions);
    context.k_properties.append(&mut k_properties);
}

fn concat_args(args: Values) -> String {
    // concat args
    let mut property_str = String::new();
    for value in args {
        property_str.push_str(value);
        //property_str.push_str(" ");
    }
    property_str
}

fn str2atom_expr(input: &str) -> AtomExpr {
    match aps_parser::atom_expr_p::<aps_parser::ApsParserKind>(
        input
    ) {
        Ok(("", expr)) => expr,
        Ok((rest, parsed)) => panic!(
            " Failed to parse whole expression:\n'{}'\nParsed (expr) :\n{:#?}\n",
            rest,
            parsed
        ),
        Err(err) => panic!(" Failed to parse expression:\n{:#?}", err)
    }
}
