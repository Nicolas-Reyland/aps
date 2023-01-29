// APS Repl

use crate::{
    parser::{
        self,
        AtomExpr,
        AlgebraicFunction,
        AlgebraicProperty,
        KProperty,
        split_algebraic_objects,
        Atom,
    },
    solution::solve_equality,
    explorer::{init_graph, explore_graph, print_graph_dot_format, atom2atom_expr}, preprocessor::read_and_preprocess_file
};
use reedline_repl_rs::{
    Repl,
    Result,
    clap::{
        Command,
        ArgMatches,
        Arg,
        parser::ValuesRef,
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
        .with_name("Algebraic Proof System Language ")
        .with_version("v0.1.0")
        .with_description("Prove algebraic statements using algebraic structures definitions")
        .with_banner("REPL for the Algebraic Proof System Language")
        .with_command(
            Command::new("prove")
                .arg(Arg::new("property")
                    .required(true)
                    .num_args(1..)
                )
                .about("Prove a property using the current context"),
            prove_callback
        )
        .with_command(
            Command::new("import")
            .arg(Arg::new("files")
                .required(true)
                .num_args(1..)
            )
            .about("Import rules from a list of files"),
            import_callback
        )
        .with_command(
            Command::new("graph")
            .arg(Arg::new("num explorations")
                .required(true)
            )
            .arg(Arg::new("expression")
                .required(true)
                .num_args(1..)
            )
            .about("Explore a graph a number of times and print it in dot format"),
            graph_callback
        )
        .with_command(
            Command::new("def")
            .arg(Arg::new("body")
                .required(true)
                .num_args(1..)
                .allow_hyphen_values(true)
            )
            .about("Add a definition to the current context"),
            rule_callback
        )
        .with_command(
            Command::new("ctx")
            .arg(Arg::new("break-status")
                .required(false)
                .num_args(0..)
            )
            .about("Print the current context, or set the break-status (auto/no-break)"),
            ctx_callback
        );
    repl.run().unwrap();
}

fn ctx_callback(args: ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    // look for sub-command
    let break_status_id: String = "break-status".to_string();
    let break_status_change = args.contains_id(&break_status_id);
    if break_status_change {
        let break_status = args.get_one::<String>(&break_status_id).unwrap();
        let auto_break = match break_status.as_str() {
            "auto-break" => true,
            "no-break" => false,
            s => return Ok(Some(format!(" Break-status '{}' is not valid ('auto-break' or 'no-break')", s))),
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
    let body_str = concat_args(args.get_many("body").unwrap());
    let (
        mut properties,
        mut functions,
        mut k_properties,
    ) = split_algebraic_objects(
        match parser::root::<parser::ApsParserKind>(&body_str) {
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
    let expression_str = concat_args(args.get_many("expression").unwrap());
    let expression = str2atom_expr(&expression_str);
    let mut graph = init_graph(expression);
    // number of explorations
    let depth = args.get_one::<& str>("num explorations").unwrap().parse::<u8>()?;
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
    for filename in args.get_many::<String>("files").unwrap() {
        if import_into_context(context, filename) {
            report.push_str(&format!(" imported '{}'\n", filename));
        } else {
            report.push_str(&format!(" failed to import '{}'\n", filename));
        }
    }
    Ok(Some(report))
}

fn prove_callback(args: ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    // concat args
    let mut property_str = concat_args(args.get_many("property").unwrap());
    // add ';;' suffix, if needed
    if ! property_str.ends_with(";; ") {
        property_str.push_str(";;");
    }
    // parse the expression
    let property = match parser::property_p::<parser::ApsParserKind>(&property_str) {
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

pub fn import_into_context(context: &mut ReplContext, filename: &str) -> bool {
    // read file
    let content = match read_and_preprocess_file(filename, None) {
        Some(s) => s,
        None => return false,
    };
    let content_box = Box::new(content);
    // parse input context
    let alg_objects = match parser::root::<parser::ApsParserKind>(
        &content_box
    ) {
        Ok(("", algebraic_objects)) => algebraic_objects,
        Ok((rest, algebraic_objects)) => panic!(
            " Failed to parse everything:\nParsed (root) :\n{:#?}\n'{}'\n",
            algebraic_objects,
            rest,
        ),
        Err(err) => panic!("Failed to parse expression:\n{:#?}", err)
    };
    // split objects
    let (
        mut properties,
        mut functions,
        mut k_properties
    ) = split_algebraic_objects(alg_objects);
    // add functions as properties
    context.properties.extend(
        functions.iter().map(
            |function| AlgebraicProperty {
                atom_expr_left: atom2atom_expr(
                    Atom::FunctionCall((
                        function.name.clone(),
                        function.atom_expr_left.clone()
                    ))
                ),
                atom_expr_right: function.atom_expr_right.clone(),
            }
        )
    );
    // extend context
    context.properties.append(&mut properties);
    context.functions.append(&mut functions);
    context.k_properties.append(&mut k_properties);
    true
}

fn concat_args(args: ValuesRef<String>) -> String {
    // concat args
    let mut property_str = String::new();
    for value in args {
        property_str.push_str(value);
    }
    property_str
}

pub fn str2atom_expr(input: &str) -> AtomExpr {
    match parser::atom_expr_p::<parser::ApsParserKind>(
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
