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
use divrem::DivCeil;

static TAB_WIDTH: usize = 8;

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
            Command::new("settings")
            .arg(Arg::new("command")
                .required(false)
                .num_args(1)
            )
            .about("Print or set the break-status (auto/no-break, show)"),
            settings_callback
        )
        .with_command(
            Command::new("ctx")
            .arg(Arg::new("command")
                .required(false)
                .num_args(1)
            )
            .about("Print or clear the current context (show, clear)"),
            ctx_callback
        );
    repl.run().unwrap();
}

fn ctx_callback(args: ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    match args.get_one::<String>("command").unwrap_or(&"show".to_string()).as_str() {
        "clear" => {
            context.properties.clear();
            context.functions.clear();
            context.k_properties.clear();
            context.auto_break = true;
            return Ok(Some(" Cleared context\n".to_string()))
        },
        "show" | "" => (),
        command => return Ok(Some(format!(" Unknown ctx command: {} ('show' or 'clear')", command))),
    };
    let mut content = String::new();
    if context.properties.is_empty() {
        content.push_str(" No Properties\n");
    } else {
        // print the context contents
        content.push_str(" Properties :\n");
    }
    // properties
    for property in &context.properties {
        content.push_str(&format!(" | {}\n", property));
    }
    if !context.functions.is_empty() {
        // functions
        content.push_str("\n Functions :\n");
        for function in &context.functions {
            content.push_str(&format!(" | {}\n", function));
        }
    }
    if !context.k_properties.is_empty() {
        // k properties
        content.push_str("\n K Properties :\n");
        for k_property in &context.k_properties {
            content.push_str(&format!(" | {}\n", k_property));
        }
    }
    // break status
    content.push_str(&format!(
        "\n Auto break : {}\n", context.auto_break
    ));
    Ok(Some(content))
}

fn settings_callback(args: ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    match args.get_one::<String>("command").unwrap_or(&"show".to_string()).as_str() {
        "auto-break" => {
            context.auto_break = true;
            Ok(Some(" Activated auto-break.".to_string()))
        },
        "no-break" => {
            context.auto_break = false;
            Ok(Some(" Deactivated auto-break.".to_string()))
        },
        "show" => Ok(Some(String::from(match context.auto_break {
            true => " Auto-break is activated",
            false => " Auto-break is not activated",
        }))),
        command => Ok(Some(format!(" Command {} is not valid (auto/no-break, show)", command))),
    }
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
    let depth = args.get_one::<String>("num explorations").unwrap().parse::<u8>()?;
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
    Ok(solve_equality_str(property_str, context))
}

pub fn solve_equality_str(property_str: String, context: &mut ReplContext) -> Option<String> {
    // parse the expression
    let property = match parser::property_p::<parser::ApsParserKind>(&property_str) {
        Ok((_, property)) => property,
        Err(err) => {
            eprint!(" Error in parsing property: {}", err);
            return None
        }
    };
    // solve the equation
    let solution = match solve_equality(
        context.properties.clone(),
        context.functions.clone(),
        context.k_properties.clone(),
        &property.atom_expr_left,
        &property.atom_expr_right,
        context.auto_break,
    ) {
        Some(solution) => solution,
        None => return Some(format!(" No solution found for {}", property))
    };
    // build solution string
    let mut solution_str = format!(" Solution found for '{}' :\n", property);
    let (first_expr, _) = solution.first().unwrap(); // no transform for the base expr
    solution_str.push_str(&format!(
        "  {}\n", first_expr
    ));
    if solution.len() < 2 {
        return Some(solution_str);
    }
    // length of the lengthiest expression
    let mut max_expr_length: usize = 0;
    // stringify expressions and rules (setting max_expr_length at the same time)
    let solution_contents: Vec<(String, String)> = solution.iter().skip(1).map(|(expr, rule)| {
        let expr_str = expr.to_string();
        let rule_str = match rule {
            Some(r) => r.to_string(),
            None => "?".to_string(),
        };
        // update max_expr_length if needed
        let expr_length = expr_str.len();
        if expr_length > max_expr_length {
            max_expr_length = expr_length;
        }
        (expr_str, rule_str.clone())
    }).collect::<Vec<_>>();
    // max number of tabs
    let max_num_tabs = compute_num_tabs(max_expr_length);
    // Concatenating these into the solution_str
    for (expr_str, rule_str) in solution_contents {
        solution_str.push_str(
            &format!(
                " = {}{}|\t{} \n",
                expr_str,
                (0..compute_rel_num_tabs(expr_str.len(), max_num_tabs)).map(|_| "\t").collect::<String>().as_str(),
                rule_str,
            )
        );
    }
    Some(solution_str)
}

fn compute_num_tabs(str_len: usize) -> usize {
    // + 3: " = " prefix for each expression
    // + 1: if an expression ends exactly at the end of a tab,
    //      an extra tab would be added
    DivCeil::div_ceil(str_len + 3 + 1, TAB_WIDTH)
}

fn compute_rel_num_tabs(expr_length: usize, max_num_tabs: usize) -> usize {
    let num_tabs = compute_num_tabs(expr_length);
    // + 1: at least one tab
    return 1 + max_num_tabs - num_tabs;
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
