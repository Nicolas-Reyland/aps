// APS Repl

use crate::parser::ApsParserKind;
use crate::{
    clothing::strip_expr_naked,
    explorer::{explore_graph, init_graph, print_graph_dot_format},
    parser::format_toplevel_atom,
    parser::{
        self, split_algebraic_objects, AlgebraicFunction, AlgebraicProperty, AssociativityHashMap,
        Atom, FunctionCallExpr, KProperty,
    },
    preprocessor::read_and_preprocess_file,
    solution::solve_equality,
};
use divrem::DivCeil;
use reedline_repl_rs::Error::UnknownCommand;
use reedline_repl_rs::{
    clap::{parser::ValuesRef, Arg, ArgMatches, Command},
    Repl, Result,
};
use std::collections::{HashMap, HashSet};
use crate::preprocessor::{process_macros, remove_comments};

static TAB_WIDTH: usize = 8;

#[derive(Default, Clone)]
pub struct ReplContext {
    pub properties: HashSet<AlgebraicProperty>,
    pub functions: HashSet<AlgebraicFunction>,
    pub k_properties: HashSet<KProperty>,
    pub associativities: AssociativityHashMap,
    pub auto_break: bool,
    pub pretty_print_steps: bool,
    pub route_poc: bool,
}

pub fn init_context() -> ReplContext {
    ReplContext {
        properties: HashSet::new(),
        functions: HashSet::new(),
        k_properties: HashSet::new(),
        associativities: HashMap::new(),
        auto_break: true,
        pretty_print_steps: true,
        route_poc: false,
    }
}

fn reset_context(context: &mut ReplContext) {
    context.properties.clear();
    context.functions.clear();
    context.k_properties.clear();
    context.associativities.clear();
    context.auto_break = true;
    context.pretty_print_steps = true;
    context.route_poc = false;
}

// used in settings_callback
macro_rules! handle_setting {
    ($switch_var:expr, $action:ident, $param:ident) => {
        match $action {
            "on" => {
                $switch_var = true;
                Ok(Some(format!(" Activated {}.", $param)))
            }
            "off" => {
                $switch_var = false;
                Ok(Some(format!(" Deactivated {}.", $param)))
            }
            "show" => Ok(Some(format!(
                " {} {}",
                $param,
                match $switch_var {
                    true => "is activated",
                    false => "is not activated",
                },
            ))),
            _ => Ok(Some(
                " usage: settings (auto-break/expr-pretty-print) (on/off/show)".to_string(),
            )),
        }
    };
}

pub fn repl(context: ReplContext) {
    let mut repl = Repl::new(context)
        .with_name("APSL ")
        .with_version("v0.2.0")
        .with_description("Prove algebraic statements in algebraic structures")
        .with_banner("REPL for the Algebraic Proof System Language")
        .with_partial_completions(true)
        .with_command(
            Command::new("prove")
                .arg(Arg::new("property").required(true).num_args(1..))
                .about("Prove a property using the current context"),
            prove_callback,
        )
        .with_command(
            Command::new("import")
                .arg(Arg::new("files").required(true).num_args(1..))
                .about("Import rules from a list of files"),
            import_callback,
        )
        .with_command(
            Command::new("graph")
                .arg(Arg::new("num explorations").required(true))
                .arg(Arg::new("expression").required(true).num_args(1..))
                .about("Explore a graph a number of times and print it in dot format"),
            graph_callback,
        )
        .with_command(
            Command::new("def")
                .arg(
                    Arg::new("body")
                        .required(true)
                        .num_args(1..)
                        .allow_hyphen_values(true),
                )
                .about("Add a definition to the current context"),
            rule_callback,
        )
        .with_command(
            Command::new("settings")
                .arg(Arg::new("param").required(true).num_args(1))
                .arg(Arg::new("action").required(false).num_args(1))
                .about(
                    "Print or set the auto-break, route-poc and expr-pretty-print (on/off/show)",
                ),
            settings_callback,
        )
        .with_command(
            Command::new("ctx")
                .subcommand(
                    Command::new("add")
                        .arg(Arg::new("property").required(true).num_args(1..))
                        .about(
                            "Add a property to the current context (see 'def' for better options)",
                        ),
                )
                .subcommand(
                    Command::new("rem")
                        .arg(Arg::new("property").required(true).num_args(1..))
                        .about("Remove a property from the current context"),
                )
                .subcommand(Command::new("show").about("Show the current context"))
                .subcommand(
                    Command::new("clear")
                        .about("Clear the current context (removes everything + resets settings)"),
                )
                .about("Change or print the current context"),
            ctx_callback,
        );
    repl.run().unwrap();
}

fn ctx_callback(args: ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    match args.subcommand() {
        Some(("add", sub_args)) => ctx_add_callback(sub_args, context),
        Some(("rem", sub_args)) => ctx_rem_callback(sub_args, context),
        Some(("clear", _)) => ctx_clear_callback(context),
        _ => ctx_show_callback(context),
    }
}

fn ctx_clear_callback(context: &mut ReplContext) -> Result<Option<String>> {
    reset_context(context);
    Ok(Some(" Cleared context\n".to_string()))
}

fn ctx_add_callback(args: &ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    let mut property_str = concat_args(args.get_many("property").unwrap());
    property_str.push_str(";");
    let property = match parser::property_p::<ApsParserKind>(&property_str) {
        Ok(("", p)) => p,
        Ok((r, _)) => {
            return Ok(Some(format!(
                " Failed to parse everything as a property: \"{}\"",
                r
            )))
        }
        Err(_) => {
            return Ok(Some(format!(
                " Failed to parse \"{}\" as a property",
                property_str
            )))
        }
    };
    match context.properties.insert(property.clone()) {
        true => Ok(Some(format!(" Added {} to the properties", property))),
        false => Ok(Some(format!(" Property {} was already defined", property))),
    }
}

fn ctx_rem_callback(args: &ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    let mut property_str = concat_args(args.get_many("property").unwrap());
    property_str.push_str(";");
    let property = match parser::property_p::<ApsParserKind>(&property_str) {
        Ok(("", p)) => p,
        Ok((r, _)) => {
            return Ok(Some(format!(
                " Failed to parse everything as a property: \"{}\"",
                r
            )))
        }
        Err(_) => {
            return Ok(Some(format!(
                " Failed to parse \"{}\" as a property",
                property_str
            )))
        }
    };
    match context.properties.remove(&property) {
        true => Ok(Some(format!(" Removed {} from the properties", property))),
        false => Ok(Some(format!(
            " Could not find {} in the properties",
            property
        ))),
    }
}

fn ctx_show_callback(context: &mut ReplContext) -> Result<Option<String>> {
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
    if !context.associativities.is_empty() {
        // k properties
        content.push_str("\n Operator Associativities :\n");
        for (op_c, op_ass) in &context.associativities {
            content.push_str(&format!(" | {} ({})\n", op_c, op_ass));
        }
    }
    Ok(Some(content))
}

fn settings_callback(args: ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    let param_name = match args.get_one::<String>("param") {
        Some(x) => x.as_str(),
        None => {
            return Ok(Some(
                " usage: settings (auto-break/route-poc/expr-pretty-print) (on/off/show)"
                    .to_string(),
            ))
        }
    };
    let action_name = match args.get_one::<String>("action") {
        Some(x) => x.as_str(),
        None => "show",
    };
    match param_name {
        "auto-break" => handle_setting!(context.auto_break, action_name, param_name),
        "expr-pretty-print" => handle_setting!(context.pretty_print_steps, action_name, param_name),
        "route-poc" => handle_setting!(context.route_poc, action_name, param_name),
        _ => return Err(UnknownCommand(param_name.to_string())),
    }
}

fn rule_callback(args: ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    let body_str = concat_args(args.get_many("body").unwrap());
    let preprocessed_body_srt = process_macros(remove_comments(body_str.as_str()).as_str(), None);
    let objects = split_algebraic_objects(match parser::root::<ApsParserKind>(&preprocessed_body_srt) {
        Ok(("", objects)) => objects,
        Ok((rest, _)) => return Ok(Some(format!(" Error: Could not parse '{}'", rest))),
        Err(err) => return Ok(Some(format!(" Error occurred while parsing :\n{}", err))),
    });
    // extend context
    extend_context(objects, context);
    Ok(Some(" Added object(s) to context.".to_owned()))
}

fn graph_callback(args: ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    // concat args of expression
    let expression_str = concat_args(args.get_many("expression").unwrap());
    let atom = str2atom(&expression_str);
    let mut graph = init_graph(atom);
    // number of explorations
    let depth = args
        .get_one::<String>("num explorations")
        .unwrap()
        .parse::<u8>()?;
    for _ in 0..depth {
        if !explore_graph(&mut graph, &context.properties, &context.associativities) {
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
    if !property_str.ends_with(";; ") {
        property_str.push_str(";;");
    }
    Ok(solve_equality_str(property_str, context))
}

pub fn solve_equality_str(property_str: String, context: &mut ReplContext) -> Option<String> {
    // parse the expression
    let property = match parser::property_p::<ApsParserKind>(&property_str) {
        Ok((_, property)) => property,
        Err(err) => {
            eprint!(" Error in parsing property: {}", err);
            return None;
        }
    };
    // solve the equation
    let solution = match solve_equality(
        context.properties.clone(),
        context.k_properties.clone(),
        &context.associativities,
        &property.left_atom,
        &property.right_atom,
        context.auto_break,
    ) {
        Some(solution) => solution,
        None => return Some(format!(" No proof found for {}", property)),
    };
    // build solution string
    let mut solution_str = format!(" Proof found for {} :\n", property);
    let (first_expr, _, _) = solution.first().unwrap(); // no transform for the base expr
    solution_str.push_str(&format!("  {}\n", format_toplevel_atom(first_expr)));
    if solution.len() < 2 {
        return Some(solution_str);
    }
    // length of the lengthiest expression
    let mut max_expr_length: usize = 0;
    // stringify expressions and rules (setting max_expr_length at the same time)
    let solution_contents: Vec<(String, String, String)> = solution
        .iter()
        .skip(1)
        .map(|(atom, rule, common)| {
            // Stringify expression
            let expr_str = if context.pretty_print_steps {
                let naked_atom = strip_expr_naked(atom, &context.associativities);
                format_toplevel_atom(&naked_atom)
            } else {
                format_toplevel_atom(&atom)
            };
            // Stringify rule
            let rule_str = match rule {
                Some(r) => r.to_string(),
                None => "?".to_string(),
            };
            // update max_expr_length if needed
            let expr_length = expr_str.len();
            if expr_length > max_expr_length {
                max_expr_length = expr_length;
            }
            (
                expr_str,
                String::from(if *common && context.route_poc {
                    " /!\\\t"
                } else {
                    "\t"
                }),
                rule_str.clone(),
            )
        })
        .collect::<Vec<_>>();
    // max number of tabs
    let max_num_tabs = compute_num_tabs(max_expr_length);
    // Concatenating these into the solution_str
    for (expr_str, special_str, rule_str) in solution_contents {
        solution_str.push_str(&format!(
            " = {}{}|{}{} \n",
            expr_str,
            (0..compute_rel_num_tabs(expr_str.len(), max_num_tabs))
                .map(|_| "\t")
                .collect::<String>()
                .as_str(),
            special_str,
            rule_str,
        ));
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
    // + 2: at least one full tab
    return 2 + max_num_tabs - num_tabs;
}

pub fn import_into_context(context: &mut ReplContext, filename: &str) -> bool {
    // read file
    let content = match read_and_preprocess_file(filename, None) {
        Some(s) => s,
        None => return false,
    };
    let content_box = Box::new(content);
    // parse input context
    let alg_objects = match parser::root::<ApsParserKind>(&content_box) {
        Ok(("", algebraic_objects)) => algebraic_objects,
        Ok((rest, algebraic_objects)) => panic!(
            " Failed to parse everything:\nParsed (root) :\n{:#?}\n'{}'\n",
            algebraic_objects, rest,
        ),
        Err(err) => panic!("Failed to parse expression:\n{:#?}", err),
    };
    // split objects
    let objects = split_algebraic_objects(alg_objects);
    // extend context
    extend_context(objects, context);
    true
}

fn extend_context(
    objects: (
        HashSet<AlgebraicProperty>,
        HashSet<AlgebraicFunction>,
        HashSet<KProperty>,
        AssociativityHashMap,
    ),
    context: &mut ReplContext,
) {
    let (properties, functions, _k_properties, associativities) = objects;
    context.properties.extend(properties);
    context.functions.extend(functions.clone());
    context.k_properties.extend(_k_properties);
    context.associativities.extend(associativities);
    // add functions as properties
    context
        .properties
        .extend(functions.iter().map(|function| AlgebraicProperty {
            left_atom: Atom::FunctionCall(FunctionCallExpr {
                name: function.name.clone(),
                args: function.arg_atoms.clone(),
            }),
            right_atom: function.value_atom.clone(),
        }));
}
fn concat_args(args: ValuesRef<String>) -> String {
    // concat args
    let mut property_str = String::new();
    for value in args {
        property_str.push_str(value);
        property_str.push(' ');
    }
    property_str.trim().to_string()
}

pub fn str2atom(input: &str) -> Atom {
    match parser::toplevel_atom_p::<ApsParserKind>(input) {
        Ok(("", expr)) => expr,
        Ok((rest, parsed)) => panic!(
            " Failed to parse entirety of atom:\nRest: <<{}>>\nParsed (expr) :\n{:#?}\n",
            rest, parsed
        ),
        Err(err) => panic!(" Failed to parse atom:\n{:#?}", err),
    }
}
