// APS Repl

use crate::explorer::strip_expr_naked;
use crate::{
    explorer::{atom2atom_expr, explore_graph, init_graph, print_graph_dot_format},
    parser::{
        self, split_algebraic_objects, AlgebraicFunction, AlgebraicProperty, AssociativityHashMap,
        Atom, AtomExpr, KProperty,
    },
    preprocessor::read_and_preprocess_file,
    solution::solve_equality,
};
use divrem::DivCeil;
use reedline_repl_rs::{
    clap::{parser::ValuesRef, Arg, ArgMatches, Command},
    Repl, Result,
};
use std::collections::{HashMap, HashSet};

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
        .with_name("Algebraic Proof System Language ")
        .with_version("v0.1.0")
        .with_description("Prove algebraic statements using algebraic structures definitions")
        .with_banner("REPL for the Algebraic Proof System Language")
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
                .arg(Arg::new("command").required(false).num_args(1))
                .about("Print or clear the current context (show, clear)"),
            ctx_callback,
        );
    repl.run().unwrap();
}

fn ctx_callback(args: ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    match args
        .get_one::<String>("command")
        .unwrap_or(&"show".to_string())
        .as_str()
    {
        "clear" => {
            reset_context(context);
            return Ok(Some(" Cleared context\n".to_string()));
        }
        "show" | "" => (),
        command => {
            return Ok(Some(format!(
                " Unknown ctx command: {} ('show' or 'clear')",
                command
            )))
        }
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
        _ => {
            return Ok(Some(
                " usage: settings (auto-break/expr-pretty-print) (on/off/show)".to_string(),
            ))
        }
    }
}

fn rule_callback(args: ArgMatches, context: &mut ReplContext) -> Result<Option<String>> {
    let body_str = concat_args(args.get_many("body").unwrap());
    let objects = split_algebraic_objects(match parser::root::<parser::ApsParserKind>(&body_str) {
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
    let expression = str2atom_expr(&expression_str);
    let mut graph = init_graph(expression);
    // number of explorations
    let depth = args
        .get_one::<String>("num explorations")
        .unwrap()
        .parse::<u8>()?;
    for _ in 0..depth {
        if !explore_graph(
            &mut graph,
            &context.properties,
            &context.functions,
            &context.associativities,
        ) {
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
    let property = match parser::property_p::<parser::ApsParserKind>(&property_str) {
        Ok((_, property)) => property,
        Err(err) => {
            eprint!(" Error in parsing property: {}", err);
            return None;
        }
    };
    // solve the equation
    let solution = match solve_equality(
        context.properties.clone(),
        context.functions.clone(),
        context.k_properties.clone(),
        &context.associativities,
        &property.atom_expr_left,
        &property.atom_expr_right,
        context.auto_break,
    ) {
        Some(solution) => solution,
        None => return Some(format!(" No solution found for {}", property)),
    };
    // build solution string
    let mut solution_str = format!(" Solution found for {} :\n", property);
    let (first_expr, _, _) = solution.first().unwrap(); // no transform for the base expr
    solution_str.push_str(&format!("  {}\n", first_expr));
    if solution.len() < 2 {
        return Some(solution_str);
    }
    // length of the lengthiest expression
    let mut max_expr_length: usize = 0;
    // stringify expressions and rules (setting max_expr_length at the same time)
    let solution_contents: Vec<(String, String, String)> = solution
        .iter()
        .skip(1)
        .map(|(expr, rule, common)| {
            // Stringify expression
            let expr_str = if context.pretty_print_steps {
                strip_expr_naked(expr, &context.associativities).to_string()
            } else {
                expr.to_string()
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
    let alg_objects = match parser::root::<parser::ApsParserKind>(&content_box) {
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
            atom_expr_left: atom2atom_expr(Atom::FunctionCall((
                function.name.clone(),
                function.atom_expr_args.clone(),
            ))),
            atom_expr_right: function.atom_expr_right.clone(),
        }));
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
    match parser::atom_expr_p::<parser::ApsParserKind>(input) {
        Ok(("", expr)) => expr,
        Ok((rest, parsed)) => panic!(
            " Failed to parse whole expression:\n'{}'\nParsed (expr) :\n{:#?}\n",
            rest, parsed
        ),
        Err(err) => panic!(" Failed to parse expression:\n{:#?}", err),
    }
}
