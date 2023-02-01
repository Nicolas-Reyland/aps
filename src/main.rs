/* Parser for Algebraic Proofing System Language */

use repl::{import_into_context, init_context, solve_equality_str};
use std::env;

#[path = "explorer/explorer.rs"]
pub(crate) mod explorer;
#[path = "tests/explorer_tests.rs"]
pub(crate) mod explorer_tests;
#[path = "parser/parser.rs"]
pub(crate) mod parser;
#[path = "tests/parser_tests.rs"]
pub(crate) mod parser_tests;
#[path = "parser/preprocessor.rs"]
pub(crate) mod preprocessor;
#[path = "tests/preprocessor_tests.rs"]
pub(crate) mod preprocessor_tests;
#[path = "repl/repl.rs"]
mod repl;
#[path = "explorer/solution.rs"]
mod solution;

static MAX_GRAPH_EXPLORATION_DEPTH: usize = 12;
static MAX_NODES_PER_GRAPH: usize = 100_000;

fn main() {
    // collect cmd args
    let args: Vec<String> = env::args().collect();
    // create repl context
    let mut context = init_context();
    // import files if needed
    for filename in args.iter().skip(1) {
        // add rules to context
        if !import_into_context(&mut context, filename) {
            println!(" failed to import '{}'", filename);
        }
    }
    // println!("{:?}", solve_equality_str("square(sqrt(X)) = X ;;".to_string(), &mut context));
    // start a repl
    repl::repl(context);
}
