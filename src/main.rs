/* Parser for Algebraic Proofing System Language */

use std::env;
use repl::{init_context, import_into_context};

#[path = "parser/parser.rs"] mod parser;
#[path = "parser/preprocessor.rs"] pub(crate) mod preprocessor;
#[path = "explorer/explorer.rs"] mod explorer;
#[path = "explorer/solution.rs"] mod solution;
#[path = "repl/repl.rs"] mod repl;
#[path = "either/either.rs"] mod either;
#[path = "tests/parser_tests.rs"] pub(crate) mod parser_tests;
#[path = "tests/solution_tests.rs"] pub(crate) mod solution_tests;
#[path = "tests/preprocessor_tests.rs"] pub(crate) mod preprocessor_tests;

static MAX_GRAPH_EXPLORATION_DEPTH: u8 = 12;
static MAX_NODES_PER_GRAPH: usize = 100_000;

fn main() {
    // collect cmd args
    let args: Vec<String> = env::args().collect();
    // create repl context
    let mut context = init_context();
    // import files if needed
    for filename in args.iter().skip(1) {
        // add rules to context
        import_into_context(&mut context, filename);
    }
    // start a repl
    repl::repl(context);
}
