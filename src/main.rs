/* Parser for Algebraic Proofing System Language */

use std::env;
use repl::{init_context, import_into_context};


#[path = "aps_parser/aps_parser.rs"] mod aps_parser;
#[path = "explorer/explorer.rs"] mod explorer;
#[path = "explorer/solution.rs"] mod solution;
#[path = "repl/repl.rs"] mod repl;

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
