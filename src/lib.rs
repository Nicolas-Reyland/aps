#[path = "explorer/clothing.rs"]
pub mod clothing;
#[path = "explorer/explorer.rs"]
pub mod explorer;
#[path = "explorer/generate.rs"]
pub mod generate;
#[path = "parser/parser.rs"]
pub mod parser;
#[path = "parser/preprocessor/preprocessor.rs"]
pub mod preprocessor;
#[path = "repl/repl.rs"]
pub mod repl;
#[path = "solution/solution.rs"]
pub mod solution;
#[path = "explorer/threads.rs"]
pub mod threads;

static MAX_GRAPH_EXPLORATION_DEPTH: usize = 12;
static MAX_NODES_PER_GRAPH: usize = 100_000;
