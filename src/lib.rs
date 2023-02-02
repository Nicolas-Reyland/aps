#[path = "parser/parser.rs"]
pub mod parser;
#[path = "parser/preprocessor.rs"]
pub mod preprocessor;
#[path = "repl/repl.rs"]
pub mod repl;
#[path = "explorer/solution.rs"]
pub mod solution;
#[path = "explorer/explorer.rs"]
pub mod explorer;
#[path = "explorer/clothing.rs"]
pub mod clothing;

static MAX_GRAPH_EXPLORATION_DEPTH: usize = 12;
static MAX_NODES_PER_GRAPH: usize = 100_000;
