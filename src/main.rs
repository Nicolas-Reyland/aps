/* Parser for Algebraic Proofing System Language */

use aps::repl::{import_into_context, init_context, repl};
use aps::threads::MAX_NUM_THREADS_PER_EXPLORATION;
use std::env;

fn main() {
    // set max number of threads per exploration
    unsafe {
        MAX_NUM_THREADS_PER_EXPLORATION = num_cpus::get() * 2;
        println!(
            "num threads per exploration (max): {}",
            MAX_NUM_THREADS_PER_EXPLORATION
        );
    }
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
    repl(context);
}
