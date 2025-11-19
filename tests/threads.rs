use std::collections::HashSet;
use std::sync::mpsc::channel;
use std::thread;
use std::time::Duration;

use aps::explorer::{init_graph, GraphNode};
use aps::parser::{self, AlgebraicProperty, ApsParserKind, Atom};
use aps::repl::str2atom;
use aps::threads::collect_once;

#[test]
fn collect_once_waits_past_timeout() {
    let (sender, receiver) = channel();
    let graph = init_graph(str2atom("A + B"));
    let mut new_nodes: Vec<(GraphNode, GraphNode)> = Vec::new();
    let property: AlgebraicProperty = parser::property_p::<ApsParserKind>("A + B = B + A ;")
        .unwrap()
        .1;
    let atoms: HashSet<Atom> = vec![str2atom("B + A")].into_iter().collect();

    let handle = thread::spawn(move || {
        thread::sleep(Duration::from_millis(1200));
        sender.send((property, 0, atoms)).unwrap();
    });

    let _ = collect_once(&receiver, &graph, &mut new_nodes);
    handle.join().unwrap();

    assert_eq!(new_nodes.len(), 1);
}
