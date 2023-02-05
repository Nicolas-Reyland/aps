use crate::{
    explorer::{apply_property, AtomGraph, GraphNode, GraphNodeIndex},
    parser::{AlgebraicProperty, AssociativityHashMap, Atom},
};
use std::{
    collections::HashSet,
    sync::mpsc::{Receiver, SendError, Sender},
    thread::{self, JoinHandle},
    time::Duration,
};

pub type ExplorationResult = (AlgebraicProperty, GraphNodeIndex, HashSet<Atom>);
pub type ExplorationHandle = JoinHandle<Result<(), SendError<ExplorationResult>>>;

// number od CPU core that I have. this would result in 2 * MAX_NUM_THREADS_PER_EXPLORATION threads in total
// TODO: make this N * num-cpu-cores (value of N ??)
static MAX_NUM_THREADS_PER_EXPLORATION: usize = 6;

/// Returns 1 if we collected once, 0 otherwise
pub fn wait_for_next_thread(
    receiver: &Receiver<ExplorationResult>,
    graph: &AtomGraph,
    new_nodes: &mut Vec<(GraphNode, GraphNode)>,
    handles: &Vec<ExplorationHandle>,
) -> i32 {
    println!("Number of running threads: {}", handles.len());
    if handles.len() >= MAX_NUM_THREADS_PER_EXPLORATION {
        println!("Waiting for a thread to finish ... ({})", handles.len());
        // wait for one thread to finish
        collect_once(receiver, graph, new_nodes);
        1
    } else {
        0
    }
}

pub fn explore_property(
    sender: &Sender<ExplorationResult>,
    node: &GraphNode,
    property: AlgebraicProperty,
    associativities: &AssociativityHashMap,
    handles: &mut Vec<ExplorationHandle>,
) {
    handles.push({
        let sender_clone = sender.clone();
        let atom_clone = node.atom.clone();
        let property_clone = property.clone();
        let associativities_clone = associativities.clone();
        let node_index = node.index;
        thread::spawn(move || {
            sender_clone.send((
                property_clone.clone(),
                node_index,
                apply_property(&atom_clone, &property_clone, &associativities_clone),
            ))
        })
    });
}

pub fn collect_all_remaining_threads(
    receiver: &Receiver<ExplorationResult>,
    mut early_collections: i32,
    graph: &AtomGraph,
    new_nodes: &mut Vec<(GraphNode, GraphNode)>,
    handles: Vec<ExplorationHandle>,
) {
    // join all the handles
    let mut num_threads = handles.len();
    for handle in handles {
        println!(
            "Joining final handles {} -> {}...",
            num_threads,
            num_threads - 1
        );
        handle
            .join()
            .unwrap()
            .expect("Could not join remaining handle");
        println!("early_collections: {}", early_collections);
        if early_collections != 0 {
            early_collections -= 1;
        } else {
            collect_once(receiver, graph, new_nodes)
        };
        num_threads -= 1;
    }
}

pub fn collect_once(
    receiver: &Receiver<ExplorationResult>,
    graph: &AtomGraph,
    new_nodes: &mut Vec<(GraphNode, GraphNode)>,
) {
    println!("Collecting once ({})", new_nodes.len());
    let (property, node_index, new_expressions) =
        receiver.recv_timeout(Duration::from_millis(1000)).unwrap();
    println!("Received ({}, {})", property, node_index);
    for expr in &new_expressions {
        println!("({}) - {}", node_index, expr);
    }
    let node = &graph.nodes[node_index];
    new_nodes.extend(new_expressions.iter().map(|new_atom| {
        (
            node.clone(),
            GraphNode {
                atom: new_atom.clone(),
                parent: 0,
                transform: Some(property.clone()),
                depth: 0,
                index: 0,
            },
        )
    }));
}
