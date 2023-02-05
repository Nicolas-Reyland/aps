use crate::{
    explorer::{
        apply_property, graph_contains_atom, new_nodes_contain_atom, AtomGraph, GraphNode,
        GraphNodeIndex,
    },
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

// Results in 2 * MAX_NUM_THREADS_PER_EXPLORATION threads in total
// (two graph explorations in parallel)
pub static mut MAX_NUM_THREADS_PER_EXPLORATION: usize = 4;

/// Returns 1 if we collected once, 0 otherwise
pub fn wait_for_next_thread(
    receiver: &Receiver<ExplorationResult>,
    graph: &AtomGraph,
    new_nodes: &mut Vec<(GraphNode, GraphNode)>,
    handles: &Vec<ExplorationHandle>,
) -> i32 {
    let num_current_threads = handles.len();
    let wait_once: bool;
    unsafe {
        wait_once = num_current_threads >= MAX_NUM_THREADS_PER_EXPLORATION;
    };
    if wait_once {
        // wait for one thread to finish
        collect_once(receiver, graph, new_nodes);
        1
    } else {
        0
    }
}

/// Explore the property, and send the result through the 'sender'
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

/// All threads for the current graph exploration are considered to be started.
/// Join them all, and collect once per joining (except for 'early_collections' threads),
/// which are considered to be already collected.
pub fn collect_all_remaining_threads(
    receiver: &Receiver<ExplorationResult>,
    mut early_collections: i32,
    graph: &AtomGraph,
    new_nodes: &mut Vec<(GraphNode, GraphNode)>,
    handles: Vec<ExplorationHandle>,
) {
    // join all the handles
    for handle in handles {
        handle
            .join()
            .unwrap()
            .expect("Could not join remaining handle");
        if early_collections != 0 {
            early_collections -= 1;
        } else {
            collect_once(receiver, graph, new_nodes)
        };
    }
}

/// Collect one result through the 'receiver'. Prune it. Add it to the new_nodes.
pub fn collect_once(
    receiver: &Receiver<ExplorationResult>,
    graph: &AtomGraph,
    new_nodes: &mut Vec<(GraphNode, GraphNode)>,
) {
    let (property, node_index, mut new_atoms) =
        receiver.recv_timeout(Duration::from_millis(1000)).unwrap();
    // filter out the generated atoms that are already in the graph
    new_atoms = new_atoms
        .into_iter()
        .filter(|new_atom| {
            !graph_contains_atom(graph, new_atom) && !new_nodes_contain_atom(new_nodes, new_atom)
        })
        .collect();
    // source node
    let node = &graph.nodes[node_index];
    // add all new nodes to graph
    new_nodes.extend(new_atoms.into_iter().map(|new_atom| {
        (
            node.clone(),
            GraphNode {
                atom: new_atom,
                parent: 0,
                transform: Some(property.clone()),
                depth: 0,
                index: 0,
            },
        )
    }));
}
