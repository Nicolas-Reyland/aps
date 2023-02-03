use crate::{
    explorer::{apply_property, AtomGraph, GraphNode, GraphNodeIndex},
    parser::{AlgebraicProperty, AssociativityHashMap, Atom},
};
use std::{
    collections::HashSet,
    sync::mpsc::{Receiver, SendError, Sender},
    thread::{self, JoinHandle},
};

pub type ExplorationResult = (AlgebraicProperty, GraphNodeIndex, HashSet<Atom>);
pub type ExplorationHandle = JoinHandle<Result<(), SendError<ExplorationResult>>>;

// number od CPU core that I have. this would result in 2 * MAX_NUM_THREADS_PER_EXPLORATION threads in total
// TODO: make this N * num-cpu-cores (value of N ??)
static MAX_NUM_THREADS_PER_EXPLORATION: usize = 6;

pub fn wait_for_next_thread(
    receiver: &Receiver<ExplorationResult>,
    graph: &AtomGraph,
    new_nodes: &mut Vec<(GraphNode, GraphNode)>,
    handles: &mut Vec<ExplorationHandle>,
) {
    if handles.len() >= MAX_NUM_THREADS_PER_EXPLORATION {
        // wait for one thread to finish
        collect_once(receiver, graph, new_nodes);
        for i in 0..handles.len() {
            let handle: &ExplorationHandle = &handles[i];
            if handle.is_finished() {
                handle.join().unwrap().expect("Failed to join on handle");
                handles.remove(i);
                return;
            }
        }
        panic!(
            "Collect one item, but no threads have finished (where does the result come from ???)"
        );
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

pub fn collect_finished_threads(
    receiver: &Receiver<ExplorationResult>,
    graph: &AtomGraph,
    new_nodes: &mut Vec<(GraphNode, GraphNode)>,
    handles: &mut Vec<ExplorationHandle>,
) {
    for (i, handle) in handles.iter().enumerate() {
        if handle.is_finished() {
            handle.join().unwrap().expect("Failed to join on handle");
            handles.remove(i);
            // there HAS to be ONE message (we just joined a thread)
            collect_once(receiver, graph, new_nodes);
        }
    }
}

pub fn collect_all_remaining_threads(
    receiver: &Receiver<ExplorationResult>,
    graph: &AtomGraph,
    new_nodes: &mut Vec<(GraphNode, GraphNode)>,
    handles: &mut Vec<ExplorationHandle>,
) {
    // join all the handles
    for handle in handles {
        handle.join().unwrap();
        collect_once(receiver, graph, new_nodes);
    }
    handles.clear();
}

fn collect_once(
    receiver: &Receiver<ExplorationResult>,
    graph: &AtomGraph,
    new_nodes: &mut Vec<(GraphNode, GraphNode)>,
) {
    let (property, node_index, new_expressions) = receiver.recv().unwrap();
    println!("Received ({}, {},", property, node_index);
    for expr in &new_expressions {
        println!("({}) - {}", node_index, expr);
    }
    let node = graph.nodes[node_index];
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
