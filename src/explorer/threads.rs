use crate::{
    explorer::{
        apply_property, graph_contains_atom, new_nodes_contain_atom, AtomGraph, GraphNode,
        GraphNodeIndex,
    },
    parser::{AlgebraicProperty, AssociativityHashMap, Atom},
};
use std::{
    collections::HashSet,
    sync::mpsc::{Receiver, RecvTimeoutError, SendError, Sender},
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
) -> Result<i32, RecvTimeoutError> {
    let num_current_threads = handles.len();
    let wait_once: bool;
    unsafe {
        wait_once = num_current_threads >= MAX_NUM_THREADS_PER_EXPLORATION;
    };
    if wait_once {
        // wait for one thread to finish
        collect_once(receiver, graph, new_nodes)?;
        Ok(1)
    } else {
        Ok(0)
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
) -> Result<(), RecvTimeoutError> {
    // join all the handles
    for handle in handles {
        handle
            .join()
            .unwrap()
            .expect("Could not join remaining handle");
        if early_collections != 0 {
            early_collections -= 1;
        } else {
            collect_once(receiver, graph, new_nodes)?
        };
    }
    Ok(())
}

/// Collect one result through the 'receiver'. Prune it. Add it to the new_nodes.
pub fn collect_once(
    receiver: &Receiver<ExplorationResult>,
    graph: &AtomGraph,
    new_nodes: &mut Vec<(GraphNode, GraphNode)>,
) -> Result<(), RecvTimeoutError> {
    loop {
        match receiver.recv_timeout(Duration::from_millis(1000)) {
            Ok((property, node_index, mut new_atoms)) => {
                new_atoms = new_atoms
                    .into_iter()
                    .filter(|new_atom| {
                        !graph_contains_atom(graph, new_atom)
                            && !new_nodes_contain_atom(new_nodes, new_atom)
                    })
                    .collect();
                let node = &graph.nodes[node_index];
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
                break Ok(());
            }
            Err(RecvTimeoutError::Timeout) => continue,
            Err(RecvTimeoutError::Disconnected) => break Err(RecvTimeoutError::Disconnected),
        }
    }
}
