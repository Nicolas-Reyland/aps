//

use std::collections::HashSet;
use std::thread::JoinHandle;
use std::{
    sync::{Arc, Mutex},
    thread,
};

use crate::clothing::{dress_up_expr, strip_expr_naked};
use crate::{
    explorer::{explore_graph, init_graph, ExprGraph, ExprNode},
    parser::{AlgebraicProperty, AssociativityHashMap, Atom, KProperty},
    MAX_GRAPH_EXPLORATION_DEPTH, MAX_NODES_PER_GRAPH,
};

pub fn solve_equality(
    properties: HashSet<AlgebraicProperty>,
    _k_properties: HashSet<KProperty>,
    associativities: &AssociativityHashMap,
    left_atom: &Atom,
    right_atom: &Atom,
    auto_break: bool,
) -> Option<Vec<(Atom, Option<AlgebraicProperty>, bool)>> {
    // dress both expressions
    // left graph
    let left = init_graph(dress_up_expr(left_atom, associativities));
    let left_mutex = Arc::new(Mutex::new(left));
    // right graph
    let right = init_graph(dress_up_expr(right_atom, associativities));
    let right_mutex = Arc::new(Mutex::new(right));
    // number of explorations
    let mut depth: usize = 0;
    loop {
        let lnodes: Vec<(ExprNode, Atom)> =
            gather_nodes_from_graph(&left_mutex, associativities, depth as u8);
        let rnodes: Vec<(ExprNode, Atom)> =
            gather_nodes_from_graph(&right_mutex, associativities, depth as u8);
        // look for a common element
        for (lnode, naked_lexpr) in lnodes {
            for (rnode, naked_rexpr) in rnodes.clone() {
                if naked_lexpr == naked_rexpr {
                    return Some(find_route(
                        &left_mutex.lock().unwrap(),
                        &right_mutex.lock().unwrap(),
                        &lnode,
                        &rnode,
                    ));
                }
            }
        }
        // explore the graphs once each
        let left_handle = start_graph_exploration(&properties, &left_mutex, associativities);
        let right_handle = start_graph_exploration(&properties, &right_mutex, associativities);
        let left_has_evolved = left_handle.join().unwrap();
        let right_has_evolved = right_handle.join().unwrap();

        if !left_has_evolved && !right_has_evolved {
            // one graph can evolve alone, and maybe 'join' the other
            // so we have to wait for both graphs to be 'exhausted'
            break;
        }
        // increment the depth
        depth += 1;
        // manual calculation limits (see 'settings' command)
        if auto_break {
            let num_left_nodes = num_nodes_left_in_graph(&left_mutex);
            let num_right_nodes = num_nodes_left_in_graph(&right_mutex);
            if depth > MAX_GRAPH_EXPLORATION_DEPTH
                || num_left_nodes > MAX_NODES_PER_GRAPH
                || num_right_nodes > MAX_NODES_PER_GRAPH
            {
                eprintln!(
                    "Automatically breaking from graph exploration algorithm. Depth: {}, Left: {}, Right: {}",
                    depth, num_left_nodes, num_right_nodes
                );
                break;
            }
        }
    }
    // no solution found
    None
}

fn num_nodes_left_in_graph(left_mutex: &Arc<Mutex<ExprGraph>>) -> usize {
    let left_mutex_clone = Arc::clone(&left_mutex);
    let left_clone = (*left_mutex_clone.lock().unwrap()).clone();
    left_clone.nodes.len()
}

fn gather_nodes_from_graph(
    left_mutex: &Arc<Mutex<ExprGraph>>,
    operators: &AssociativityHashMap,
    _depth: u8,
) -> Vec<(ExprNode, Atom)> {
    let mutex_clone = Arc::clone(&left_mutex);
    let clone = (*mutex_clone.lock().unwrap()).clone();
    clone
        .nodes
        .into_iter()
        // .filter(|node| node.depth == depth || node.depth + 1 == depth)
        .map(|node| (node.clone(), strip_expr_naked(&node.atom, operators)))
        .collect()
}

fn start_graph_exploration(
    properties: &HashSet<AlgebraicProperty>,
    left_mutex: &Arc<Mutex<ExprGraph>>,
    associativities: &AssociativityHashMap,
) -> JoinHandle<bool> {
    // clone things
    let scoped_left_mutex_clone = Arc::clone(&left_mutex);
    let properties_clone = properties.clone();
    let associativities_clone = associativities.clone();
    // return thread
    thread::spawn(move || {
        explore_graph(
            &mut *scoped_left_mutex_clone.lock().unwrap(),
            &properties_clone,
            &associativities_clone,
        )
    })
}

/// A solution has been found (common element in the two graphs)
/// Now, the 'route' that links the root nodes from each graph has to be found
/// Since the nodes are unique and hierarchically ordered,
/// there is only one such route in the graphs. It is also guaranteed that this
/// path (route) is the shortest possible route between the two root nodes
/// (with the given properties).
fn find_route(
    left: &ExprGraph,
    right: &ExprGraph,
    lcommon: &ExprNode,
    rcommon: &ExprNode,
) -> Vec<(Atom, Option<AlgebraicProperty>, bool)> {
    // final route
    let mut route: Vec<(Atom, Option<AlgebraicProperty>, bool)> = Vec::new();
    // link base-node to common on the left
    let mut lroute: Vec<(Atom, Option<AlgebraicProperty>, bool)> = Vec::new();
    let mut lnode = lcommon;
    let first_index = lnode.index;
    while lnode.index != 0 {
        lroute.push((
            lnode.atom.clone(),
            Some(lnode.transform.as_ref().unwrap().clone()),
            lnode.index == first_index,
        ));
        lnode = &left.nodes[lnode.parent];
    }
    // push base node
    lroute.push((lnode.atom.clone(), None, false));
    lroute.reverse(); // from common->base to base->common
                      // link base-node to common on the right
    let mut rroute: Vec<(Atom, Option<AlgebraicProperty>, bool)> = Vec::new();
    let mut rnode = rcommon;
    let mut next_transform: Option<AlgebraicProperty> = None;
    while rnode.index != 0 {
        rroute.push((rnode.atom.clone(), next_transform, false));
        next_transform = Some(rnode.transform.as_ref().unwrap().clone());
        rnode = &right.nodes[rnode.parent];
    }
    // push the base
    rroute.push((rnode.atom.clone(), next_transform, false));
    // add all the left route
    route.append(&mut lroute);
    // remove the common element from the right route (or we'll have it twice)
    rroute.remove(0);
    /*
    println!("(l)route :");
    for (route_expr, route_tr, _) in route.clone() {
        match route_tr {
            Some(tr) => println!("\t{} | {}", route_expr, tr),
            None => println!("\t{} | ?", route_expr),
        }
    }
    println!("rroute :");
    for (route_expr, route_tr, _) in rroute.clone() {
        match route_tr {
            Some(tr) => println!("\t{} | {}", route_expr, tr),
            None => println!("\t{} | ?", route_expr),
        }
    }
    */
    route.append(&mut rroute);
    // return the route
    route
}
