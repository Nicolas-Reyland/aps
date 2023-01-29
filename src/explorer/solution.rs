// 

use std::{thread, sync::{Mutex, Arc}};

use crate::{
    parser::{
        AtomExpr,
        AlgebraicProperty,
        AlgebraicFunction,
        KProperty
    },
    explorer::{
        init_graph,
        ExprGraph,
        ExprNode,
        explore_graph
    },
    MAX_GRAPH_EXPLORATION_DEPTH,
    MAX_NODES_PER_GRAPH
};

pub fn solve_equality(
    properties: Vec<AlgebraicProperty>,
    functions: Vec<AlgebraicFunction>,
    _k_properties: Vec<KProperty>,
    left_expression: &AtomExpr,
    right_expression: &AtomExpr,
    auto_break: bool,
) -> Option<Vec<(AtomExpr, Option<AlgebraicProperty>)>> {
    // left graph
    let left = init_graph(left_expression.clone());
    let left_mutex = Arc::new(Mutex::new(left));
    // right graph
    let right = init_graph(right_expression.clone());
    let right_mutex = Arc::new(Mutex::new(right));
    // number of explorations
    let mut depth: u8 = 0;
    loop {
        let lnodes = {
            let left_mutex_clone = Arc::clone(&left_mutex);
            let left_clone = (*left_mutex_clone.lock().unwrap()).clone();
            left_clone.nodes
        };
        let rnodes = {
            let right_mutex_clone = Arc::clone(&right_mutex);
            let right_clone = (*right_mutex_clone.lock().unwrap()).clone();
            right_clone.nodes
        };
        // look for a common element
        for lnode in &lnodes {
            for rnode in &rnodes {
                if rnode == lnode {
                    return Some(find_route(
                        &left_mutex.lock().unwrap(),
                        &right_mutex.lock().unwrap(),
                        &lnode,
                        &rnode,
                    ))
                }
            }
        }
        // explore the graphs once each
        let left_handle = {
            // clone things
            let scoped_left_mutex_clone = Arc::clone(&left_mutex);
            let properties_clone = properties.clone();
            let functions_clone = functions.clone();
            // return thread
            thread::spawn(
                move || explore_graph(&mut *scoped_left_mutex_clone.lock().unwrap(), &properties_clone, &functions_clone)
            )
        };
        let right_handle = {
            // clone things
            let scoped_right_mutex_clone = Arc::clone(&right_mutex);
            let properties_clone = properties.clone();
            let functions_clone = functions.clone();
            // return thread
            thread::spawn(
                move || explore_graph(&mut *scoped_right_mutex_clone.lock().unwrap(), &properties_clone, &functions_clone)
            )
        };
        let left_has_evolved = left_handle.join().unwrap();
        let right_has_evolved = right_handle.join().unwrap();

        if !(left_has_evolved || right_has_evolved)
        {
            // one graph can evolve alone, and maybe 'join' the other
            // so we have to wait for both graphs to be 'exhausted'
            break
        }
        // increment the depth
        depth += 1;
        // manual calculation limits
        if auto_break {
            let num_left_nodes = {
                let left_mutex_clone = Arc::clone(&left_mutex);
                let left_clone = (*left_mutex_clone.lock().unwrap()).clone();
                left_clone.nodes.len()
            };
            let num_right_nodes = {
                let right_mutex_clone = Arc::clone(&right_mutex);
                let right_clone = (*right_mutex_clone.lock().unwrap()).clone();
                right_clone.nodes.len()
            };
            if depth > MAX_GRAPH_EXPLORATION_DEPTH ||
                num_left_nodes > MAX_NODES_PER_GRAPH ||
                num_right_nodes > MAX_NODES_PER_GRAPH
            {
                eprintln!(
                    "Automatically breaking from graph exploration algorithm. Depth: {}, Left: {}, Right: {}",
                    depth, num_left_nodes, num_right_nodes
                );
                break
            }
        }
        
    }
    // no solution found
    None
}

fn find_route(
    left: &ExprGraph,
    right: &ExprGraph,
    lcommon: &ExprNode,
    rcommon: &ExprNode,
) -> Vec<(AtomExpr, Option<AlgebraicProperty>)> {
    let mut route: Vec<(AtomExpr, Option<AlgebraicProperty>)> = Vec::new();
    // link base-node to common on the left
    let mut lroute: Vec<(AtomExpr, Option<AlgebraicProperty>)> = Vec::new();
    let mut lnode = lcommon;
    while lnode.index != 0 {
        lroute.push((
            lnode.atom_expr.clone(),
            Some(lnode.transform.as_ref().unwrap().clone())
        ));
        lnode = &left.nodes[lnode.parent];
    }
    // push base node
    lroute.push((lnode.atom_expr.clone(), None));
    lroute.reverse(); // from common->base to base->common
    // link base-node to common on the right
    let mut rroute: Vec<(AtomExpr, Option<AlgebraicProperty>)> = Vec::new();
    let mut rnode = rcommon;
    let mut next_transform: Option<AlgebraicProperty> = None;
    while rnode.index != 0 {
        rroute.push((
            rnode.atom_expr.clone(),
            next_transform
        ));
        next_transform = Some(rnode.transform.as_ref().unwrap().clone());
        rnode = &right.nodes[rnode.parent];
    }
    // push the base
    rroute.push((rnode.atom_expr.clone(), next_transform));
    // add all the left route
    route.append(
        &mut lroute
    );
    // remove the common element from the right route (or we'll have it twice)
    // println!("route:\n{:#?}\n\nrroute:\n{:#?}\n", route, rroute);
    assert_eq!(
        match rroute.remove(0) {(x,_) => x},
        lcommon.atom_expr.clone()
    );
    route.append(
        &mut rroute
    );
    // return the route
    route
}
