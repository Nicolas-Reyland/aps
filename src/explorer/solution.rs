// 

use std::thread;

use crate::{
    aps_parser::{
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
    let mut left = init_graph(left_expression.clone());
    let mut right = init_graph(right_expression.clone());
    let mut depth: u8 = 0;
    loop {
        // look for a common element
        for lnode in &left.nodes {
            for rnode in &right.nodes {
                if rnode == lnode {
                    return Some(find_route(
                        &left,
                        &right,
                        &lnode,
                        &rnode
                    ))
                }
            }
        }
        // explore the graphs once each
        let left_handle = thread::spawn(
            move || explore_graph(&mut left, &properties, &functions)
        );
        let right_handle = thread::spawn(
            move || explore_graph(&mut right, &properties, &functions)
        );
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
        if auto_break && (
            depth > MAX_GRAPH_EXPLORATION_DEPTH ||
            left.nodes.len() > MAX_NODES_PER_GRAPH ||
            right.nodes.len() > MAX_NODES_PER_GRAPH)
        {
            eprintln!(
                "Automatically breaking from graph exploration algorithm. Depth: {}, Left: {}, Right: {}",
                depth, left.nodes.len(), right.nodes.len()
            );
            break
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
            Some(lnode.transforms.first().unwrap().clone())
        ));
        lnode = &left.nodes[*lnode.neighbours.first().unwrap()];
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
        next_transform = Some(rnode.transforms.first().unwrap().clone());
        rnode = &right.nodes[*rnode.neighbours.first().unwrap()];
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
