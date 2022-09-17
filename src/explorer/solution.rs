// 

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
    right_expression: &AtomExpr
) -> Option<Vec<AtomExpr>> {
    let mut left = init_graph(left_expression.clone());
    let mut right = init_graph(right_expression.clone());
    let mut depth: u8 = 0;
    loop {
        // look for a common element
        for lnode in &left.nodes {
            for rnode in &right.nodes {
                if rnode == lnode {
                    return Some(find_route(&left, &right, &lnode, &rnode))
                }
            }
        }
        // explore the graphs once each
        if !(explore_graph(&mut left, &properties, &functions) ||
            explore_graph(&mut right, &properties, &functions))
        {
            // one graph can evolve alone, and maybe 'join' the other
            // so we have to wait for both graphs to be 'exhausted'
            break
        }
        // increment the depth
        depth += 1;
        // manual calculation limits
        if depth > MAX_GRAPH_EXPLORATION_DEPTH ||
            left.nodes.len() > MAX_NODES_PER_GRAPH ||
            right.nodes.len() > MAX_NODES_PER_GRAPH
        {
            eprintln!("Manually breaking from graph exploration algorithm");
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
) -> Vec<AtomExpr> {
    let mut route: Vec<AtomExpr> = Vec::new();
    // link base-node to common on the left
    let mut lroute: Vec<AtomExpr> = Vec::new();
    let mut lnode = lcommon;
    while lnode.index != 0 {
        lroute.push(lnode.atom_expr.clone());
        lnode = &left.nodes[*lnode.neighbours.first().unwrap()];
    }
    lroute.push(lnode.atom_expr.clone());
    lroute.reverse(); // from common->base to base->common
    // link base-node to common on the right
    let mut rroute: Vec<AtomExpr> = Vec::new();
    let mut rnode = rcommon;
    while rnode.index != 0 {
        rroute.push(rnode.atom_expr.clone());
        rnode = &right.nodes[*rnode.neighbours.first().unwrap()];
    }
    rroute.push(rnode.atom_expr.clone());
    rroute.reverse(); // from common->base to base->common
    // add all the left route
    route.append(
        &mut lroute
    );
    // remove the common element from the right route (or we'll have it twice)
    assert_eq!(rroute.pop().unwrap(), lcommon.atom_expr);
    // return the route
    route
}
