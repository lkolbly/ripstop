pub struct Tree<T> {
    nodes: Vec<Node<T>>,
}

pub struct Node<T> {
    id: Option<NodeId>,
    parent: Option<NodeId>,
    previous_sibling: Option<NodeId>,
    next_sibling: Option<NodeId>,
    children: Option<Vec<NodeId>>,
    data: T,
}

pub struct NodeId {
    //using usize guarantees vector of nodes is not too large
    index: usize,
}
