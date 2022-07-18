pub struct Tree<T> {
    nodes: Vec<Node<T>>,
}

impl<T> Tree<T> {
    pub fn new() -> Tree<T> {
        Tree { nodes: Vec::new() }
    }

    pub fn get_node(&mut self, id: &NodeId) -> Option<&Node<T>> {
        self.nodes.get(id.index)
    }

    pub fn get_node_option(&mut self, id: Option<NodeId>) -> Option<&Node<T>> {
        if let Some(nodeid) = id {
            self.get_node(&nodeid)
        } else {
            None
        }
    }

    pub fn new_node(&mut self, data: T) -> NodeId {
        let id = NodeId {
            index: self.nodes.len(),
        };
        self.nodes.push(Node {
            id: id,
            parent: None,
            previous_sibling: None,
            next_sibling: None,
            children: None,
            data: data,
        });
        id
    }

    pub fn append_to(&mut self, node: &mut Node<T>, toappend: Node<T>) {
        if let None = node.children {
            node.children = Some(Vec::new());
        }

        let children = node.children.expect("Should never fail.");
        if children.len() > 0 {
            let sibling_id = children
                .get(children.len() - 1)
                .expect("Should never fail.");
            let sibling = self.get_node(sibling_id).expect("Should never fail.");

            toappend.previous_sibling = Some(sibling_id.clone());
            sibling.next_sibling = Some(toappend.id);
        }
        children.push(toappend.id);

        toappend.parent = Some(node.id);
    }
}

pub struct Node<T> {
    id: NodeId,
    parent: Option<NodeId>,
    previous_sibling: Option<NodeId>,
    next_sibling: Option<NodeId>,
    children: Option<Vec<NodeId>>,
    data: T,
}

impl<T> Node<T> {
    pub fn append(&mut self, toappend: Node<T>, tree: &mut Tree<T>) {
        tree.append_to(self, toappend);
    }
}

#[derive(Copy, Clone)]
pub struct NodeId {
    //using usize guarantees vector of nodes is not too large
    index: usize,
}
