use std::fmt;

#[derive(Debug, Clone)]
pub enum TreeError {
    NodeNotFound { node_id: NodeId },
}

pub struct Tree<T> {
    nodes: Vec<Node<T>>,
}

impl<T> Tree<T> {
    pub fn new() -> Tree<T> {
        Tree { nodes: Vec::new() }
    }

    pub fn get_node(&self, id: &NodeId) -> Result<&Node<T>, TreeError> {
        let ret = self.nodes.get(id.index);
        if let None = ret {
            Err(TreeError::NodeNotFound {
                node_id: id.clone(),
            })
        } else {
            Ok(ret.expect("Found to be not none."))
        }
    }

    pub fn get_node_mut(&mut self, id: &NodeId) -> Result<&mut Node<T>, TreeError> {
        let ret = self.nodes.get_mut(id.index);
        if let None = ret {
            Err(TreeError::NodeNotFound {
                node_id: id.clone(),
            })
        } else {
            Ok(ret.expect("Found to be not none."))
        }
    }

    pub fn new_node(&mut self, data: T) -> NodeId {
        let id = NodeId {
            index: self.nodes.len(),
        };
        self.nodes.push(Node {
            id,
            parent: None,
            previous_sibling: None,
            next_sibling: None,
            children: None,
            data,
        });
        id
    }

    pub fn append_to(&mut self, nodeid: &NodeId, toappendid: &NodeId) -> Result<(), TreeError> {
        let mut node = self.get_node_mut(nodeid)?;
        let mut toappend = self.get_node_mut(toappendid)?;

        if let None = node.children {
            node.children = Some(Vec::new());
        }

        let mut children = node.children.as_mut().expect("Should never fail.");
        if children.len() > 0 {
            let sibling_id = children
                .get(children.len() - 1)
                .expect("Should never fail.");
            let mut sibling = self.get_node_mut(sibling_id)?;

            toappend.previous_sibling = Some(sibling_id.clone());
            sibling.next_sibling = Some(toappend.id);
        }
        children.push(toappend.id);

        toappend.parent = Some(node.id);

        Ok(())
    }
}

impl<T> fmt::Display for Tree<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "hello")
    }
}

pub struct Node<T> {
    pub id: NodeId,
    pub parent: Option<NodeId>,
    pub previous_sibling: Option<NodeId>,
    pub next_sibling: Option<NodeId>,
    pub children: Option<Vec<NodeId>>,
    pub data: T,
}

impl<T> Node<T> {
    pub fn append(&mut self, toappend: &NodeId, tree: &mut Tree<T>) {
        tree.append_to(&self.id, toappend);
    }
}

#[derive(Debug, Copy, Clone)]
pub struct NodeId {
    //using usize guarantees vector of nodes is not too large
    index: usize,
}
