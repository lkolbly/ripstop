use std::{
    fmt,
    ops::{Index, IndexMut},
};

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
        {
            let mut node = self.get_node_mut(nodeid)?;
            if let None = node.children {
                node.children = Some(Vec::new());
            }
        }

        let sibling_id = {
            let node = self.get_node_mut(nodeid)?;

            let children = node.children.as_mut().expect("Should never fail.");
            if children.len() > 0 {
                children.push(toappendid.clone());

                let sibling_id = children
                    .get(children.len() - 1)
                    .expect("Should never fail.")
                    .clone();

                Some(sibling_id)
            } else {
                children.push(toappendid.clone());
                None
            }
        };

        if let Some(sib_id) = sibling_id {
            let mut sibling = self.get_node_mut(&sib_id)?;
            sibling.next_sibling = Some(toappendid.clone());
        }

        let mut toappend = self.get_node_mut(toappendid)?;
        toappend.previous_sibling = {
            if let Some(sib_id) = sibling_id {
                Some(sib_id.clone())
            } else {
                None
            }
        };
        toappend.parent = Some(nodeid.clone());

        Ok(())
    }
}

impl<T: std::fmt::Debug> fmt::Display for Tree<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let root = self.nodes.get(0);
        if let Some(root_node) = root {
            let mut cur_node = root_node;
            let mut indent_level = 0;
            while {
                f.write_fmt(format_args!(
                    "{}{:?} \n",
                    "  ".repeat(indent_level),
                    cur_node.data
                ))?;
                let mut found_next = false;
                if let Some(children) = &cur_node.children {
                    let child = children.get(0);
                    if let Some(child_id) = child {
                        cur_node = self.get_node(child_id).unwrap();
                        indent_level += 1;
                        found_next = true;
                    }
                }
                if !found_next {
                    if let Some(sib_id) = cur_node.next_sibling {
                        cur_node = self.get_node(&sib_id).unwrap();
                        found_next = true;
                    }
                }
                if !found_next {
                    if let Some(parent_id) = cur_node.parent {
                        let parent = self.get_node(&parent_id).unwrap();
                        if let Some(uncle_id) = parent.next_sibling {
                            cur_node = self.get_node(&uncle_id).unwrap();
                            found_next = true;
                            indent_level -= 1;
                        }
                    }
                }
                found_next
            } {}
            write!(f, "")
        } else {
            write!(f, "Empty Tree")
        }
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
    pub fn append(&mut self, toappend: &NodeId, tree: &mut Tree<T>) -> Result<(), TreeError> {
        tree.append_to(&self.id, toappend)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct NodeId {
    //using usize guarantees vector of nodes is not too large
    index: usize,
}

impl From<usize> for NodeId {
    fn from(n: usize) -> Self {
        NodeId { index: n }
    }
}

impl<T> Index<NodeId> for Vec<Node<T>> {
    type Output = Node<T>;

    fn index(&self, index: NodeId) -> &Self::Output {
        &self[index.index]
    }
}

impl<T> IndexMut<NodeId> for Vec<Node<T>> {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self[index.index]
    }
}
