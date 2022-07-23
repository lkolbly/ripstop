use std::{
    fmt,
    ops::{Add, AddAssign, Index, IndexMut, Sub, SubAssign},
};

#[derive(Debug, Clone)]
pub enum TreeError {
    NodeNotFound { node_id: NodeId },
}

//PROBLEM?: We don't know where the head of the tree is
//In some cases, it's fine to assume that the head of the tree is index 0 but that's definitely not true generically
//This mostly just makes recursion and such harder, though
pub struct Tree<T> {
    nodes: Vec<Node<T>>,
}

impl<T> Default for Tree<T> {
    fn default() -> Self {
        Tree::new()
    }
}

impl<T> Tree<T> {
    pub fn new() -> Tree<T> {
        Tree { nodes: Vec::new() }
    }

    /// Finds the first node without any parents. If no such node exists, returns `None`
    pub fn find_head(&self) -> Option<NodeId> {
        for i in 0..self.nodes.len() {
            if let None = self.nodes[i].parent {
                return Some(i.into());
            }
        }

        None
    }

    //TIL: Rust comments support Markdown (makes sense that they do)
    /// If `id` is a valid node id, then returns the node matching the id
    ///
    /// Otherwise, returns an `Err(TreeError)` describing the failure
    pub fn get_node(&self, id: NodeId) -> Result<&Node<T>, TreeError> {
        let ret = self.nodes.get(id.index);
        if let Some(node) = ret {
            Ok(node)
        } else {
            Err(TreeError::NodeNotFound { node_id: id })
        }
    }

    /// If `id` is a valid node id, then returns the node matching the id
    ///
    /// Otherwise, returns an `Err(TreeError)` describing the failure
    pub fn get_node_mut(&mut self, id: NodeId) -> Result<&mut Node<T>, TreeError> {
        let ret = self.nodes.get_mut(id.index);
        if let Some(node) = ret {
            Ok(node)
        } else {
            Err(TreeError::NodeNotFound { node_id: id })
        }
    }

    /// Tries to get the `Node<T>` with `id`, panics if it can't be found
    pub fn get_node_unchecked(&self, id: NodeId) -> &Node<T> {
        &self.nodes[id]
    }

    /// Tries to get the `Node<T>` with `id`, panics if it can't be found
    pub fn get_node_mut_unchecked(&self, id: NodeId) -> &mut Node<T> {
        &mut self.nodes[id]
    }

    /// Creates a new node with `data` and returns its `NodeId`
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

    /// **NOT YET IMPLEMENTED** (I'll get around to it -- especially if it becomes necessary)
    ///
    /// Swaps the positions of `a` and `b` in the tree, changing relations to reflect that
    ///
    /// However, this *does not* change the values of the nodes referred to by `a` and `b`
    pub fn swap(&mut self, a: NodeId, b: NodeId) {
        //First, swap the `NodeId`s referenced by `a` and `b` without changing the values of `a` and `b`
        {
            //This is accomplished by swapping the two values then swapping their `data` back
            //This is not performant (requires more swaps than necessary), but that should not cause problems and this is more expandable/easier
            self.nodes.swap(a.index, b.index);
            std::mem::swap(&mut self[a].data, &mut self[b].data);
        }
        //Then, the hard part: swap the ids referenced by `a` and `b`'s siblings so each sibling changes which node it references
        {
            //Some things to keep in mind:
            //1. If anything has the same type of relation to both `a` and `b`, there's no need to mutate it
            //  --But, if for example `foo` is a parent to `a` and a sibling to `b`, mutation is required

            //To make this easier, "dereference" `a` and `b`
            let a = self[a];
            let b = self[b];
        }

        todo!()
    }

    /// Swaps the positions of `a` and `b` in the tree, changing relations to reflect that
    ///
    /// This *will* change the nodes referred to by the ids `a` and `b`. After performing this swap, the id `a` will refer to `b` and the id `b` will refer to `a`
    ///
    /// This method is significantly more performant than `swap()` and should be used unless preserving the references is important
    pub fn swap_change_id(&mut self, a: NodeId, b: NodeId) {
        self.nodes.swap(a.index, b.index);
    }

    /// Appends `toappendid` to `nodeid`, making `toappendid` a child of `nodeid`
    pub fn append_to(&mut self, nodeid: NodeId, toappendid: NodeId) -> Result<(), TreeError> {
        {
            let mut node = self.get_node_mut(nodeid)?;
            if node.children.is_none() {
                node.children = Some(Vec::new());
            }
        }

        let sibling_id = {
            let node = self.get_node_mut(nodeid)?;

            let children = node.children.as_mut().expect("Should never fail.");
            if !children.is_empty() {
                let sibling_id = *children
                    .get(children.len() - 1)
                    .expect("Should never fail.");

                children.push(toappendid);

                Some(sibling_id)
            } else {
                children.push(toappendid);
                None
            }
        };

        if let Some(sib_id) = sibling_id {
            let mut sibling = self.get_node_mut(sib_id)?;
            sibling.next_sibling = Some(toappendid);
        }

        let mut toappend = self.get_node_mut(toappendid)?;
        toappend.previous_sibling = sibling_id;
        toappend.parent = Some(nodeid);

        Ok(())
    }

    /// Appends the head of `tree` (found using `find_head()`) to `nodeid`, moving all values of `tree` into `self`
    ///
    /// Note that this will change the id of each node in `tree` (but not `self`). If successful, this returns the offset applied to each of `tree`'s `NodeId`s
    pub fn append_tree(&mut self, nodeid: NodeId, tree: &mut Tree<T>) -> Result<NodeId, TreeError> {
        //Without this out-of-bounds check, the following could happen:
        //If `nodeid` is only a valid NodeId in `self` *after* appending the nodes of `tree`, then `append_tree` will not return an error
        self.get_node(nodeid)?;

        //The offset applied on each node of `tree` so that they have the correct id post-append
        let offset = self.nodes.len().into();
        //The index of tree's head post-append
        let tree_head = tree
            .find_head()
            .expect("append_tree called when `tree` had no head")
            + offset;

        //First, update the id of all tree's nodes to match what their ids will be after appending
        for n in tree.nodes {
            n.id += offset;
        }
        //Now, append all of tree's nodes to self
        self.nodes.append(&mut tree.nodes);

        //Finally, append the tree's head to `nodeid`
        self.append_to(nodeid, tree_head);

        Ok(offset)
    }

    /// Applies a recursive operation `f` on every node on or below `n`. Note that `f` is applied to each node *before* their children
    ///
    /// Each node is passed a mutable reference to `val` during execution, which can be used to store recursion return values (such as errors)
    pub fn recurse_on_node<V, F>(&mut self, n: NodeId, f: &F, val: &mut V) -> Result<(), TreeError>
    where
        F: FnMut(&mut T, &mut V) + Clone,
    {
        let node: &mut Node<T> = self.get_node_mut(n)?;
        //Apply the method on `n` before its children
        (f)(&mut node.data, val);

        if let Some(children) = node.children {
            for c in children {
                self.recurse_on_node(c, f, val)?;
            }
        }

        Ok(())
    }
}

//TODO: If Node<T> becomes encapsulated, turn this into Iter<T>
impl<T> IntoIterator for Tree<T> {
    type Item = Node<T>;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.nodes.into_iter()
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
                        cur_node = self.get_node(*child_id).unwrap();
                        indent_level += 1;
                        found_next = true;
                    }
                }
                if !found_next {
                    if let Some(sib_id) = cur_node.next_sibling {
                        cur_node = self.get_node(sib_id).unwrap();
                        found_next = true;
                    }
                }
                if !found_next {
                    if let Some(parent_id) = cur_node.parent {
                        let parent = self.get_node(parent_id).unwrap();
                        if let Some(uncle_id) = parent.next_sibling {
                            cur_node = self.get_node(uncle_id).unwrap();
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

#[derive(Debug)]
pub struct Node<T> {
    pub id: NodeId,
    pub parent: Option<NodeId>,
    pub previous_sibling: Option<NodeId>,
    pub next_sibling: Option<NodeId>,
    pub children: Option<Vec<NodeId>>,
    pub data: T,
}

impl<T> Node<T> {
    pub fn append(&mut self, toappend: NodeId, tree: &mut Tree<T>) -> Result<(), TreeError> {
        tree.append_to(self.id, toappend)
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

//Maybe this is a bit overkill, but does provide a shorthand for unchecked getting (much like with a hashmap)
impl<T> Index<NodeId> for Tree<T> {
    type Output = Node<T>;

    fn index(&self, index: NodeId) -> &Self::Output {
        &self.nodes[index.index]
    }
}

impl<T> IndexMut<NodeId> for Tree<T> {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self.nodes[index.index]
    }
}

//When has a little bit of pointer arithmetic ever hurt anyone?
//Useful for updating node ids by an offset (for example, when appending trees)
impl Add for NodeId {
    type Output = NodeId;

    fn add(self, rhs: Self) -> Self::Output {
        (self.index + rhs.index).into()
    }
}

impl Sub for NodeId {
    type Output = NodeId;

    fn sub(self, rhs: Self) -> Self::Output {
        (self.index - rhs.index).into()
    }
}

impl AddAssign for NodeId {
    fn add_assign(&mut self, rhs: Self) {
        self.index += rhs.index;
    }
}

impl SubAssign for NodeId {
    fn sub_assign(&mut self, rhs: Self) {
        self.index -= rhs.index;
    }
}
