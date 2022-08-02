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
#[derive(Debug, Clone)]
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
            if self.nodes[i].parent.is_none() {
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
    pub fn get_node_mut_unchecked(&mut self, id: NodeId) -> &mut Node<T> {
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
            depth: 0,
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
        if a == b {
            return;
        }
        //First, swap the `NodeId`s referenced by `a` and `b` without changing the values of `a` and `b`
        {
            //This is accomplished by swapping the two values then swapping their `data` back
            //This is not performant (requires more swaps than necessary), but that should not cause problems and this is more expandable/easier
            self.nodes.swap(a.index, b.index);
            //The reason this unsafe operation is actually fine is as follows:
            //1. `a` and `b` are guaranteed to be in-bounds due to the above `self.nodes.swap(..)`
            //2. `a` and `b` cannot be the same, since there's a check at the start of the method
            unsafe {
                std::ptr::swap(&mut self[b].data, &mut self[a].data);
            }
        }
        //Then, the hard part: swap the ids referenced by `a` and `b`'s siblings so each sibling changes which node it references
        {
            //Some things to keep in mind:
            //1. If anything has the same type of relation to both `a` and `b`, there's no need to mutate it
            //  --But, if for example `foo` is a parent to `a` and a sibling to `b`, mutation is required

            //To make this easier, "dereference" `a` and `b`
            // let a = &self[a];
            // let b = &self[b];
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

        let parent_depth: u32;

        let sibling_id = {
            let node = self.get_node_mut(nodeid)?;
            parent_depth = node.depth;

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

        let old_depth = toappend.depth;

        let children_to_update: Vec<NodeId> = self.iter_subtree(toappendid).collect();
        for c in children_to_update {
            self.get_node_mut(c).unwrap().depth += parent_depth - old_depth + 1;
        }

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
        for n in &mut tree.nodes {
            n.id += offset;
        }
        //Now, append all of tree's nodes to self
        self.nodes.append(&mut tree.nodes);

        //Finally, append the tree's head to `nodeid`
        self.append_to(nodeid, tree_head)?;

        Ok(offset)
    }

    /// Applies a recursive operation `f` on every node on or below `n`. Note that `f` is applied to each node *before* their children
    ///
    /// Each node is passed a mutable reference to `val` during execution, which can be used to store recursion return values (such as errors)
    ///
    /// **BE WARNED**: ~~there be dragons~~ this method *does not* check for infinite loops at the moment
    pub fn recurse_on_node<V, F>(
        &mut self,
        n: NodeId,
        f: &mut F,
        val: &mut V,
    ) -> Result<(), TreeError>
    where
        F: FnMut(&mut T, &mut V) + Clone,
    {
        let node: &mut Node<T> = self.get_node_mut(n)?;
        //Apply the method on `n` before its children
        (f)(&mut node.data, val);

        //Iterate through the node's children and recursively apply on them
        if let Some(children) = node.children.clone() {
            for c in children {
                self.recurse_on_node(c, f, val)?;
            }
        }

        Ok(())
    }

    /// Returns an in-order iterator over a subtree that returns `NodeId`'s.
    /// Implemented non-recursively.
    ///
    /// The iterated subtree is all nodes below and including `head`.
    ///
    /// Iteration is done in-order, meaning it is depth-first, where children come after parents and children are traversed in order.
    ///
    /// To iterate over the entire tree, use `into_iter`.
    pub fn iter_subtree(&'_ self, head: NodeId) -> TreeIterator<'_, T> {
        TreeIterator::iter_subtree(self, head).unwrap()
    }
}

impl<'a, T> IntoIterator for &'a Tree<T> {
    type Item = NodeId;

    type IntoIter = TreeIterator<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        TreeIterator::iter_tree(self).unwrap()
    }
}

/// In-order iterator for `Tree<T>`.
pub struct TreeIterator<'a, T> {
    tree: &'a Tree<T>,
    head: NodeId,
    cur_node: &'a Node<T>,
    finished: bool,
}

impl<'a, T> TreeIterator<'a, T> {
    fn iter_tree(tree: &'a Tree<T>) -> Result<TreeIterator<'a, T>, TreeError> {
        Self::iter_subtree(tree, tree.find_head().unwrap())
    }

    fn iter_subtree(tree: &'a Tree<T>, head: NodeId) -> Result<TreeIterator<'a, T>, TreeError> {
        Ok(TreeIterator {
            tree,
            head,
            cur_node: tree.get_node(head)?,
            finished: false,
        })
    }
}

impl<'a, T> Iterator for TreeIterator<'a, T> {
    type Item = NodeId;
    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        let ret_id = self.cur_node.id;

        // Keep track of whether we've found another node in the tree
        let mut found_next = false;

        // If this node has children, move to the first of those children.
        if let Some(children) = &self.cur_node.children {
            let child = children.get(0);
            if let Some(child_id) = child {
                self.cur_node = self.tree.get_node(*child_id).unwrap();
                found_next = true;
            }
        }

        // If this node doesn't have children but has a sibling following it, move to that sibling.
        // Otherwise, if this node has a parent, move to the parent to check for uncles.
        // Loop until either an uncle (grand-uncle, grand-grand-uncle, etc.) is found or there are no more parents.
        while !found_next {
            if self.cur_node.id == self.head {
                self.finished = true;
                found_next = true;
                break;
            }
            if let Some(sib_id) = self.cur_node.next_sibling {
                self.cur_node = self.tree.get_node(sib_id).unwrap();
                found_next = true;
                break;
            }
            if let Some(parent_id) = self.cur_node.parent {
                self.cur_node = self.tree.get_node(parent_id).unwrap();
            } else {
                break;
            }
        }

        if found_next {
            Some(ret_id)
        } else {
            self.finished = true;
            None
        }
    }
}

impl<T: std::fmt::Debug> fmt::Display for Tree<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for id in self {
            let node = self.get_node(id).unwrap();
            f.write_fmt(format_args!(
                "{}{:?} \n",
                "  ".repeat(node.depth.try_into().unwrap()),
                node.data
            ))?;
        }

        return write!(f, "");
    }
}

/// Generic node struct that stores references to siblings, parents, and children as well as the node's data.
#[derive(Debug, Clone)]
pub struct Node<T> {
    /// This node's ID.
    pub id: NodeId,

    /// This node's parent.
    pub parent: Option<NodeId>,

    /// The previous sibling in this node's parent's ordered list of children.
    pub previous_sibling: Option<NodeId>,

    /// The next sibling in this node's parent's ordered list of children.
    pub next_sibling: Option<NodeId>,

    /// An ordered list of this node's children.
    pub children: Option<Vec<NodeId>>,

    /// The depth of this node, with `0` being the tree's root, `1` being the root's children, etc.
    pub depth: u32,

    /// The non-positional data of this node. This may simply be a label or contain more information.
    ///
    /// For example, an addition node needs no additional data besides the fact that it's an addition node, but a variable reference node needs to store the name of the variable.
    pub data: T,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
