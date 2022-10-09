use std::{
    collections::HashMap,
    fmt,
    ops::{Add, AddAssign, Index, IndexMut, Sub, SubAssign},
};

#[derive(Debug, Clone)]
pub enum TreeError {
    NodeNotFound { node_id: NodeId },
    ChildrenExpected { node_id: NodeId },
    ChildNotFound { node_id: NodeId, child_index: usize },
}

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

    /// Creates a new tree and populates it with nodes using the parameter `nodes`.
    /// The parameter lists each node's data and children
    ///
    /// Currently, this depends on `new_node` creating sequential indices, starting from 0
    pub fn new_with_nodes(mut nodes: Vec<(T, Vec<NodeId>)>) -> Result<Tree<T>, TreeError> {
        let mut t = Tree::new();

        //Holds the children of each given node
        let mut node_children: HashMap<NodeId, Vec<NodeId>> = HashMap::new();

        //Populate node_children and create every node
        while nodes.len() > 0 {
            let (data, children) = nodes.remove(0);
            let node = t.new_node(data);
            node_children.insert(node, children);
        }

        //Set up relationships between each parent and their children
        for (parent, children) in node_children {
            for child in children {
                t.append_to(parent, child)?;
            }
        }

        Ok(t)
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

    /// Shorthand to get an immutable reference to one of a node's children
    pub fn get_child_node(&self, id: NodeId, child_index: usize) -> Result<&Node<T>, TreeError> {
        let node = self.get_node(id)?;
        let child_id = node
            .children
            .get(child_index)
            .ok_or(TreeError::ChildNotFound {
                node_id: id,
                child_index,
            })?;
        self.get_node(*child_id)
    }

    pub fn get_first_child(&self, id: NodeId) -> Result<&Node<T>, TreeError> {
        self.get_child_node(id, 0)
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
            children: Vec::new(),
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
        let parent_depth: u32;

        let sibling_id = {
            let node = self.get_node_mut(nodeid)?;
            parent_depth = node.depth;

            let children = &mut node.children;
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

        fn add_offset_vec(vals: &mut Vec<NodeId>, offset: NodeId) {
            for v in vals {
                *v += offset;
            }
        }

        fn add_offset(val: &mut Option<NodeId>, offset: NodeId) {
            if let Some(val) = val {
                *val += offset;
            }
        }

        //First, update the id of all tree's nodes to match what their ids will be after appending
        for n in &mut tree.nodes {
            n.id += offset;
            add_offset_vec(&mut n.children, offset);
            add_offset(&mut n.parent, offset);
            add_offset(&mut n.next_sibling, offset);
            add_offset(&mut n.previous_sibling, offset);
        }
        //Now, append all of tree's nodes to self
        self.nodes.append(&mut tree.nodes);

        //Finally, append the tree's head to `nodeid`
        self.append_to(nodeid, tree_head)?;

        Ok(offset)
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

    /// Simulates recursion using iteration and calls the provided method on each node starting at the head
    ///
    /// The method acts on the following data: the tree itself, the index of each node, and the values returned by each node's children.
    /// It also uses some static value of type `&C` provided by the user if needed
    ///
    /// If any call to the function returns an error, stop the recursion.
    pub fn recurse_iterative<V, E, C, F>(&self, head: NodeId, f: F, static_val: &C) -> Result<V, E>
    where
        F: Fn(&Tree<T>, NodeId, Vec<&V>, &C) -> Result<V, E>,
    {
        //Tentative algorithm (to be optimized):
        //1-Define a frontier which contains the deepest children of `head`
        //2-Until the frontier is just `head`, do the following for every node in the frontier:
        //-a-If the parent of this node is contained in the frontier, remove this node from the frontier
        //-b-If every sibling of this node is contained in the frontier, calculate the parent's value and add it to the frontier

        //Step 1
        let mut frontier = HashMap::new();

        for n in self.iter_subtree(head) {
            //If `n` has no children, it is part of the deepest children (which is the starting value of the frontier)
            if self[n].children.is_empty() {
                frontier.insert(n, (f)(&self, n, Vec::new(), static_val)?);
            }
        }

        //Step 2
        loop {
            let frontier_nodes = frontier.keys().map(|n| *n).collect::<Vec<NodeId>>();
            'frontier: for n in frontier_nodes {
                let n = &self[n];
                let parent = &self[n.parent.unwrap()];

                //2.a)
                if frontier.contains_key(&parent.id) {
                    frontier.remove(&n.id);
                }

                //2.b)
                let mut sibling_values = Vec::new();
                for sibling in &parent.children.clone() {
                    if let Some(v) = frontier.get(sibling) {
                        sibling_values.push(v);
                    } else {
                        continue 'frontier;
                    }
                }

                let parent_val = (f)(&self, parent.id, sibling_values, static_val)?;
                frontier.insert(parent.id, parent_val);
            }

            //If the frontier contains the `head` node, then return its value
            if let Some(head_val) = frontier.remove(&head) {
                return Ok(head_val);
            }
        }
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
        if !self.cur_node.children.is_empty() {
            let child_id = self.cur_node.children[0];
            self.cur_node = self.tree.get_node(child_id).unwrap();
            found_next = true;
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
    pub children: Vec<NodeId>,

    /// The depth of this node, with `0` being the tree's root, `1` being the root's children, etc.
    pub depth: u32,

    /// The non-positional data of this node. This may simply be a label or contain more information.
    ///
    /// For example, an addition node needs no additional data besides the fact that it's an addition node, but a variable reference node needs to store the name of the variable.
    pub data: T,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
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
