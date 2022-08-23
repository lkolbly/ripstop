use crate::ast::{StringContext, Type};
use std::collections::{HashMap, HashSet};

use crate::{
    ast::{ASTNode, ASTNodeType},
    ir::{BinaryOperator, Expression, Module, UnaryOperator},
    tree::{NodeId, Tree, TreeError},
    verilog_ast::{AlwaysBeginTriggerType, VNode},
};

// fn normalize(tree: &mut Tree<ASTNode>) -> Result<(), CompileError> {
//     let variables = get_referenced_variables_with_highest_and_lowest_t_offset(tree)?;

//     for n in tree.into_iter().collect::<Vec<NodeId>>() {
//         if let ASTNodeType::VariableReference { var_id, t_offset } = &mut tree[n].data.node_type {
//             if let Some((lowest_offset, _highest_offset)) = variables.get(var_id) {
//                 *t_offset -= lowest_offset;
//             } else {
//                 return Err(CompileError::UndeclaredVariable {
//                     context: tree[n].data.context.clone(),
//                 });
//             }
//         }
//     }

//     Ok(())
// }

/// Conveniently stores information about the range of time values at which a variable is referenced.
#[derive(Debug, Clone)]
struct VarBounds {
    lowest_ref: i64,
    highest_ref: i64,
    highest_assignment: i64,
    var_scope: VarScope,
    var_type: Type,
}

#[derive(Debug, Clone, Copy)]
enum VarScope {
    Input,
    Output,
    Local,
}

impl VarBounds {
    fn new(offset: i64, var_scope: VarScope, var_type: Type) -> VarBounds {
        VarBounds {
            lowest_ref: offset,
            highest_ref: offset,
            highest_assignment: offset,
            var_scope,
            var_type,
        }
    }

    /// Update `lowest_ref` and `highest_ref` with a new offset
    fn update(&mut self, offset: i64) {
        self.lowest_ref = self.lowest_ref.min(offset);
        self.highest_ref = self.highest_ref.max(offset);
    }

    /// Update `highest_assignment` with a new offset
    fn update_assignment(&mut self, offset: i64) {
        self.highest_assignment = self.highest_assignment.max(offset);
    }
}

fn get_node_type(
    tree: &Tree<ASTNode>,
    this_node: NodeId,
    child_vals: Vec<&Type>,
    variables: &HashMap<String, VarBounds>,
) -> Result<Type, CompileError> {
    let this_node = &tree[this_node];
    let child_vals: Vec<Type> = child_vals.iter().map(|&x| x.clone()).collect();

    match &this_node.data.node_type {
        //Variable references pass their types up without needing to check their (non-existent) children
        ASTNodeType::VariableReference { var_id } => {
            let bounds = &variables[var_id];
            Ok(bounds.var_type)
        }
        //Indexing restricts the type and only works on arrays/bits<n> when `n >= high >= low >= 0`. Due to type limits, `low >= 0` is guaranteed
        ASTNodeType::Index { high, low } => {
            if let Type::Bits { size } = child_vals[0] {
                if low > high || *high > size {
                    Err(CompileError::IndexOutOfBounds {
                        context: this_node.data.context.clone(),
                    })
                } else {
                    Ok(Type::Bits {
                        size: high - low + 1,
                    })
                }
            } else {
                Err(CompileError::CannotIndexType {
                    context: this_node.data.context.clone(),
                    invalid_type: child_vals[0],
                })
            }
        }
        //Unary operators don't need to worry about type matching and transformation
        ASTNodeType::BitwiseInverse => Ok(child_vals[0]),
        //Binary operators require valid types, and theoretically this could allow for different types on each side
        //For example, if matrix multiplication was added, then as long as `lhs` was a matrix, `rhs` could be matrix or scalar
        ASTNodeType::Add | ASTNodeType::Subtract => {
            //The error which will be returned if a type mismatch occurs
            let err = Err(CompileError::MismatchedTypes {
                context: tree[this_node.children.clone().unwrap()[0]]
                    .data
                    .context
                    .clone(),
                current_type: child_vals[1],
                needed_type: child_vals[0],
            });

            match (child_vals[0], child_vals[1]) {
                (Type::Bit, Type::Bit) => Ok(Type::Bit),
                (Type::Bits { size: lhs_size }, Type::Bits { size: rhs_size }) => {
                    if lhs_size == rhs_size {
                        Ok(Type::Bits { size: lhs_size })
                    } else {
                        err
                    }
                }
                _ => err,
            }
        }
        //Time offsets need to be allowed but do *not* pass any type, they are typeless
        //Since this can't be represented, just return a type of bits<0>
        ASTNodeType::TimeOffsetRelative { offset: _ }
        | ASTNodeType::TimeOffsetAbsolute { time: _ } => Ok(Type::None),

        ASTNodeType::NumberLiteral(literal) => {
            if literal.size_bits == 1 {
                Ok(Type::Bit)
            } else {
                Ok(Type::Bits {
                    size: literal.size_bits,
                })
            }
        }

        //Invalid nodes (which do not work as expressions in assignment) will be expressed as a bad relationship between this node and its children
        _ => {
            //Start with this node
            let mut v = vec![this_node.data.clone()];
            //Add the children
            v.append(
                &mut this_node
                    .children
                    .clone()
                    .unwrap_or_default()
                    .into_iter()
                    .map(|id| tree[id].data.clone())
                    .collect(),
            );
            Err(CompileError::InvalidNodeTypes { nodes: v })
        }
    }
}

/// Currently, this takes the input tree and does the following:
/// * Verifies that types in assignments and expressions match up correctly (i.e. verifies types)
fn verify(
    tree: &Tree<ASTNode>,
    variables: &HashMap<String, VarBounds>,
) -> Result<(), CompileError> {
    //Type verification, done bottom-up, keeping track of each nodes' return type
    //Done "recursively" for each assignment
    for n in tree {
        #[allow(clippy::single_match)] // Just for now, to silence the warning
        match &tree[n].data.node_type {
            ASTNodeType::Assign => {
                let children = tree[n].children.clone().unwrap();
                let lhs_t = tree.recurse_iterative(children[0], get_node_type, variables)?;
                let rhs_t = tree.recurse_iterative(children[1], get_node_type, variables)?;

                // Coerce bits<1> to bit
                let lhs_t = match lhs_t {
                    Type::Bits { size: 1 } => Type::Bit,
                    x => x,
                };
                let rhs_t = match rhs_t {
                    Type::Bits { size: 1 } => Type::Bit,
                    x => x,
                };

                if lhs_t != rhs_t {
                    let rhs = &tree[children[1]];
                    return Err(CompileError::MismatchedTypes {
                        context: rhs.data.context.clone(),
                        current_type: rhs_t,
                        needed_type: lhs_t,
                    });
                }
            }
            _ => (),
        }
    }
    Ok(())
}

/// Returns the verilog equivalent of adding together `lhs` and `rhs`. At the moment, this supports addition between two nodes of type `VariableReference`
///
/// The head of the tree this returns will be a variant of `VNode::Add`
/*fn add(
    tree: &Tree<ASTNode>,
    variables: &HashMap<String, VarBounds>,
    lhs_id: NodeId,
    rhs_id: NodeId,
) -> Result<Tree<VNode>, CompileError> {
    let lhs = &tree[lhs_id].data;
    let rhs = &tree[rhs_id].data;
    //Make sure both nodes are var references
    match (&lhs.node_type, &rhs.node_type) {
        (
            ASTNodeType::VariableReference {
                var_id: var_id_l,
                t_offset: t_offset_l,
            },
            ASTNodeType::VariableReference {
                var_id: var_id_r,
                t_offset: t_offset_r,
            },
        ) => {
            let mut tree = Tree::new();
            let add = tree.new_node(VNode::Add {});

            let lhs_bounds = variables.get(var_id_l).unwrap();
            let rhs_bounds = variables.get(var_id_r).unwrap();

            match (&lhs_bounds.var_type, &rhs_bounds.var_type) {
                (Type::Bit, Type::Bit) => todo!(),
                (Type::Bits { size: lhs_size }, Type::Bits { size: rhs_size }) => todo!(),
                _ => {
                    return Err(CompileError::MismatchedTypes {
                        context: rhs.context.clone(),
                        current_type: rhs_bounds.var_type,
                        needed_type: lhs_bounds.var_type,
                    })
                }
            }

            Ok(tree)
        }
        _ => Err(CompileError::InvalidNodeTypes {
            nodes: vec![lhs.clone(), rhs.clone()],
        }),
    }
}*/
//PROBLEM:
//Due to the solution used to fix the below error, unused variables will have a register assigned to them. This can be fixed by an optimization step
//--Undeclared variables used to panic on an `unwrap()` of `Option<VarBounds>` right before the method returned

/// Returns a list of all variables referenced in the input AST and the lowest *and* highest t-values referenced for each variable. This is accomplished recursively
///
/// Variables that are inputs or outputs will always contain 0 within the closed range \[lowest, highest\].
///
/// Result is given as a hashmap mapping variable names (strings) to `VarBounds` structs.
fn get_var_bounds(tree: &Tree<ASTNode>) -> Result<HashMap<String, VarBounds>, CompileError> {
    //A list of all the variables and their t-offsets. If the variable has only been declared (neither referenced or assigned), then the t-offset will be `None`
    //Reminder: a positive t-offset represents [t+n] and a negative represents [t-n]
    let mut variables: HashMap<String, VarBounds> = HashMap::new();

    //A closure taking information about a var reference and inserting that data into `variables` if needed
    let insert = |var_id: &str,
                  nodeid: NodeId,
                  offset: i64,
                  is_assignment: bool,
                  variables: &mut HashMap<String, VarBounds>|
     -> Result<(), CompileError> {
        if let Some(bounds) = variables.get_mut(var_id) {
            // Update the stored bounds with this new offset
            bounds.update(offset);
            if is_assignment {
                bounds.update_assignment(offset);
            }
            Ok(())
        } else {
            Err(CompileError::UndeclaredVariable {
                context: tree[nodeid].data.context.clone(),
            })
        }
    };

    for nodeid in tree {
        match &tree[nodeid].data.node_type {
            ASTNodeType::ModuleDeclaration {
                id: _,
                in_values,
                out_values,
            } => {
                // I/O variables are guaranteed a reference at offset 0
                for (t, name) in in_values {
                    variables.insert(name.clone(), VarBounds::new(0, VarScope::Input, *t));
                }
                for (t, name) in out_values {
                    variables.insert(name.clone(), VarBounds::new(0, VarScope::Output, *t));
                }
            }
            ASTNodeType::VariableReference { var_id } => {
                let t_offset = &tree.get_first_child(nodeid).unwrap().data.node_type;
                if let ASTNodeType::TimeOffsetRelative { offset } = t_offset {
                    insert(var_id, nodeid, *offset, false, &mut variables)?
                }
            }
            ASTNodeType::VariableDeclaration { var_type, var_id } => {
                //Never referenced variables have an offset of 0 by default
                variables.insert(
                    var_id.clone(),
                    VarBounds::new(0, VarScope::Local, *var_type),
                );
            }
            ASTNodeType::Assign => {
                let lhs = tree.get_node(nodeid).unwrap().children.as_ref().unwrap()[0];
                if let ASTNodeType::VariableReference { var_id } =
                    &tree.get_node(lhs).unwrap().data.node_type
                {
                    let t_offset = &tree.get_first_child(nodeid).unwrap().data.node_type;
                    if let ASTNodeType::TimeOffsetRelative { offset } = t_offset {
                        insert(var_id, nodeid, *offset, true, &mut variables)?;
                    }
                }
            }
            _ => {}
        }
    }

    Ok(variables)
}

fn compile_ir_expression(expr: &Expression) -> Result<Tree<VNode>, CompileError> {
    let mut vast = Tree::new();

    match expr {
        Expression::BinaryOperation {
            operation,
            lhs,
            rhs,
        } => {
            let node = vast.new_node(match operation {
                BinaryOperator::Addition => VNode::Add {},
            });
            let mut lhs = compile_ir_expression(lhs)?;
            let mut rhs = compile_ir_expression(rhs)?;
            vast.append_tree(node, &mut lhs);
            vast.append_tree(node, &mut rhs);
        }
        Expression::UnaryOperation {
            operation,
            operatee,
        } => {
            let node = vast.new_node(match operation {
                UnaryOperator::Negation => VNode::BitwiseInverse {},
                UnaryOperator::Index(range) => VNode::Index {
                    high: range.high as usize,
                    low: range.low as usize,
                },
            });
            let mut lhs = compile_ir_expression(operatee)?;
            vast.append_tree(node, &mut lhs)?;
        }
        Expression::Ternary {
            condition,
            lhs,
            rhs,
        } => {
            let mut condition = compile_ir_expression(condition)?;
            let node = vast.new_node(VNode::Ternary {});
            let mut lhs = compile_ir_expression(lhs)?;
            let mut rhs = compile_ir_expression(rhs)?;
            vast.append_tree(node, &mut condition);
            vast.append_tree(node, &mut lhs);
            vast.append_tree(node, &mut rhs);
        }
        Expression::VariableReference(reference) => {
            // Get t-offset
            let t_offset = if let crate::ir::TimeReference::Relative(offset) = reference.time {
                offset
            } else {
                panic!("Reset value as RHS?");
            };

            // TODO: Ensure t-offset is not greater than the highest t-offset at which this variable is assigned
            /*if let ASTNodeType::TimeOffsetRelative { offset } = t_offset {
                if offset > &bounds.highest_assignment {
                    return Err(CompileError::ReferenceAfterAssignment {
                        context: ast.get_node(var_ref).unwrap().data.context.clone(),
                    });
                }
            }*/

            let vref = variable_name_relative(&reference.variable, t_offset);
            let node = vast.new_node(VNode::VariableReference { var_id: vref });
        }
        Expression::NumberLiteral(literal) => {
            vast.new_node(VNode::NumberLiteral { literal: *literal });
        }
        _ => {
            unimplemented!();
        }
    }

    Ok(vast)
}

fn compile_expression(
    ast: &Tree<ASTNode>,
    //The current node in `ast` used to generate a subtree for `vast`
    node: NodeId,
    //vast: &mut Tree<VNode>,
    //The node in `vast` to append the generated subtree to, if specified
    //vnode: Option<NodeId>,
    variables: &HashMap<String, VarBounds>,
    // Time to shift by
    time_shift: i64,
) -> Result<(Option<NodeId>, Tree<VNode>), CompileError> {
    /// Adds a new node to `vast` with the given parent and returns the node's id
    fn add_node(
        vast: &mut Tree<VNode>,
        node: VNode,
        parent: Option<NodeId>,
    ) -> Result<Option<NodeId>, CompileError> {
        let node = vast.new_node(node);
        if let Some(parent) = parent {
            vast.append_to(parent, node)?;
        }

        Ok(Some(node))
    }

    let mut vast = Tree::new();

    // The parent of this node's compiled children. If multiple VNodes are generated from one ASTNode, then this is the node where children are attached
    let new_vnode = match &ast.get_node(node).unwrap().data.node_type {
        ASTNodeType::VariableReference { var_id: _ } => {
            //First, generate the subtree with both a t offset and implicit index enabled
            let mut var_ref_subtree =
                compile_var_ref(ast, node, variables, true, true, time_shift)?;

            vast = var_ref_subtree;

            //The bottom of the subtree, where the children of this node will be added
            let mut var_ref_subtree_bottom = None;
            for n in &vast {
                if let None = vast[n].children {
                    var_ref_subtree_bottom = Some(n);
                    break;
                }
            }

            //Finally, append the generated subtree to `vast` and apply the offset accordingly
            //if let Some(parent) = vnode {
            /*if let Some(var_ref_subtree_bottom) = &mut var_ref_subtree_bottom {
                *var_ref_subtree_bottom += vast
                    .append_tree(parent, &mut var_ref_subtree)
                    .map_err(|err| CompileError::from(err))?;
            }*/
            //}

            Ok(var_ref_subtree_bottom)
        }
        ASTNodeType::BitwiseInverse => add_node(&mut vast, VNode::BitwiseInverse {}, None),
        ASTNodeType::Add => add_node(&mut vast, VNode::Add {}, None),
        ASTNodeType::Subtract => add_node(&mut vast, VNode::Subtract {}, None),
        ASTNodeType::TimeOffsetRelative { offset: _ } => Ok(None),
        ASTNodeType::Index { high, low } => add_node(
            &mut vast,
            VNode::Index {
                high: *high,
                low: *low,
            },
            None,
        ),
        ASTNodeType::NumberLiteral(literal) => {
            add_node(&mut vast, VNode::NumberLiteral { literal: *literal }, None)
        }
        _ => unimplemented!(),
    };

    let new_vnode = new_vnode?;

    //Compile all the children expressions
    if let Some(new_vnode) = new_vnode {
        if let Some(children) = &ast.get_node(node).unwrap().children {
            for child in children {
                let (node, mut child_tree) =
                    compile_expression(ast, *child, variables, time_shift)?;
                if let Some(_) = node {
                    vast.append_tree(new_vnode, &mut child_tree)?;
                }
            }
        }
    }

    Ok((new_vnode, vast))
}

/// Compiles the supplied VariableReference into a suitable Verilog subtree
///
/// Some notes:
/// * if `add_implicit_index == true`, then an index will be added above the variable reference *only* if there is none already
/// * if `var_ref` is not actually a `VariableReference`, then an `InvalidNodeTypes` error will be raised on `var_ref`
fn compile_var_ref(
    ast: &Tree<ASTNode>,
    var_ref: NodeId,
    variables: &HashMap<String, VarBounds>,
    include_t_offset: bool,
    add_implicit_index: bool,
    time_shift: i64,
) -> Result<Tree<VNode>, CompileError> {
    if let ASTNodeType::VariableReference { var_id } = &ast[var_ref].data.node_type {
        let bounds = variables.get(var_id).unwrap();

        // The var_id adjusted for t-offset if specified
        let var_id = {
            if include_t_offset {
                // Get t-offset
                let t_offset_node = ast.get_first_child(var_ref).unwrap();
                let t_offset = &t_offset_node.data.node_type;

                // Ensure t-offset is not greater than the highest t-offset at which this variable is assigned
                if let ASTNodeType::TimeOffsetRelative { offset } = t_offset {
                    if offset > &bounds.highest_assignment {
                        return Err(CompileError::ReferenceAfterAssignment {
                            context: ast.get_node(var_ref).unwrap().data.context.clone(),
                        });
                    }
                }

                variable_name(var_id, ast, t_offset_node.id, time_shift)
            } else {
                var_id.clone()
            }
        };

        let mut vast = Tree::new();

        //The parent of the var ref which will be added to the Verilog AST. By default, this is None and will only change when adding an implicit index
        let v_var_ref_parent = {
            if add_implicit_index {
                if let Some(var_ref_parent) = ast[var_ref].parent {
                    match &ast[var_ref_parent].data.node_type {
                        //If the current variable reference is already indexed, no implicit index should be added
                        ASTNodeType::Index { .. } => None,
                        //If the current variable reference is not already indexed, add an implicit index.
                        _ => {
                            //But, only add an implicit index to array types
                            match bounds.var_type {
                                Type::Bits { size } => {
                                    //Create the index node and add it to the tree
                                    let idx = vast.new_node(VNode::Index {
                                        high: size - 1,
                                        low: 0,
                                    });

                                    Some(idx)
                                }
                                _ => None,
                            }
                        }
                    }
                } else {
                    None
                }
            } else {
                None
            }
        };

        let v_var_ref = vast.new_node(VNode::VariableReference { var_id });

        if let Some(v_var_ref_parent) = v_var_ref_parent {
            vast.append_to(v_var_ref_parent, v_var_ref)?;
        }

        Ok(vast)
    } else {
        Err(CompileError::InvalidNodeTypes {
            nodes: vec![ast[var_ref].data.clone()],
        })
    }
}

/// Compiles the supplied VariableReference into a suitable Verilog subtree without information from the ast itself other than the variable name .
/// This makes it significantly more primative while still being suitable for some applications
///
/// Some notes:
/// * if `index` is a variant of `VNode` other than `VNode::Index {..}`, this method has undefined behavior
fn compile_var_ref_from_string(
    var_id: &str,
    t_offset: Option<i64>,
    index: Option<VNode>,
) -> Result<Tree<VNode>, TreeError> {
    let mut tree = Tree::new();

    //Creating the `VariableReference`
    let var_id = match t_offset {
        Some(offset) => variable_name_relative(var_id, offset),
        None => var_id.to_string(),
    };
    let var_ref = tree.new_node(VNode::VariableReference { var_id: var_id });

    //Creating the index if needed
    if let Some(idx) = index {
        let idx = tree.new_node(idx);
        tree.append_to(idx, var_ref)?;
    }

    Ok(tree)
}

#[derive(Clone)]
pub enum CompileError {
    CouldNotFindASTHead,
    TreeError {
        err: TreeError,
    },
    UndeclaredVariable {
        context: StringContext,
    },
    ReferenceAfterAssignment {
        context: StringContext,
    },
    AssignmentInPast {
        context: StringContext,
    },
    InputAssignment {
        context: StringContext,
    },
    /// When a variable/literal/etc has one type but should have another type in order to compile
    MismatchedTypes {
        context: StringContext,
        current_type: Type,
        needed_type: Type,
    },
    IndexOutOfBounds {
        context: StringContext,
    },
    CannotIndexType {
        context: StringContext,
        invalid_type: Type,
    },
    /// When a set of nodes have types which do not make sense in relation to eachother.
    /// For example, this error will be raised when adding a `ModuleDeclaration` to a `VariableReference` and it will contain data for those two nodes
    InvalidNodeTypes {
        nodes: Vec<ASTNode>,
    },
}

impl From<TreeError> for CompileError {
    fn from(e: TreeError) -> Self {
        CompileError::TreeError { err: e }
    }
}

impl std::fmt::Debug for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut include_position =
            |ctx: &StringContext, msg: &str| write!(f, "{} on {}", msg, ctx,);
        match self {
            CompileError::CouldNotFindASTHead => write!(f, "Could not find AST head."),
            CompileError::TreeError { err } => write!(f, "Tree error: {:?}", err),
            CompileError::UndeclaredVariable { context } => {
                include_position(context, "Undeclared variable")
            }
            CompileError::ReferenceAfterAssignment { context } => {
                include_position(context, "Reference too late (you cannot reference a variable at a time offset greater than when it's assigned)")
            }
            CompileError::AssignmentInPast { context } => {
                include_position(context, "Assignment in past (you cannot assign a variable in the past)")
            }
            CompileError::InputAssignment { context } => {
                include_position(context, "Assigning to an input value")
            }
            CompileError::MismatchedTypes { context, current_type, needed_type } => {
                include_position(context, &format!("Mismatched types: provided `{}` but should have been `{}`", current_type, needed_type))
            },
            CompileError::InvalidNodeTypes { nodes } => {
                let mut nodes_string = String::new();
                for n in nodes {
                    nodes_string += &format!("{}\n", n.context);
                }
                write!(f, "The following nodes have invalid types given their relationship:\n{}", nodes_string)
            },
            CompileError::IndexOutOfBounds { context } => include_position(context, "Index out of bounds"),
            CompileError::CannotIndexType { context, invalid_type } => include_position(context, &format!("Cannot index type {}", invalid_type)),
        }
    }
}

#[derive(Debug)]
struct AssignmentSet {
    /// The tuple is (time offset, is combinatorial, tree)
    assignments: HashMap<String, (i64, bool, Tree<VNode>)>,
    reset_values: HashMap<String, NodeId>,
}

impl AssignmentSet {
    fn new() -> Self {
        Self {
            assignments: HashMap::new(),
            reset_values: HashMap::new(),
        }
    }

    #[must_use]
    fn merge(&mut self, mut other: Self) -> Vec<CompileError> {
        let mut errors = vec![];

        for (var_id, var) in other.assignments.drain() {
            if self.assignments.contains_key(&var_id) {
                // TODO: Append to errors
                panic!("Can't assign variables multiple times");
            }
            self.assignments.insert(var_id, var);
        }

        for (var_id, var) in other.reset_values.drain() {
            if self.reset_values.contains_key(&var_id) {
                // TODO: Append to errors
                panic!("Can't set reset value multiple times");
            }
            self.reset_values.insert(var_id, var);
        }

        errors
    }
}

fn compile_assign_node(
    tree: &Tree<ASTNode>,
    node: &crate::tree::Node<ASTNode>,
    variables: &HashMap<String, VarBounds>,
    is_input: impl Fn(&str) -> bool,
) -> Result<AssignmentSet, CompileError> {
    let lhs = node.children.as_ref().unwrap()[0];
    let rhs = node.children.as_ref().unwrap()[1];

    let is_combinatorial = tree.recurse_iterative::<_, CompileError, _, _>(
        rhs,
        |tree, node, children, _| {
            // If any children returned true, we're true
            if children.iter().any(|x| **x) {
                return Ok(true);
            }

            match &tree.get_node(node).unwrap().data.node_type {
                ASTNodeType::TimeOffsetRelative { offset } => Ok(*offset == 0),
                _ => Ok(false),
            }
        },
        &(),
    )?;

    let mut assignments = AssignmentSet::new();

    match &tree.get_node(lhs).unwrap().data.node_type {
        ASTNodeType::VariableReference { var_id } => {
            //Can't assign to an input
            if is_input(var_id) {
                return Err(CompileError::InputAssignment {
                    context: tree.get_node(lhs).unwrap().data.context.clone(),
                });
            }

            match tree.get_first_child(lhs)?.data.node_type {
                ASTNodeType::TimeOffsetRelative { offset } => {
                    if offset < 0 {
                        return Err(CompileError::AssignmentInPast {
                            context: tree.get_node(lhs).unwrap().data.context.clone(),
                        });
                    }
                    /*if assignments.assignments.contains_key(var_id) {
                        panic!("Tried assigning to a variable multiple times!");
                    }*/
                    let (_, rhs_tree) = compile_expression(
                        tree,
                        rhs,
                        &variables,
                        if is_combinatorial { 0 } else { 1 },
                    )?;
                    assignments
                        .assignments
                        .insert(var_id.to_owned(), (offset, is_combinatorial, rhs_tree));
                }
                ASTNodeType::TimeOffsetAbsolute { time } => {
                    // TODO: We should throw an error according to issue #15 if the user uses the wrong time
                    if time != 0 && time != -1 {
                        panic!("May only assign absolute times at t=0!");
                    }
                    /*if reset_values.contains_key(var_id) {
                        panic!("Tried setting a reset value for a variable multiple times!");
                    }*/
                    assignments.reset_values.insert(var_id.to_owned(), rhs);
                }
                _ => panic!("Unexpected node type"),
            }
        }
        _ => panic!("Should be unreachable!"),
    }

    Ok(assignments)
}

fn compile_condition(
    tree: &Tree<ASTNode>,
    node: NodeId,
    variables: &HashMap<String, VarBounds>,
) -> Result<Tree<VNode>, CompileError> {
    let is_combinatorial = tree.recurse_iterative::<_, CompileError, _, _>(
        node,
        |tree, node, children, _| {
            // If any children returned true, we're true
            if children.iter().any(|x| **x) {
                return Ok(true);
            }

            match &tree.get_node(node).unwrap().data.node_type {
                ASTNodeType::TimeOffsetRelative { offset } => Ok(*offset == 0),
                _ => Ok(false),
            }
        },
        &(),
    )?;

    let (_, tree) =
        compile_expression(tree, node, variables, if is_combinatorial { 0 } else { 1 })?;
    Ok(tree)
}

fn compile_conditional_chain_recursive(
    tree: &Tree<ASTNode>,
    children: &[NodeId],
    variables: &HashMap<String, VarBounds>,
    in_values: &Vec<(String, usize)>,
) -> Result<AssignmentSet, CompileError> {
    let is_input = |name: &str| in_values.iter().any(|(in_name, _)| in_name == name);
    //let mut assignments = AssignmentSet::new();

    assert!(children.len() >= 1);

    if let ASTNodeType::Block = tree.get_node(children[0]).unwrap().data.node_type {
        // Actually, this is the final else
        println!("Found else");
        if children.len() > 1 {
            panic!("There are more children beyond the else!");
        }
        let block = compile_block_to_assignments(
            tree,
            tree.get_node(children[0]).unwrap(),
            variables,
            in_values,
        )?;
        println!("Assignments: {:?}", block);

        Ok(block)
    } else {
        if children.len() < 2 {
            panic!("Not enough children!");
        }
        let condition = compile_condition(tree, children[0], variables)?;
        let block = children[1];
        let block_node = tree.get_node(block).unwrap();
        if let ASTNodeType::Block = block_node.data.node_type {
            // This is a block, as we expect
        } else {
            panic!("Node following condition was not a Block");
        }
        let iftrue = compile_block_to_assignments(tree, block_node, variables, in_values)?;
        println!("Condition:\n{}", condition);
        println!("If true: {:?}", iftrue);

        let iffalse =
            compile_conditional_chain_recursive(tree, &children[2..], variables, in_values)?;

        // For each variable in either assignment set, build a verilog ternary
        let full_variable_set: HashSet<_> = iftrue
            .assignments
            .iter()
            .chain(iffalse.assignments.iter())
            .map(|(k, v)| k)
            .collect();
        let mut assignments = AssignmentSet::new();
        for var in full_variable_set.iter() {
            // TODO: If the conditional is entirely combinatorial, then all branches MUST be specified
            let mut iftrue = iftrue
                .assignments
                .get(*var)
                .map(|x| x.to_owned())
                .unwrap_or_else(|| {
                    (
                        0,
                        false,
                        compile_var_ref_from_string(var, Some(0), None).unwrap(),
                    )
                });
            let mut iffalse = iffalse
                .assignments
                .get(*var)
                .map(|x| x.to_owned())
                .unwrap_or_else(|| {
                    (
                        0,
                        false,
                        compile_var_ref_from_string(var, Some(0), None).unwrap(),
                    )
                });

            let mut ternary_tree = Tree::new();
            let ternary = ternary_tree.new_node(VNode::Ternary {});
            let mut condition = condition.clone();
            ternary_tree.append_tree(ternary, &mut condition)?;
            ternary_tree.append_tree(ternary, &mut iftrue.2)?;
            ternary_tree.append_tree(ternary, &mut iffalse.2)?;

            assignments.assignments.insert(
                var.to_string(),
                (
                    std::cmp::max(iftrue.0, iffalse.0),
                    iftrue.1 || iffalse.1,
                    ternary_tree,
                ),
            );
        }

        Ok(assignments)
    }
}

fn compile_conditional_node(
    tree: &Tree<ASTNode>,
    node: &crate::tree::Node<ASTNode>,
    variables: &HashMap<String, VarBounds>,
    in_values: &Vec<(String, usize)>,
) -> Result<AssignmentSet, CompileError> {
    let is_input = |name: &str| in_values.iter().any(|(in_name, _)| in_name == name);
    let mut assignments = AssignmentSet::new();

    Ok(compile_conditional_chain_recursive(
        tree,
        node.children.as_ref().unwrap(),
        variables,
        in_values,
    )?)
}

fn compile_block_to_assignments(
    tree: &Tree<ASTNode>,
    node: &crate::tree::Node<ASTNode>,
    variables: &HashMap<String, VarBounds>,
    in_values: &Vec<(String, usize)>,
) -> Result<AssignmentSet, CompileError> {
    let is_input = |name: &str| in_values.iter().any(|(in_name, _)| in_name == name);
    let mut assignments = AssignmentSet::new();
    if let Some(children) = &node.children {
        for c in children {
            let child_node = tree.get_node(*c).unwrap();
            match child_node.data.node_type {
                ASTNodeType::Assign => {
                    let new_assignment =
                        compile_assign_node(tree, child_node, variables, &is_input)?;
                    assignments.merge(new_assignment);
                }
                ASTNodeType::Conditional => {
                    let new_assignment =
                        compile_conditional_node(tree, child_node, variables, in_values)?;
                    assignments.merge(new_assignment);
                }
                ASTNodeType::VariableDeclaration {
                    var_type: _,
                    var_id: _,
                } => {}
                _ => unreachable!(),
            }
        }
    }
    Ok(assignments)
}

///Compiles a single module into Verilog from an AST
pub fn compile_module(tree: &mut Tree<ASTNode>) -> Result<Tree<VNode>, CompileError> {
    //A little bit of a workaround in order to make this work well with the ? operator
    let head = tree.find_head().ok_or(CompileError::CouldNotFindASTHead)?;

    // Type-check to make sure everything'll work
    {
        let (id, in_values, out_values) = match &tree[head].data.node_type {
            ASTNodeType::ModuleDeclaration {
                id,
                in_values,
                out_values,
            } => (id, in_values, out_values),
            _ => {
                panic!("Tried to compile module which wasn't of type Node::ModuleDeclaration");
            }
        };

        //Stores pairs of (variable ID, (highest used t-offset, lowest used t-offset))
        //This is needed to create the registers
        let variables: HashMap<String, VarBounds> = get_var_bounds(tree)?;

        //Before creating the tree, verify it
        verify(tree, &variables)?;
    }

    let module = Module::from_ast(tree, head)?;
    println!("{:#?}", module);
    //unimplemented!();

    //Creating the Verilog AST can now officially begin

    let mut v_tree = Tree::new();
    let v_head = {
        let mut in_values: Vec<_> = module
            .inputs
            .iter()
            .map(|input| (input, module.variables.get(input).unwrap()))
            .map(|(variable, vtype)| (variable.to_string(), vtype.bit_size()))
            .collect();

        let mut out_values: Vec<_> = module
            .outputs
            .iter()
            .map(|input| (input, module.variables.get(input).unwrap()))
            .map(|(variable, vtype)| (variable.to_string(), vtype.bit_size()))
            .collect();

        in_values.push(("rst".to_string(), 1));
        in_values.push(("clk".to_string(), 1));

        //Create the head of the tree, a module declaration
        //rst and clk are always included as inputs in `v_tree`, but not `tree`
        let v_head = {
            v_tree.new_node(VNode::ModuleDeclaration {
                id: module.name.clone(),
                in_values: in_values.clone(),
                out_values: out_values.clone(),
            })
        };
        v_head
    };

    /*let mut ins_and_outs: Vec<(String, usize)> = Vec::new();
    ins_and_outs.append(&mut in_values.clone());
    ins_and_outs.append(&mut out_values.clone());*/

    //let is_input = |name: &str| in_values.iter().any(|(in_name, _)| in_name == name);
    //let is_output = |name: &str| out_values.iter().any(|(out_name, _)| out_name == name);
    let is_input = |name: &str| module.inputs.iter().any(|in_name| in_name == name);
    let is_output = |name: &str| module.outputs.iter().any(|out_name| out_name == name);

    // Contains tuples of (registered name, relative time spec, variable name)
    /*let registers: Vec<(String, i64, String)> = module
    .variables
    .iter()
    .map(|(varname, _)| module.block.assignments.get(varname).unwrap())
    // Map each variable to its name with index (var_0, var_1, etc.), using flat_map to collect all values
    .flat_map(|(varname, _)| {
        module
            .block
            .variable_reference_range(varname)
            //(var.1.lowest_ref..(var.1.highest_ref + 1))
            .map(move |i| (variable_name_relative(varname, i), i, varname.to_owned()))
    })
    .collect();*/

    // Create a VNode to hold things that occur at the positive clock edge (i.e. always @(posedge clk))
    let clock_edge = v_tree.new_node(VNode::AlwaysBegin {
        trigger: AlwaysBeginTriggerType::Posedge,
    });

    // Figure out what each value is set to
    //let mut reset_values = HashMap::new();
    //let mut assignments = HashMap::new();
    //let mut assignments = compile_block_to_assignments(tree, &tree[head], &variables, &in_values)?;

    //println!("Determined module assignments:");
    //println!("Assignment set: {:#?}", assignments);
    //println!("Registers: {:#?}", registers);

    //Register chain creation for each variable
    if true {
        // Add all the registers to the chain. If the register is an array (as of now, bits<n>), create an Index above the VariableReference
        /*for (reg, timespec, variable_name) in registers {
            let var_node = VNode::VariableReference {
                var_id: reg.clone(),
            };
            let reg_head = v_tree.new_node(var_node);

            let is_combinatorial = module
                .block
                .assignments
                .get(&variable_name)
                .unwrap()
                .is_combinatorial();

            /*let is_combinatorial = match assignments.assignments.get(&variable_name) {
                Some((_, x, _)) => *x,
                None => true,
            };*/

            if timespec == 0 && is_combinatorial {
                let declaration = VNode::WireDeclare {
                    bits: module.variables[&variable_name].bit_size(),
                };
                let declaration = v_tree.new_node(declaration);
                v_tree.append_to(v_head, declaration)?;

                v_tree.append_to(declaration, reg_head)?;
            } else {
                let declaration = VNode::RegisterDeclare {
                    bits: module.variables[&variable_name].bit_size(),
                };
                let declaration = v_tree.new_node(declaration);
                v_tree.append_to(v_head, declaration)?;

                v_tree.append_to(declaration, reg_head)?;
            }
        }*/

        for (name, vartype) in module.variables.clone().iter() {
            //Create the possible index for this node for use with `compile_var_ref_from_string`
            let index = match vartype {
                Type::Bits { size } => Some(VNode::Index {
                    high: size - 1,
                    low: 0,
                }),
                _ => None,
            };

            // Assign indexed variables to their input/output counterparts
            if is_input(&name) || is_output(&name) {
                let (mut assign_no_index_tree, mut assign_index_0_tree) = {
                    (
                        compile_var_ref_from_string(&name, None, index.clone())?,
                        compile_var_ref_from_string(&name, Some(0), index.clone())?,
                    )
                };
                /*let assign_no_index = v_tree.new_node(VNode::VariableReference {
                    var_id: name.to_string(),
                });
                let assign_index_0 = v_tree.new_node(VNode::VariableReference {
                    var_id: variable_name_relative(&name, 0),
                });*/
                let assign_node = v_tree.new_node(VNode::AssignKeyword {});

                if is_output(&name) {
                    // Assign index 0 to actual variable (only necessary if variable is an output):
                    // assign out = out_0;
                    v_tree.append_tree(assign_node, &mut assign_no_index_tree)?;

                    // If the rhs has variables referenced at [t], then this is a combinatorial assign
                    // Otherwise, it is registered (and we assign the t-1 register to the output)
                    let is_combinatorial_output = module
                        .block
                        .assignments
                        .get(name)
                        .unwrap()
                        .is_combinatorial(); /* {
                                                 let rhs = assignments.get(&name).unwrap().2;
                                                 tree.recurse_iterative::<_, CompileError, _, _>(
                                                     rhs,
                                                     |tree, node, children, _| {
                                                         // If any children returned true, we're true
                                                         if children.iter().any(|x| **x) {
                                                             return Ok(true);
                                                         }

                                                         match &tree.get_node(node).unwrap().data.node_type {
                                                             ASTNodeType::TimeOffsetRelative { offset } => Ok(*offset == 0),
                                                             _ => Ok(false),
                                                         }
                                                     },
                                                     &(),
                                                 )?
                                             };*/
                    v_tree.append_tree(assign_node, &mut assign_index_0_tree)?;
                    /*if is_combinatorial_output {
                        v_tree.append_tree(assign_node, &mut assign_index_0_tree)?;
                    } else {
                        let mut prev_ref =
                            compile_var_ref_from_string(&name, Some(-1), index.clone())?;
                        v_tree.append_tree(assign_node, &mut prev_ref)?;
                    }*/

                    /*v_tree.append_to(assign_node, assign_no_index)?;
                    v_tree.append_to(assign_node, assign_index_0)?;*/

                    // Go back and declare the variable
                    if is_combinatorial_output {
                        let var_node = VNode::VariableReference {
                            var_id: variable_name_relative(name, 0),
                        };
                        let reg_head = v_tree.new_node(var_node);
                        let declaration = VNode::WireDeclare {
                            bits: module.variables[name].bit_size(),
                        };
                        let declaration = v_tree.new_node(declaration);
                        v_tree.append_to(v_head, declaration)?;

                        v_tree.append_to(declaration, reg_head)?;
                    }
                } else {
                    // The opposite is necessary for inputs:
                    // assign in_0 = in;
                    v_tree.append_tree(assign_node, &mut assign_index_0_tree)?;
                    v_tree.append_tree(assign_node, &mut assign_no_index_tree)?;
                    /*v_tree.append_to(assign_node, assign_index_0)?;
                    v_tree.append_to(assign_node, assign_no_index)?;*/

                    // Go back and declare the variable
                    let var_node = VNode::VariableReference {
                        var_id: variable_name_relative(name, 0),
                    };
                    let reg_head = v_tree.new_node(var_node);
                    let declaration = VNode::WireDeclare {
                        bits: module.variables[name].bit_size(),
                    };
                    let declaration = v_tree.new_node(declaration);
                    v_tree.append_to(v_head, declaration)?;

                    v_tree.append_to(declaration, reg_head)?;
                }

                v_tree.append_to(head, assign_node)?;
            }

            // TODO: Calculate the ref_range based on what variables we actually use
            // (it may vary based on whether they're used in combinatorial or non-combinatorial variables)
            let ref_range = module.block.variable_reference_range(name);
            //let ref_range = offsets.lowest_ref..offsets.highest_ref;

            // Chaining:
            // var_neg1 <= var_0;
            // var_0 <= var_1;
            // var_1 <= var_2;
            // etc.
            for i in ref_range {
                let mut lhs = compile_var_ref_from_string(&name, Some(i), index.clone())?;

                let reset_value = if let Some(reset_value) = module.reset_values.get(name) {
                    Some(*reset_value)
                    /*let reset_value = tree.get_node(*reset_value)?;
                    match &reset_value.data.node_type {
                        ASTNodeType::NumberLiteral(literal) => Some(*literal),
                        _ => {
                            panic!("Reset value RHS must be a number literal");
                        }
                    }*/
                } else {
                    None
                };

                let mut rhs = if let Some(reset_value) = reset_value {
                    let mut conditional_tree = Tree::new();
                    let conditional = conditional_tree.new_node(VNode::Ternary {});
                    let rst_ref = conditional_tree.new_node(VNode::VariableReference {
                        var_id: "rst".to_owned(),
                    });
                    let reset_value = conditional_tree.new_node(VNode::NumberLiteral {
                        literal: reset_value,
                    });
                    conditional_tree.append_to(conditional, rst_ref)?;
                    conditional_tree.append_to(conditional, reset_value)?;
                    let mut rhs = compile_var_ref_from_string(&name, Some(i + 1), index.clone())?;
                    conditional_tree.append_tree(conditional, &mut rhs)?;
                    conditional_tree
                } else {
                    compile_var_ref_from_string(&name, Some(i + 1), index.clone())?
                };

                let reg_assign = v_tree.new_node(VNode::ClockAssign {});

                v_tree.append_tree(reg_assign, &mut lhs)?;
                v_tree.append_tree(reg_assign, &mut rhs)?;
                v_tree.append_to(clock_edge, reg_assign)?;

                // Go back and declare the variable
                let var_node = VNode::VariableReference {
                    var_id: variable_name_relative(name, i),
                };
                let reg_head = v_tree.new_node(var_node);
                let declaration = VNode::RegisterDeclare {
                    bits: module.variables[name].bit_size(),
                };
                let declaration = v_tree.new_node(declaration);
                v_tree.append_to(v_head, declaration)?;

                v_tree.append_to(declaration, reg_head)?;
            }
        }
    }

    // Register all the non-combinatorial variables
    for (var_id, rhs) in module.block.assignments.iter() {
        if rhs.is_combinatorial() {
            continue;
        }

        // Declare it (`wire x_next;`)
        let var_node = VNode::VariableReference {
            var_id: format!("{}_next", (*var_id).to_owned()),
        };
        let reg_head = v_tree.new_node(var_node);

        let declaration = VNode::WireDeclare {
            bits: module.variables[var_id].bit_size(),
        };
        let declaration = v_tree.new_node(declaration);
        v_tree.append_to(v_head, declaration)?;

        v_tree.append_to(declaration, reg_head)?;

        // x_0 <= x_next
        let mut lhs = compile_var_ref_from_string(var_id, Some(0), None)?;

        let reset_value = if let Some(reset_value) = module.reset_values.get(var_id) {
            Some(*reset_value)
            /*let reset_value = tree.get_node(*reset_value)?;
            match &reset_value.data.node_type {
                ASTNodeType::NumberLiteral(literal) => Some(*literal),
                _ => {
                    panic!("Reset value RHS must be a number literal");
                }
            }*/
        } else {
            None
        };

        let mut rhs = if let Some(reset_value) = reset_value {
            let mut conditional_tree = Tree::new();
            let conditional = conditional_tree.new_node(VNode::Ternary {});
            let rst_ref = conditional_tree.new_node(VNode::VariableReference {
                var_id: "rst".to_owned(),
            });
            let reset_value = conditional_tree.new_node(VNode::NumberLiteral {
                literal: reset_value,
            });
            conditional_tree.append_to(conditional, rst_ref)?;
            conditional_tree.append_to(conditional, reset_value)?;
            let mut rhs = compile_var_ref_from_string(var_id, Some(1), None)?;
            conditional_tree.append_tree(conditional, &mut rhs)?;
            conditional_tree
        } else {
            compile_var_ref_from_string(&var_id, Some(1), None)?
        };

        let reg_assign = v_tree.new_node(VNode::ClockAssign {});
        v_tree.append_tree(reg_assign, &mut lhs)?;
        v_tree.append_tree(reg_assign, &mut rhs)?;
        v_tree.append_to(clock_edge, reg_assign)?;

        // Go back and declare the variable
        let var_node = VNode::VariableReference {
            var_id: variable_name_relative(var_id, 0),
        };
        let reg_head = v_tree.new_node(var_node);
        let declaration = VNode::RegisterDeclare {
            bits: module.variables[var_id].bit_size(),
        };
        let declaration = v_tree.new_node(declaration);
        v_tree.append_to(v_head, declaration)?;

        v_tree.append_to(declaration, reg_head)?;
    }

    // Compile the assignments
    for (var_id, rhs) in module.block.assignments.iter() {
        let verilog_name = variable_name_relative(&var_id, 0);

        // Create a variable reference to the lhs
        let lhs_vnode = {
            if rhs.is_combinatorial() {
                v_tree.new_node(VNode::VariableReference {
                    var_id: verilog_name.clone(),
                })
            } else {
                v_tree.new_node(VNode::VariableReference {
                    var_id: format!("{}_next", var_id),
                })
            }
            //If the lhs had an index above it created in v_tree, append lhs_vnode properly
            /*if let Some(idx) = lhs_index {
                v_tree.append_to(idx, lhs_vnode)?;
                lhs_vnode = idx;
            }*/

            //lhs_vnode
        };

        let assign_vnode = v_tree.new_node(VNode::AssignKeyword {});
        v_tree.append_to(head, assign_vnode)?;

        v_tree.append_to(assign_vnode, lhs_vnode)?;

        if !rhs.is_combinatorial() {
            let mut rhs = rhs.clone();
            rhs.shift_time(1);
            let mut rhs = compile_ir_expression(&rhs)?;
            v_tree.append_tree(assign_vnode, &mut rhs)?;
        } else {
            let mut rhs = compile_ir_expression(rhs)?;
            v_tree.append_tree(assign_vnode, &mut rhs)?;
        }

        if rhs.is_combinatorial() && !is_output(var_id) {
            let var_node = VNode::VariableReference {
                var_id: variable_name_relative(var_id, 0),
            };
            let reg_head = v_tree.new_node(var_node);
            let declaration = VNode::WireDeclare {
                bits: module.variables[var_id].bit_size(),
            };
            let declaration = v_tree.new_node(declaration);
            v_tree.append_to(v_head, declaration)?;

            v_tree.append_to(declaration, reg_head)?;
        }
    }

    /*//User-defined logic compilation (uses the compile_expression function when encountering an expression)
    if let Some(children) = &tree[head].children {
        for c in children {
            let child_node = tree.get_node(*c).unwrap();
            match child_node.data.node_type {
                ASTNodeType::Assign => {
                    //`lhs` is the ID of the left-hand-side's `ASTNode::VariableReference`
                    //`lhs_index` is the ID of the left-hand-side's `VNode::Index` which has been added to `v_tree` regardless of its existence in `tree`
                    let (lhs, lhs_index) = {
                        let mut lhs = child_node.children.as_ref().unwrap()[0];

                        let lhs_index = {
                            //If the lhs is an index, set `lhs` to the variable reference and add the index
                            //If the lhs is *not* an index (and is a var reference), then add an index above it using `VarBounds` for default values
                            //BUT, only add an index if the type is indexable
                            let lhs_node_type = &tree[lhs].data.node_type;
                            if let ASTNodeType::Index { high, low } = lhs_node_type {
                                let idx = v_tree.new_node(VNode::Index {
                                    high: *high,
                                    low: *low,
                                });
                                lhs = tree[lhs].children.as_ref().unwrap()[0];
                                Some(idx)
                            } else if let ASTNodeType::VariableReference { var_id } = lhs_node_type
                            {
                                let bounds = &variables[var_id];
                                match bounds.var_type {
                                    Type::Bits { size } => {
                                        let idx = v_tree.new_node(VNode::Index {
                                            high: size - 1,
                                            low: 0,
                                        });
                                        Some(idx)
                                    }
                                    _ => None,
                                }
                            } else {
                                unreachable!()
                            }
                        };

                        (lhs, lhs_index)

                        /*//Note: an explicit index should always exist if it's of type `bits<n>`
                        //That is, it doesn't need to be user supplied but should customarily be added by the compiler in this step

                        let mut lhs = child_node.children.as_ref().unwrap()[0];

                        {
                            let lhs_node = tree.get_node(lhs)?;

                            //If the lhs is an index, set `lhs` to the variable reference instead
                            if let ASTNodeType::Index { high, low } = lhs_node.data.node_type {
                                lhs = lhs_node.children.as_ref().unwrap()[0];
                            }
                        }

                        //Now, create the var ref subtree and append it
                        if let ASTNodeType::VariableReference { var_id } = tree[lhs].data.node_type
                        {
                            let mut lhs_subtree =
                                compile_var_ref(tree, lhs, &variables, true, true)?;

                            //Since there's always an index, `lhs_index` should always be the same as the head of `lhs_subtree`
                            let mut lhs_index = lhs_subtree.find_head();
                            v_tree.append_tree()

                            (lhs, lhs_index)
                        } else {
                            return Err(CompileError::InvalidNodeTypes {
                                nodes: vec![tree[lhs].data.clone()],
                            });
                        }*/
                    };

                    let rhs = child_node.children.as_ref().unwrap()[1];

                    let lhs_name = match &tree.get_node(lhs).unwrap().data.node_type {
                        ASTNodeType::VariableReference { var_id } => {
                            //Can't assign to an input
                            if in_values.contains(var_id) {
                                return Err(CompileError::InputAssignment {
                                    context: tree.get_node(lhs).unwrap().data.context.clone(),
                                });
                            }

                            let t_offset = tree.get_first_child(lhs)?;
                            if let ASTNodeType::TimeOffsetRelative { offset } =
                                t_offset.data.node_type
                            {
                                if offset < 0 {
                                    return Err(CompileError::AssignmentInPast {
                                        context: tree.get_node(lhs).unwrap().data.context.clone(),
                                    });
                                }
                            }
                            variable_name(var_id, &*tree, t_offset.id)
                        }
                        _ => unreachable!(),
                    };

                    //Sometimes, an index will need to be added
                    let lhs_vnode = {
                        let mut lhs_vnode = v_tree.new_node(VNode::VariableReference {
                            var_id: lhs_name.to_string(),
                        });
                        //If the lhs had an index above it created in v_tree, append lhs_vnode properly
                        if let Some(idx) = lhs_index {
                            v_tree.append_to(idx, lhs_vnode)?;
                            lhs_vnode = idx;
                        }

                        lhs_vnode
                    };

                    // For now, only use the assign keyword for assignments.
                    let assign_vnode = v_tree.new_node(VNode::AssignKeyword {});
                    v_tree.append_to(head, assign_vnode)?;

                    v_tree.append_to(assign_vnode, lhs_vnode)?;

                    compile_expression(tree, rhs, &mut v_tree, assign_vnode, &variables)?;
                }
                ASTNodeType::VariableDeclaration {
                    var_type: _,
                    var_id: _,
                } => {}
                _ => unreachable!(),
            }
        }
    } else {
        println!("Module {} has no children", id);
    }*/

    // Add the @(posedge clk) block if it's non-empty
    if v_tree.get_node(clock_edge).unwrap().children.is_some() {
        v_tree.append_to(v_head, clock_edge)?;
    }

    Ok(v_tree)
}

/// Generate a Verilog variable name for the variable `var_id` at the index given by the node at `offset_id` on `tree`.
fn variable_name(
    var_id: &String,
    tree: &Tree<ASTNode>,
    offset_id: NodeId,
    time_shift: i64,
) -> String {
    match tree.get_node(offset_id).unwrap().data.node_type {
        ASTNodeType::TimeOffsetRelative { offset } => {
            variable_name_relative(var_id, offset + time_shift)
        }
        ASTNodeType::TimeOffsetAbsolute { time } => todo!(),
        _ => unreachable!(),
    }
}

fn variable_name_relative(var_id: &str, index: i64) -> String {
    if index == 1 {
        format!("{}_next", var_id)
    } else {
        format!("{}_{}", var_id, index.to_string().replace('-', "neg"))
    }
}
