use crate::ast::{StringContext, Type};
use std::collections::HashMap;

use crate::{
    ast::{ASTNode, ASTNodeType},
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

/// Currently, this takes the input tree and does the following:
/// * Verifies that types in assignments and expressions match up correctly (i.e. verifies types)
fn verify(
    tree: &Tree<ASTNode>,
    variables: &HashMap<String, VarBounds>,
) -> Result<(), CompileError> {
    //Type verification, done bottom-up, keeping track of each nodes' return type
    //Done "recursively" for each assignment
    for n in tree {
        match &tree[n].data.node_type {
            ASTNodeType::Assign => {
                //The method to be applied recursively. Needs to cover anything that would be below an assignment
                fn f(
                    tree: &Tree<ASTNode>,
                    this_node: NodeId,
                    child_vals: Vec<&Result<Type, CompileError>>,
                    variables: &HashMap<String, VarBounds>,
                ) -> Result<Type, CompileError> {
                    let this_node = &tree[this_node];
                    //Make `child_vals` more useful *and* pass along any errors generated by the children if needed
                    let child_vals = {
                        let mut v = Vec::with_capacity(child_vals.len());
                        for c in child_vals {
                            match c {
                                Ok(t) => v.push(*t),
                                Err(e) => return Err(e.clone()),
                            }
                        }

                        v
                    };

                    match &this_node.data.node_type {
                        //Variable references pass their types up without needing to check their (non-existent) children
                        ASTNodeType::VariableReference { var_id } => {
                            let bounds = &variables[var_id];
                            Ok(bounds.var_type)
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

                let children = tree[n].children.clone().unwrap();
                let lhs_t = tree.recurse_iterative(children[0], f, variables)?;
                let rhs_t = tree.recurse_iterative(children[1], f, variables)?;

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

fn compile_expression(
    ast: &Tree<ASTNode>,
    node: NodeId,
    vast: &mut Tree<VNode>,
    vnode: NodeId,
    variables: &HashMap<String, VarBounds>,
) -> Result<(), CompileError> {
    let new_node_data = match &ast.get_node(node).unwrap().data.node_type {
        ASTNodeType::VariableReference { var_id } => {
            let bounds = variables.get(var_id).unwrap();

            // Get t-offset
            let t_offset_node = ast.get_first_child(node).unwrap();
            let t_offset = &t_offset_node.data.node_type;

            // Ensure t-offset is not greater than the highest t-offset at which this variable is assigned
            if let ASTNodeType::TimeOffsetRelative { offset } = t_offset {
                if offset > &bounds.highest_assignment {
                    return Err(CompileError::ReferenceAfterAssignment {
                        context: ast.get_node(node).unwrap().data.context.clone(),
                    });
                }
            }
            Some(VNode::VariableReference {
                var_id: variable_name(var_id, ast, ast.get_first_child(node).unwrap().id),
            })
        }
        ASTNodeType::BitwiseInverse => Some(VNode::BitwiseInverse {}),
        ASTNodeType::Add => Some(VNode::Add {}),
        ASTNodeType::Subtract => Some(VNode::Subtract {}),
        ASTNodeType::TimeOffsetRelative { offset: _ } => None,
        _ => todo!(),
    };

    if let Some(vnode_data) = new_node_data {
        let new_vnode = vast.new_node(vnode_data);
        vast.append_to(vnode, new_vnode)?;
        if let Some(children) = &ast.get_node(node).unwrap().children {
            for child in children {
                compile_expression(ast, *child, vast, new_vnode, variables)?;
            }
        }
    }

    Ok(())
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
        }
    }
}

///Compiles a single module into Verilog from an AST
pub fn compile_module(tree: &mut Tree<ASTNode>) -> Result<Tree<VNode>, CompileError> {
    //A little bit of a workaround in order to make this work well with the ? operator
    let head = tree.find_head().ok_or(CompileError::CouldNotFindASTHead)?;

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

    //Creating the Verilog AST can now officially begin

    let mut in_values: Vec<String> = in_values.iter().map(|pair| pair.1.clone()).collect();
    let out_values: Vec<String> = out_values.iter().map(|pair| pair.1.clone()).collect();

    in_values.push("rst".to_string());
    in_values.push("clk".to_string());

    let mut ins_and_outs: Vec<String> = Vec::new();
    ins_and_outs.append(&mut in_values.clone());
    ins_and_outs.append(&mut out_values.clone());

    let mut v_tree = Tree::new();

    //Create the head of the tree, a module declaration
    //rst and clk are always included as inputs in `v_tree`, but not `tree`
    let v_head = {
        v_tree.new_node(VNode::ModuleDeclaration {
            id: id.clone(),
            in_values: in_values.clone(),
            out_values: out_values.clone(),
        })
    };

    // Contains tuples of (registered name, variable name)
    let registers: Vec<(String, String)> = variables
        .clone()
        .into_iter()
        // Map each variable to its name with index (var_0, var_1, etc.), using flat_map to collect all values
        .flat_map(|var| {
            (var.1.lowest_ref..(var.1.highest_ref + 1))
                .map(move |i| (variable_name_relative(&var.0, i), var.0.to_owned()))
        })
        .collect();

    // Create a VNode to hold things that occur at the positive clock edge (i.e. always @(posedge clk))
    let clock_edge = v_tree.new_node(VNode::AlwaysBegin {
        trigger: AlwaysBeginTriggerType::Posedge,
    });

    //Register chain creation for each variable
    if !registers.is_empty() {
        let reg_chain = VNode::RegisterDeclare {};
        let reg_chain = v_tree.new_node(reg_chain);
        v_tree.append_to(v_head, reg_chain)?;

        // Add all the registers to the chain. If the register is an array (as of now, bits<n>), create an Index above the VariableReference
        for (reg, variable_name) in registers {
            let var_node = VNode::VariableReference {
                var_id: reg.clone(),
            };
            let reg_head = {
                let var = v_tree.new_node(var_node);
                println!(
                    "Reg name: {} {}\nCurrent `variables`: {:?}",
                    reg, variable_name, variables
                );

                match variables[&variable_name].var_type {
                    Type::Bits { size } => {
                        let index_node = v_tree.new_node(VNode::Index { high: size, low: 0 });
                        v_tree.append_to(index_node, var)?;
                        index_node
                    }
                    _ => var,
                }
            };

            v_tree.append_to(reg_chain, reg_head)?;
        }

        for (name, offsets) in variables.clone().into_iter() {
            // Assign indexed variables to their input/output counterparts
            if ins_and_outs.contains(&name) {
                let assign_no_index = v_tree.new_node(VNode::VariableReference {
                    var_id: name.to_string(),
                });
                let assign_index_0 = v_tree.new_node(VNode::VariableReference {
                    var_id: variable_name_relative(&name, 0),
                });
                let assign_node = v_tree.new_node(VNode::AssignKeyword {});

                if out_values.contains(&name) {
                    // Assign index 0 to actual variable (only necessary if variable is an output):
                    // assign out = out_0;
                    v_tree.append_to(assign_node, assign_no_index)?;
                    v_tree.append_to(assign_node, assign_index_0)?;
                } else {
                    // The opposite is necessary for inputs:
                    // assign in_0 = in;
                    v_tree.append_to(assign_node, assign_index_0)?;
                    v_tree.append_to(assign_node, assign_no_index)?;
                }

                v_tree.append_to(head, assign_node)?;
            }

            // Chaining:
            // var_neg1 <= var_0;
            // var_0 <= var_1;
            // var_1 <= var_2;
            // etc.
            for i in offsets.lowest_ref..offsets.highest_ref {
                let lhs = v_tree.new_node(VNode::VariableReference {
                    var_id: variable_name_relative(&name, i),
                });
                let rhs = v_tree.new_node(VNode::VariableReference {
                    var_id: variable_name_relative(&name, i + 1),
                });
                let reg_assign = v_tree.new_node(VNode::ClockAssign {});

                v_tree.append_to(reg_assign, lhs)?;
                v_tree.append_to(reg_assign, rhs)?;
                v_tree.append_to(clock_edge, reg_assign)?;
            }
        }
    }

    //User-defined logic compilation (uses the compile_expression function when encountering an expression)
    if let Some(children) = &tree[head].children {
        for c in children {
            let child_node = tree.get_node(*c).unwrap();
            match child_node.data.node_type {
                ASTNodeType::Assign => {
                    let lhs = child_node.children.as_ref().unwrap()[0];
                    let rhs = child_node.children.as_ref().unwrap()[1];

                    let lhs_name = match &tree.get_node(lhs).unwrap().data.node_type {
                        ASTNodeType::VariableReference { var_id } => {
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
                    let lhs_vnode = v_tree.new_node(VNode::VariableReference {
                        var_id: lhs_name.to_string(),
                    });

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
    }

    // Add the @(posedge clk) block if it's non-empty
    if v_tree.get_node(clock_edge).unwrap().children.is_some() {
        v_tree.append_to(v_head, clock_edge)?;
    }

    Ok(v_tree)
}

/// Generate a Verilog variable name for the variable `var_id` at the index given by the node at `offset_id` on `tree`.
fn variable_name(var_id: &String, tree: &Tree<ASTNode>, offset_id: NodeId) -> String {
    match tree.get_node(offset_id).unwrap().data.node_type {
        ASTNodeType::TimeOffsetRelative { offset } => variable_name_relative(var_id, offset),
        ASTNodeType::TimeOffsetAbsolute { time } => todo!(),
        _ => unreachable!(),
    }
}

fn variable_name_relative(var_id: &String, index: i64) -> String {
    format!("{}_{}", var_id, index.to_string().replace('-', "neg"))
}
