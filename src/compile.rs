use crate::ast::StringContext;
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

/// Returns a list of all variables referenced in the input AST and the lowest *and* highest t-values referenced for each variable. This is accomplished recursively
///
/// Variables that are inputs or outputs will always contain 0 within the closed range \[lowest, highest\].
///
/// The t-offsets are returned in pairs of `(i64, i64)` corresponding to `(lowest t-value, highest t-value)`
fn get_referenced_variables_with_highest_and_lowest_t_offset(
    tree: &Tree<ASTNode>,
    io_vars: &[String],
) -> Result<HashMap<String, (i64, i64)>, CompileError> {
    //A list of all the variables and their t-offsets. If the variable has only been declared (neither referenced or assigned), then the t-offset will be `None`
    //Reminder: a positive t-offset represents [t-n] and a negative represents [t+n]
    let mut variables: HashMap<String, Option<(i64, i64)>> = HashMap::new();

    for nodeid in tree {
        match &tree[nodeid].data.node_type {
            ASTNodeType::ModuleDeclaration {
                id: _,
                in_values,
                out_values,
            } => {
                for (_t, name) in in_values {
                    variables.insert(name.clone(), None);
                }
                for (_t, name) in out_values {
                    variables.insert(name.clone(), None);
                }
            }
            ASTNodeType::VariableReference {
                var_id,
                t_offset: new_t,
            } => {
                if let Some(current_t) = variables.get_mut(var_id) {
                    //If the variable is declared, check to see if this is the highest referenced t-offset and record
                    if let Some((low, high)) = current_t {
                        *high = (*high).max(*new_t);
                        *low = (*low).min(*new_t);

                        if io_vars.contains(var_id) {
                            *high = (*high).max(0);
                            *low = (*low).min(0);
                        }
                    } else {
                        let _ = current_t.insert((*new_t, *new_t));
                    }
                } else {
                    return Err(CompileError::UndeclaredVariable {
                        context: tree[nodeid].data.context.clone(),
                    });
                }
            }
            ASTNodeType::VariableDeclaration {
                var_type: _,
                var_id,
            } => {
                variables.insert(var_id.clone(), None);
            }
            _ => {}
        }
    }

    //For each variable, set its offset to 0 if never referenced
    let variables = variables
        .drain()
        .map(|(name, t_offset)| (name, t_offset.unwrap_or((0, 0))))
        .collect();

    Ok(variables)
}

fn compile_expression(
    ast: &Tree<ASTNode>,
    node: NodeId,
    vast: &mut Tree<VNode>,
    vnode: NodeId,
) -> Result<(), CompileError> {
    let new_node_data = match &ast.get_node(node).unwrap().data.node_type {
        ASTNodeType::VariableReference { var_id, t_offset } => Some(VNode::VariableReference {
            var_id: variable_name(var_id, *t_offset),
        }),
        ASTNodeType::BitwiseInverse => Some(VNode::BitwiseInverse {}),
        ASTNodeType::Add => Some(VNode::Add {}),
        ASTNodeType::Subtract => Some(VNode::Subtract {}),
        _ => todo!(),
    };

    if let Some(vnode_data) = new_node_data {
        let new_vnode = vast.new_node(vnode_data);
        vast.append_to(vnode, new_vnode)?;
        if let Some(children) = &ast.get_node(node).unwrap().children {
            for child in children {
                compile_expression(ast, *child, vast, new_vnode)?;
            }
        }
    }

    Ok(())
}

#[derive(Clone)]
pub enum CompileError {
    CouldNotFindASTHead,
    TreeError { err: TreeError },
    UndeclaredVariable { context: StringContext },
}

impl From<TreeError> for CompileError {
    fn from(e: TreeError) -> Self {
        CompileError::TreeError { err: e }
    }
}

impl std::fmt::Debug for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut include_position = |ctx: &StringContext, msg: &str| {
            write!(
                f,
                "{} on line {} col{}: {}\n{}{}^",
                msg,
                ctx.line,
                ctx.col,
                ctx.node_str,
                ctx.line_str,
                " ".repeat(ctx.col - 1)
            )
        };
        match self {
            CompileError::CouldNotFindASTHead => write!(f, "Could not find AST head."),
            CompileError::TreeError { err } => write!(f, "Tree error: {:?}", err),
            CompileError::UndeclaredVariable { context } => {
                include_position(context, "Undeclared variable")
            }
        }
    }
}

///Compiles a single module into Verilog from an AST
pub fn compile_module(tree: &mut Tree<ASTNode>) -> Result<Tree<VNode>, CompileError> {
    //A little bit of a workaround in order to make this work well with the ? operator
    let head = tree.find_head().ok_or(CompileError::CouldNotFindASTHead)?;

    if let ASTNodeType::ModuleDeclaration {
        id,
        in_values,
        out_values,
    } = &tree[head].data.node_type
    {
        let mut in_values: Vec<String> = in_values.iter().map(|pair| pair.1.clone()).collect();
        let out_values: Vec<String> = out_values.iter().map(|pair| pair.1.clone()).collect();

        in_values.push("rst".to_string());
        in_values.push("clk".to_string());

        let mut ins_and_outs: Vec<String> = Vec::new();
        ins_and_outs.append(&mut in_values.clone());
        ins_and_outs.append(&mut out_values.clone());

        //Stores pairs of (variable ID, (highest used t-offset, lowest used t-offset))
        //This is needed to create the registers
        let variables: HashMap<String, (i64, i64)> =
            get_referenced_variables_with_highest_and_lowest_t_offset(tree, &ins_and_outs)?;

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

        let registers: Vec<String> = variables
            .clone()
            .into_iter()
            // Map each variable to its name with index (var_0, var_1, etc.), using flat_map to collect all values
            .flat_map(|var| {
                // If a variable is an input or an output, don't include var_0
                (var.1 .0..(var.1 .1 + 1)).map(move |i| variable_name(&var.0, i))
            })
            .collect();

        // Create a VNode to hold things that occur at the positive clock edge (i.e. always @(posedge clk))
        let clock_edge = v_tree.new_node(VNode::AlwaysBegin {
            trigger: AlwaysBeginTriggerType::Posedge,
        });

        //Register chain creation for each variable
        if !registers.is_empty() {
            let reg_chain = VNode::RegisterDeclare { vars: registers };
            let reg_chain = v_tree.new_node(reg_chain);
            v_tree.append_to(v_head, reg_chain)?;

            for (name, offsets) in variables.into_iter() {
                // Assign indexed variables to their input/output counterparts
                if ins_and_outs.contains(&name) {
                    let assign_no_index = v_tree.new_node(VNode::VariableReference {
                        var_id: name.to_string(),
                    });
                    let assign_index_0 = v_tree.new_node(VNode::VariableReference {
                        var_id: variable_name(&name, 0),
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
                for i in offsets.0..offsets.1 {
                    let lhs = v_tree.new_node(VNode::VariableReference {
                        var_id: variable_name(&name, i),
                    });
                    let rhs = v_tree.new_node(VNode::VariableReference {
                        var_id: variable_name(&name, i + 1),
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
                            ASTNodeType::VariableReference { var_id, t_offset } => {
                                variable_name(var_id, *t_offset)
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

                        compile_expression(tree, rhs, &mut v_tree, assign_vnode)?;
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
    } else {
        panic!("Tried to compile module which wasn't of type Node::ModuleDeclaration");
    }
}

/// Generate a Verilog variable name for the variable `var_id` at index `index`.
fn variable_name(var_id: &String, index: i64) -> String {
    format!("{}_{}", var_id, index.to_string().replace('-', "neg"))
}
