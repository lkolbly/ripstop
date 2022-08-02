use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::{
    ast::{ASTNode, ASTNodeType, Type},
    tree::{self, NodeId, Tree},
    verilog_ast::{AlwaysBeginTriggerType, VNode},
};

///Returns a list of all variables referenced in the input AST and the highest t-value referenced for each variable. This is accomplished recursively
fn get_referenced_variables_and_highest_t_offset(
    tree: &Tree<ASTNode>,
    input: &tree::Node<ASTNode>,
) -> HashMap<String, i64> {
    //Takes a found variable reference and registers it appropriately, either adding it to the hashmap, incrementing the hashmap value, or leaving it alone
    fn register_reference(var_id: String, t_offset: i64, hm: &mut HashMap<String, i64>) {
        if let Some(current_t) = hm.get_mut(&var_id) {
            *current_t = (*current_t).max(t_offset);
        } else {
            hm.insert(var_id, t_offset);
        }
    }
    //Takes a hashmap of found variables and registers each one in the hashmap
    fn register_references(refs: &mut HashMap<String, i64>, hm: &mut HashMap<String, i64>) {
        for r in refs.drain() {
            register_reference(r.0, r.1, hm);
        }
    }
    let mut variables: HashMap<String, i64> = HashMap::new();

    //For each of these branches, register any variable references
    //Searching children recursively will occur *after* this match statement
    match &input.data.node_type {
        ASTNodeType::ModuleDeclaration {
            id,
            in_values,
            out_values,
        } => {
            //Register all the variables, both the inputs and outputs
            for v in in_values {
                variables.insert(v.1.clone(), 0);
            }
            for v in out_values {
                variables.insert(v.1.clone(), 0);
            }
        }
        ASTNodeType::VariableReference { var_id, t_offset } => {
            register_reference(var_id.clone(), *t_offset, &mut variables);
        }
        _ => (),
    }

    //Get the hashmap for each child of `input` and register the variable references
    if let Some(children) = &input.children {
        for c in children {
            //This is where the recursion comes in
            let mut hm = get_referenced_variables_and_highest_t_offset(tree, &tree[*c]);
            register_references(&mut hm, &mut variables);
        }
    }

    variables
}

fn compile_expression(
    ast: &Tree<ASTNode>,
    node: NodeId,
    vast: &mut Tree<VNode>,
    vnode: NodeId,
) -> Result<(), CompileError> {
    let to_recurse = match &ast.get_node(node).unwrap().data.node_type {
        ASTNodeType::VariableReference { var_id, t_offset } => {
            Some(vast.new_node(VNode::VariableReference {
                var_id: variable_name(var_id, *t_offset),
            }))
        }
        ASTNodeType::BitwiseInverse => Some(vast.new_node(VNode::BitwiseInverse {})),
        ASTNodeType::Add => Some(vast.new_node(VNode::Add {})),
        _ => todo!(),
    };

    if let Some(new_vnode) = to_recurse {
        vast.append_to(vnode, new_vnode);
        if let Some(children) = &ast.get_node(node).unwrap().children {
            for child in children {
                compile_expression(ast, *child, vast, new_vnode)?;
            }
        }
    }

    Ok(())
}

#[derive(Debug, Clone)]
pub enum CompileError {
    CouldNotFindASTHead,
}

///Compiles a single module into Verilog from an AST
pub fn compile_module(tree: &Tree<ASTNode>) -> Result<Tree<VNode>, CompileError> {
    //A little bit of a workaround in order to make this work well with the ? operator
    let head = tree.find_head().ok_or(CompileError::CouldNotFindASTHead)?;

    if let ASTNodeType::ModuleDeclaration {
        id,
        in_values,
        out_values,
    } = &tree[head].data.node_type
    {
        //Stores pairs of (variable ID, highest used t-offset)
        //This is needed to create the registers
        let variables: HashMap<String, i64> =
            get_referenced_variables_and_highest_t_offset(&tree, &tree[head]);

        let mut v_tree = Tree::new();

        //Create the head of the tree, a module declaration
        //rst and clk are always included as inputs in `v_tree`, but not `tree`
        let v_head = {
            let mut in_values: Vec<String> =
                in_values.into_iter().map(|pair| pair.1.clone()).collect();
            let out_values: Vec<String> =
                out_values.into_iter().map(|pair| pair.1.clone()).collect();

            in_values.push("rst".to_string());
            in_values.push("clk".to_string());

            v_tree.new_node(VNode::ModuleDeclaration {
                id: id.clone(),
                in_values,
                out_values,
            })
        };

        let registers: Vec<String> = variables
            .clone()
            .into_iter()
            // Map each variable to its name with index (var_0, var_1, etc.), using flat_map to collect all values
            .flat_map(|var| (0..(var.1 + 1)).map(move |i| variable_name(&var.0, i)))
            .collect();

        //Register chain creation for each variable
        {
            let reg_chain = VNode::RegisterDeclare {
                vars: registers.clone(),
            };
            let reg_chain = v_tree.new_node(reg_chain);
            v_tree.append_to(v_head, reg_chain);
        }

        // Create a VNode to hold things that occur at the positive clock edge (i.e. always @(posedge clk))
        let clock_edge = v_tree.new_node(VNode::AlwaysBegin {
            trigger: AlwaysBeginTriggerType::Posedge,
        });
        v_tree.append_to(v_head, clock_edge);

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

                        let assign_vnode = if registers.contains(&lhs_name) {
                            let n = v_tree.new_node(VNode::ClockAssign {});
                            v_tree.append_to(clock_edge, n);
                            n
                        } else {
                            let n = v_tree.new_node(VNode::AssignKeyword {});
                            v_tree.append_to(head, n);
                            n
                        };

                        v_tree.append_to(assign_vnode, lhs_vnode);

                        compile_expression(tree, rhs, &mut v_tree, assign_vnode)?;
                    }
                    _ => unreachable!(),
                }
            }
        } else {
            println!("Module {} has no children", id);
        }

        Ok(v_tree)
    } else {
        panic!("Tried to compile module which wasn't of type Node::ModuleDeclaration");
    }
}

/// Generate a Verilog variable name for the variable `var_id` at index `index`.
fn variable_name(var_id: &String, index: i64) -> String {
    format!("{}_{}", var_id, index)
}
