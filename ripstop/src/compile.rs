use crate::{
    ast::{ASTNode, ASTNodeType},
    error::{CompileError, CompileResult},
    ir::{BinaryOperator, Expression, LogicalExpression, Module, UnaryOperator},
    ir::{ModuleDeclaration, VariablePath},
    logerror, noncriterr, singleerror,
    tree::{NodeId, Tree, TreeError},
    types::{PrimordialStructDefinition, Type, TypeDatabase},
    verilog_ast::{AlwaysBeginTriggerType, VNode},
};
use serde::{Deserialize, Serialize};

struct IrCompilationState {
    next_internal_var_id: usize,

    /// (variable name, range high, range low, expression tree)
    vars_to_declare: Vec<(String, u32, u32, Tree<VNode>)>,
}

fn compile_ir_expression(
    state: &mut IrCompilationState,
    expr: &Expression,
) -> Result<Tree<VNode>, CompileError> {
    let mut vast = Tree::new();

    match &expr.expr {
        LogicalExpression::BinaryOperation {
            operation,
            lhs,
            rhs,
        } => {
            let node = vast.new_node(match operation {
                BinaryOperator::Addition => VNode::Add {},
                BinaryOperator::Subtraction => VNode::Subtract {},
                BinaryOperator::BitwiseAnd => VNode::BitwiseAnd {},
                BinaryOperator::BitwiseOr => VNode::BitwiseOr {},
                BinaryOperator::BitwiseXor => VNode::BitwiseXor {},
                BinaryOperator::Equal => VNode::Equal {},
                BinaryOperator::NotEqual => VNode::NotEqual {},
                BinaryOperator::Greater => VNode::Greater {},
                BinaryOperator::GreaterEq => VNode::GreaterEq {},
                BinaryOperator::Less => VNode::Less {},
                BinaryOperator::LessEq => VNode::LessEq {},
                BinaryOperator::Concatenate => VNode::Concatenate {},
            });
            let mut lhs = compile_ir_expression(state, &lhs)?;
            let mut rhs = compile_ir_expression(state, &rhs)?;
            vast.append_tree(node, &mut lhs)
                .expect("Couldn't append to tree");
            vast.append_tree(node, &mut rhs)
                .expect("Couldn't append to tree");
        }
        LogicalExpression::UnaryOperation {
            operation,
            operatee,
        } => {
            if let UnaryOperator::Index(range) = operation {
                // This is an index! We have to put this in its own variable
                // Basically, we're converting this:
                // assign expr_were_evaluating = (a + b)[6:4];
                // into this:
                // wire[2:0] __ripstop_index_deferral_N;
                // assign __ripstop_index_deferral_N = (a + b) >> 4;
                // assign expr_were_evaluating = __ripstop_index_deferral_N;
                let lhs = compile_ir_expression(state, &operatee)?;
                let varname = format!("__ripstop_index_deferral_{}", state.next_internal_var_id);
                state.next_internal_var_id += 1;
                state
                    .vars_to_declare
                    .push((varname.clone(), range.high, range.low, lhs));

                vast.new_node(VNode::VariableReference { var_id: varname });

                /*let node = VNode::Index {
                    high: range.high as usize,
                    low: range.low as usize,
                };*/
            } else {
                let node = vast.new_node(match operation {
                    UnaryOperator::Negation => VNode::BitwiseInverse {},
                    UnaryOperator::Index(_range) => {
                        panic!("Should have been handled in the above case");
                    }
                });
                let mut lhs = compile_ir_expression(state, &operatee)?;
                vast.append_tree(node, &mut lhs)?;
            }
        }
        LogicalExpression::Ternary {
            condition,
            lhs,
            rhs,
        } => {
            let mut condition = compile_ir_expression(state, &condition)?;
            let node = vast.new_node(VNode::Ternary {});
            let mut lhs = compile_ir_expression(state, &lhs)?;
            let mut rhs = compile_ir_expression(state, &rhs)?;
            vast.append_tree(node, &mut condition)
                .expect("Couldn't append to tree");
            vast.append_tree(node, &mut lhs)
                .expect("Couldn't append to tree");
            vast.append_tree(node, &mut rhs)
                .expect("Couldn't append to tree");
        }
        LogicalExpression::VariableReference(reference) => {
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
            vast.new_node(VNode::VariableReference { var_id: vref });
        }
        LogicalExpression::NumberLiteral(literal) => {
            vast.new_node(VNode::NumberLiteral {
                literal: literal.clone(),
            });
        }
    }

    Ok(vast)
}

/// Compiles the supplied VariableReference into a suitable Verilog subtree without information from the ast itself other than the variable name .
/// This makes it significantly more primative while still being suitable for some applications
///
/// Some notes:
/// * if `index` is a variant of `VNode` other than `VNode::Index {..}`, this method has undefined behavior
fn compile_var_ref_from_string(
    var_id: &VariablePath,
    t_offset: Option<i64>,
    index: Option<VNode>,
) -> Result<Tree<VNode>, TreeError> {
    let mut tree = Tree::new();

    //Creating the `VariableReference`
    let var_id = match t_offset {
        Some(offset) => variable_name_relative(var_id, offset),
        None => var_id.to_verilog_name(),
    };
    let var_ref = tree.new_node(VNode::VariableReference { var_id });

    //Creating the index if needed
    if let Some(idx) = index {
        let idx = tree.new_node(idx);
        tree.append_to(idx, var_ref)?;
    }

    Ok(tree)
}

pub fn compile_document(
    tree: &mut Tree<ASTNode>,
) -> CompileResult<(Vec<ModuleDeclaration>, Vec<Module>, Tree<VNode>)> {
    let mut result = CompileResult::new();

    let types = logerror!(result, get_type_declarations(tree));

    let declarations = logerror!(result, get_module_declarations(tree, &types));

    let mut vast = Tree::new();
    let v_head = vast.new_node(VNode::Document);

    let head = singleerror!(
        result,
        tree.find_head().ok_or(CompileError::CouldNotFindASTHead)
    );
    let children: Vec<_> = tree.get_node(head).unwrap().children.to_owned();
    let mut modules = vec![];
    for &child in children.iter() {
        match &tree.get_node(child).unwrap().data.node_type {
            ASTNodeType::ModuleDeclaration { .. } => {
                if let Some((module, mut module_vast)) =
                    noncriterr!(result, compile_module(tree, child, &declarations, &types))
                {
                    modules.push(module);
                    singleerror!(result, vast.append_tree(v_head, &mut module_vast));
                }
            }
            _ => {
                // Ignore extern modules
            }
        }
    }

    result.ok((declarations, modules, vast));
    result
}

fn get_type_declarations(tree: &mut Tree<ASTNode>) -> CompileResult<TypeDatabase> {
    let mut result = CompileResult::new();

    let head = singleerror!(
        result,
        tree.find_head().ok_or(CompileError::CouldNotFindASTHead)
    );

    let structs: Vec<_> = tree
        .get_node(head)
        .unwrap()
        .children
        .iter()
        .filter_map(
            |child| match &tree.get_node(*child).unwrap().data.node_type {
                ASTNodeType::StructDefinition { .. } => {
                    noncriterr!(result, PrimordialStructDefinition::from_ast(tree, *child))
                }
                _ => None,
            },
        )
        .collect();

    result.ok(TypeDatabase::new(structs));
    result
}

fn get_module_declarations(
    tree: &mut Tree<ASTNode>,
    types: &TypeDatabase,
) -> CompileResult<Vec<ModuleDeclaration>> {
    let mut result = CompileResult::new();

    // First, parse all of the declarations
    let head = singleerror!(
        result,
        tree.find_head().ok_or(CompileError::CouldNotFindASTHead)
    );

    let mut modules = vec![];
    for child in tree.get_node(head).unwrap().children.iter() {
        match &tree.get_node(*child).unwrap().data.node_type {
            ASTNodeType::ModuleDeclaration {
                id: _id,
                doc_comment: _doc_comment,
                in_values: _in_values,
                out_values: _out_values,
            } => {
                if let Some(module) =
                    noncriterr!(result, ModuleDeclaration::from_ast(tree, *child, types))
                {
                    modules.push(module);
                }
            }
            ASTNodeType::ExternModuleDeclaration { .. } => {
                if let Some(module) =
                    noncriterr!(result, ModuleDeclaration::from_ast(tree, *child, types))
                {
                    modules.push(module);
                }
            }
            ASTNodeType::StructDefinition { .. } => {
                // No-op, these are handled elsewhere
            }
            _ => {
                panic!("Found top-level object which wasn't a module");
            }
        }
    }

    result.ok(modules);
    result
}

///Compiles a single module into Verilog from an AST
pub fn compile_module(
    tree: &mut Tree<ASTNode>,
    head: NodeId,
    modules: &Vec<ModuleDeclaration>,
    types: &TypeDatabase,
) -> CompileResult<(Module, Tree<VNode>)> {
    let mut result = CompileResult::new();

    let module = logerror!(result, Module::from_ast(tree, head, modules, types));

    //Creating the Verilog AST can now officially begin

    let mut v_tree = Tree::new();
    let v_head = {
        let mut in_values: Vec<_> = module
            .inputs
            .iter()
            .flat_map(|input| {
                let ty = module
                    .variables
                    .get(&VariablePath::from_root(input))
                    .unwrap();
                let leafs = VariablePath::from_root(input).leaf_paths(ty);
                let leafs: Vec<_> = leafs
                    .iter()
                    .map(|(name, ty)| (name.to_verilog_name(), ty.clone()))
                    .collect();
                leafs
            })
            .map(|(variable, vtype)| (variable.to_string(), vtype.bit_size()))
            .collect();

        let out_values: Vec<_> = module
            .outputs
            .iter()
            .flat_map(|output| {
                let ty = module
                    .variables
                    .get(&VariablePath::from_root(output))
                    .unwrap();
                let leafs = VariablePath::from_root(output).leaf_paths(ty);
                let leafs: Vec<_> = leafs
                    .iter()
                    .map(|(name, ty)| (name.to_verilog_name(), ty.clone()))
                    .collect();
                leafs
            })
            .map(|(variable, vtype)| (variable.to_string(), vtype.bit_size()))
            .collect();

        in_values.push(("rst".to_string(), 1));
        in_values.push(("clk".to_string(), 1));

        //Create the head of the tree, a module declaration
        //rst and clk are always included as inputs in `v_tree`, but not `tree`
        v_tree.new_node(VNode::ModuleDeclaration {
            id: module.name.clone(),
            in_values: in_values.clone(),
            out_values,
        })
    };

    let is_input = |name: &VariablePath| {
        module
            .inputs
            .iter()
            .any(|in_name| name.subset_of(&VariablePath::from_root(in_name)))
    };
    let is_output = |name: &VariablePath| {
        module
            .outputs
            .iter()
            .any(|out_name| name.subset_of(&VariablePath::from_root(out_name)))
    };

    // Create a VNode to hold things that occur at the positive clock edge (i.e. always @(posedge clk))
    let clock_edge = v_tree.new_node(VNode::AlwaysBegin {
        trigger: AlwaysBeginTriggerType::Posedge,
    });

    // Instantiate each module instantiation
    for (instance, module) in module.instantiations.iter() {
        let module = match modules.iter().filter(|m| &m.name == module).next() {
            Some(x) => x,
            None => {
                result.error(CompileError::UndeclaredModule {
                    module_name: module.clone(),
                    instance_name: instance.clone(),
                });
                continue;
            }
        };

        let v_instance = v_tree.new_node(VNode::ModuleInstantiation {
            module: module.name.clone(),
            instance: instance.to_owned(),
        });

        for (vartype, portname, varname, declare, is_input) in module
            .inputs
            .iter()
            .map(|(ty, name)| (ty, name, true))
            .chain(module.outputs.iter().map(|(ty, name)| (ty, name, false)))
            .flat_map(|(ty, var_name, is_input)| {
                let path = VariablePath::from_root(var_name);
                let paths: Vec<_> = path
                    .leaf_paths(ty)
                    .iter()
                    .map(|(path, ty)| (path.clone(), ty.clone(), is_input))
                    .collect();
                paths
            })
            .map(|(path, ty, is_input)| {
                let var_name = path.put_under(VariablePath::from_root(instance));
                (path, var_name, ty, is_input)
            })
            .map(|(port_name, var_name, ty, is_input)| (ty, port_name, var_name, true, is_input))
            .chain(std::iter::once((
                Type::Bit,
                VariablePath::from_root("clk"),
                VariablePath::from_root("clk"),
                false,
                true,
            )))
            .chain(std::iter::once((
                Type::Bit,
                VariablePath::from_root("rst"),
                VariablePath::from_root("rst"),
                false,
                true,
            )))
        {
            let v_port = v_tree.new_node(VNode::ModuleInstantiationVariable {
                portname: portname.to_verilog_name(),
                variable: varname.to_verilog_name(),
            });
            singleerror!(result, v_tree.append_to(v_instance, v_port));

            if declare {
                // Also declare that variable as a wire
                let v_decl = v_tree.new_node(VNode::WireDeclare {
                    bits: vartype.bit_size(),
                });
                let v_varref = v_tree.new_node(VNode::VariableReference {
                    var_id: varname.to_verilog_name(),
                });
                singleerror!(result, v_tree.append_to(v_decl, v_varref));
                singleerror!(result, v_tree.append_to(v_head, v_decl));

                // And assign it appropriately
                if is_input {
                    let v_assign = v_tree.new_node(VNode::AssignKeyword {});
                    let lhs = v_tree.new_node(VNode::VariableReference {
                        var_id: varname.to_verilog_name(),
                    });
                    let rhs = v_tree.new_node(VNode::VariableReference {
                        var_id: variable_name_relative(&varname, 0),
                    });
                    singleerror!(result, v_tree.append_to(v_assign, lhs));
                    singleerror!(result, v_tree.append_to(v_assign, rhs));
                    singleerror!(result, v_tree.append_to(v_head, v_assign));
                } else {
                    let v_decl = v_tree.new_node(VNode::WireDeclare {
                        bits: vartype.bit_size(),
                    });
                    let v_varref = v_tree.new_node(VNode::VariableReference {
                        var_id: variable_name_relative(&varname, 0),
                    });
                    singleerror!(result, v_tree.append_to(v_decl, v_varref));
                    singleerror!(result, v_tree.append_to(v_head, v_decl));

                    let v_assign = v_tree.new_node(VNode::AssignKeyword {});
                    let lhs = v_tree.new_node(VNode::VariableReference {
                        var_id: variable_name_relative(&varname, 0),
                    });
                    let rhs = v_tree.new_node(VNode::VariableReference {
                        var_id: varname.to_verilog_name(),
                    });
                    singleerror!(result, v_tree.append_to(v_assign, lhs));
                    singleerror!(result, v_tree.append_to(v_assign, rhs));
                    singleerror!(result, v_tree.append_to(v_head, v_assign));
                }
            }
        }

        singleerror!(result, v_tree.append_to(v_head, v_instance));
    }

    //Register chain creation for each variable
    for (name, vartype) in module.variables.clone().iter() {
        // Only create register chains for leaf variables
        match vartype {
            Type::Bit | Type::Bits { .. } => {}
            _ => {
                continue;
            }
        }

        //Create the possible index for this node for use with `compile_var_ref_from_string`
        let index = None;
        /*let index = match vartype {
            Type::Bits { size } => Some(VNode::Index {
                high: size - 1,
                low: 0,
            }),
            _ => None,
        };*/

        // Assign indexed variables to their input/output counterparts
        if is_input(name) || is_output(name) {
            let (mut assign_no_index_tree, mut assign_index_0_tree) = {
                (
                    singleerror!(
                        result,
                        compile_var_ref_from_string(name, None, index.clone())
                    ),
                    singleerror!(
                        result,
                        compile_var_ref_from_string(name, Some(0), index.clone())
                    ),
                )
            };
            let assign_node = v_tree.new_node(VNode::AssignKeyword {});

            if is_output(name) {
                // Assign index 0 to actual variable (only necessary if variable is an output):
                // assign out = out_0;
                singleerror!(
                    result,
                    v_tree.append_tree(assign_node, &mut assign_no_index_tree)
                );

                // If the rhs has variables referenced at [t], then this is a combinatorial assign
                // Otherwise, it is registered (and we assign the t-1 register to the output)
                let expr = match module.block.assignments.get(name) {
                    Some(e) => e,
                    None => {
                        result.error(CompileError::VariableNotAssigned {
                            var_name: name.display(),
                        });
                        continue;
                    }
                };
                let is_combinatorial_output = expr.is_combinatorial();

                singleerror!(
                    result,
                    v_tree.append_tree(assign_node, &mut assign_index_0_tree)
                );

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
                    singleerror!(result, v_tree.append_to(v_head, declaration));

                    singleerror!(result, v_tree.append_to(declaration, reg_head));
                }
            } else {
                // The opposite is necessary for inputs:
                // assign in_0 = in;
                singleerror!(
                    result,
                    v_tree.append_tree(assign_node, &mut assign_index_0_tree)
                );
                singleerror!(
                    result,
                    v_tree.append_tree(assign_node, &mut assign_no_index_tree)
                );
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
                singleerror!(result, v_tree.append_to(v_head, declaration));

                singleerror!(result, v_tree.append_to(declaration, reg_head));
            }

            singleerror!(result, v_tree.append_to(v_head, assign_node));
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
            let mut lhs = singleerror!(
                result,
                compile_var_ref_from_string(name, Some(i), index.clone())
            );

            let reset_value = module.reset_values.get(name);

            let mut rhs = if let Some((reset_value, _)) = reset_value {
                let mut conditional_tree = Tree::new();
                let conditional = conditional_tree.new_node(VNode::Ternary {});
                let rst_ref = conditional_tree.new_node(VNode::VariableReference {
                    var_id: "rst".to_owned(),
                });
                let reset_value = conditional_tree.new_node(VNode::NumberLiteral {
                    literal: reset_value.clone(),
                });
                singleerror!(result, conditional_tree.append_to(conditional, rst_ref));
                singleerror!(result, conditional_tree.append_to(conditional, reset_value));
                let mut rhs = singleerror!(
                    result,
                    compile_var_ref_from_string(name, Some(i + 1), index.clone())
                );
                singleerror!(result, conditional_tree.append_tree(conditional, &mut rhs));
                conditional_tree
            } else {
                singleerror!(
                    result,
                    compile_var_ref_from_string(name, Some(i + 1), index.clone())
                )
            };

            let reg_assign = v_tree.new_node(VNode::ClockAssign {});

            singleerror!(result, v_tree.append_tree(reg_assign, &mut lhs));
            singleerror!(result, v_tree.append_tree(reg_assign, &mut rhs));
            singleerror!(result, v_tree.append_to(clock_edge, reg_assign));

            // Go back and declare the variable
            let var_node = VNode::VariableReference {
                var_id: variable_name_relative(name, i),
            };
            let reg_head = v_tree.new_node(var_node);
            let declaration = VNode::RegisterDeclare {
                bits: module.variables[name].bit_size(),
            };
            let declaration = v_tree.new_node(declaration);
            singleerror!(result, v_tree.append_to(v_head, declaration));

            singleerror!(result, v_tree.append_to(declaration, reg_head));
        }
    }

    // Check that all reset values are correct w.r.t. combinatorialness
    for (var_id, rhs) in module.block.assignments.iter() {
        if let Some((_, timeval)) = module.reset_values.get(var_id) {
            if rhs.is_combinatorial() && *timeval != -1 {
                panic!(
                    "Reset value at timestamp 0 is not permitted for combinatorial variable {}",
                    var_id
                );
            }
            if !rhs.is_combinatorial() && *timeval != 0 {
                panic!(
                    "Reset value at timestamp -1 is not permitted for combinatorial variable {}",
                    var_id
                );
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
            var_id: variable_name_relative(var_id, 1),
        };
        let reg_head = v_tree.new_node(var_node);

        let declaration = VNode::WireDeclare {
            bits: module.variables[var_id].bit_size(),
        };
        let declaration = v_tree.new_node(declaration);
        singleerror!(result, v_tree.append_to(v_head, declaration));

        singleerror!(result, v_tree.append_to(declaration, reg_head));

        // x_0 <= x_next
        let mut lhs = singleerror!(result, compile_var_ref_from_string(var_id, Some(0), None));

        let reset_value = module.reset_values.get(var_id);

        let mut rhs = if let Some((reset_value, _)) = reset_value {
            let mut conditional_tree = Tree::new();
            let conditional = conditional_tree.new_node(VNode::Ternary {});
            let rst_ref = conditional_tree.new_node(VNode::VariableReference {
                var_id: "rst".to_owned(),
            });
            let reset_value = conditional_tree.new_node(VNode::NumberLiteral {
                literal: reset_value.clone(),
            });
            singleerror!(result, conditional_tree.append_to(conditional, rst_ref));
            singleerror!(result, conditional_tree.append_to(conditional, reset_value));
            let mut rhs = singleerror!(result, compile_var_ref_from_string(var_id, Some(1), None));
            singleerror!(result, conditional_tree.append_tree(conditional, &mut rhs));
            conditional_tree
        } else {
            singleerror!(result, compile_var_ref_from_string(var_id, Some(1), None))
        };

        let reg_assign = v_tree.new_node(VNode::ClockAssign {});
        singleerror!(result, v_tree.append_tree(reg_assign, &mut lhs));
        singleerror!(result, v_tree.append_tree(reg_assign, &mut rhs));
        singleerror!(result, v_tree.append_to(clock_edge, reg_assign));

        // Go back and declare the variable
        let var_node = VNode::VariableReference {
            var_id: variable_name_relative(var_id, 0),
        };
        let reg_head = v_tree.new_node(var_node);
        let declaration = VNode::RegisterDeclare {
            bits: module.variables[var_id].bit_size(),
        };
        let declaration = v_tree.new_node(declaration);
        singleerror!(result, v_tree.append_to(v_head, declaration));

        singleerror!(result, v_tree.append_to(declaration, reg_head));
    }

    // Compile the assignments
    let mut ir_compile_state = IrCompilationState {
        next_internal_var_id: 0,
        vars_to_declare: vec![],
    };
    for (var_id, rhs) in module.block.assignments.iter() {
        let verilog_name = variable_name_relative(var_id, 0);

        // Create a variable reference to the lhs
        let lhs_vnode = {
            if rhs.is_combinatorial() {
                v_tree.new_node(VNode::VariableReference {
                    var_id: verilog_name.clone(),
                })
            } else {
                v_tree.new_node(VNode::VariableReference {
                    var_id: variable_name_relative(var_id, 1),
                })
            }
        };

        let assign_vnode = v_tree.new_node(VNode::AssignKeyword {});
        singleerror!(result, v_tree.append_to(v_head, assign_vnode));

        singleerror!(result, v_tree.append_to(assign_vnode, lhs_vnode));

        if !rhs.is_combinatorial() {
            let mut rhs = rhs.clone();
            rhs.shift_time(1);
            let mut rhs = singleerror!(result, compile_ir_expression(&mut ir_compile_state, &rhs));
            singleerror!(result, v_tree.append_tree(assign_vnode, &mut rhs));
        } else {
            let mut rhs = singleerror!(result, compile_ir_expression(&mut ir_compile_state, rhs));
            singleerror!(result, v_tree.append_tree(assign_vnode, &mut rhs));
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
            singleerror!(result, v_tree.append_to(v_head, declaration));

            singleerror!(result, v_tree.append_to(declaration, reg_head));
        }
    }

    // Declare & assign all the internal variables we need
    for (name, high, low, mut value) in ir_compile_state.vars_to_declare.drain(..) {
        // Declare the variable
        let varref = VNode::VariableReference {
            var_id: name.clone(),
        };
        let varref = v_tree.new_node(varref);
        let declaration = VNode::WireDeclare {
            bits: (high - low + 1) as usize,
        };
        let declaration = v_tree.new_node(declaration);
        singleerror!(result, v_tree.append_to(v_head, declaration));
        singleerror!(result, v_tree.append_to(declaration, varref));

        // Construct the shifted value
        let shift_node = v_tree.new_node(VNode::BitwiseRightShift { amount: low });
        singleerror!(result, v_tree.append_tree(shift_node, &mut value));

        let assign_node = v_tree.new_node(VNode::AssignKeyword {});
        let varref = v_tree.new_node(VNode::VariableReference {
            var_id: name.clone(),
        });
        singleerror!(result, v_tree.append_to(assign_node, varref));
        singleerror!(result, v_tree.append_to(assign_node, shift_node));
        singleerror!(result, v_tree.append_to(v_head, assign_node));
    }

    // Add the @(posedge clk) block if it's non-empty
    if !v_tree.get_node(clock_edge).unwrap().children.is_empty() {
        singleerror!(result, v_tree.append_to(v_head, clock_edge));
    }

    result.ok((module, v_tree));
    result
}

fn variable_name_relative(var_id: &VariablePath, index: i64) -> String {
    let var_id = var_id.to_verilog_name();
    if index == 1 {
        format!("{}_next", var_id)
    } else {
        format!("{}_{}", var_id, index.to_string().replace('-', "neg"))
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExternalModule {
    pub module_name: String,
    pub instance_path: Vec<String>,
    pub declaration: ModuleDeclaration,
}

fn recursive_external_module_helper(
    declarations: &[ModuleDeclaration],
    modules: &[Module],
    module: &Module,
    path: &[String],
) -> Vec<ExternalModule> {
    let mut result = vec![];
    for (instance_name, module_name) in module.instantiations.iter() {
        let declaration = declarations
            .iter()
            .find(|decl| &decl.name == module_name)
            .expect("Couldn't find module declaration");

        let mut path = path.to_vec();
        path.push(instance_name.to_string());

        if declaration.is_extern {
            result.push(ExternalModule {
                module_name: module_name.to_string(),
                instance_path: path,
                declaration: declaration.clone(),
            });
        } else {
            let module = modules
                .iter()
                .find(|module| &module.name == module_name)
                .expect("Couldn't find non-extern module");
            let mut v = recursive_external_module_helper(declarations, modules, module, &path);
            result.append(&mut v);
        }
    }
    result
}

/// Enumerates all external modules required in order to run the given top module.
pub fn collect_external_modules(
    top: &str,
    declarations: &[ModuleDeclaration],
    modules: &[Module],
) -> Result<Vec<ExternalModule>, ()> {
    // Find the top
    let top = match modules.iter().filter(|module| module.name == top).next() {
        Some(x) => x,
        None => {
            return Err(());
        }
    };

    Ok(recursive_external_module_helper(
        declarations,
        modules,
        top,
        &[],
    ))
}
