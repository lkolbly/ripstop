use crate::ast::{StringContext, Type};
use std::collections::{HashMap, HashSet};

use crate::{
    ast::{ASTNode, ASTNodeType},
    error::{CompileError, CompileResult},
    ir::{BinaryOperator, Expression, Module, UnaryOperator},
    logerror, singleerror,
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
        ASTNodeType::Add
        | ASTNodeType::Subtract
        | ASTNodeType::BitwiseAnd
        | ASTNodeType::BitwiseOr
        | ASTNodeType::BitwiseXor => {
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
        ASTNodeType::Equal
        | ASTNodeType::NotEqual
        | ASTNodeType::Greater
        | ASTNodeType::Less
        | ASTNodeType::GreaterEq
        | ASTNodeType::LessEq => {
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
                        Ok(Type::Bit)
                    } else {
                        err
                    }
                }
                _ => err,
            }
        }
        ASTNodeType::Concatenate => Ok(Type::Bits {
            size: child_vals[0].bit_size() + child_vals[1].bit_size(),
        }),

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
fn verify(tree: &Tree<ASTNode>, variables: &HashMap<String, VarBounds>) -> CompileResult<()> {
    let mut result = CompileResult::new();

    //Type verification, done bottom-up, keeping track of each nodes' return type
    //Done "recursively" for each assignment
    for n in tree {
        #[allow(clippy::single_match)] // Just for now, to silence the warning
        match &tree[n].data.node_type {
            ASTNodeType::Assign => {
                let children = tree[n].children.clone().unwrap();
                let lhs_t = singleerror!(
                    result,
                    tree.recurse_iterative(children[0], get_node_type, variables)
                );
                let rhs_t = singleerror!(
                    result,
                    tree.recurse_iterative(children[1], get_node_type, variables)
                );

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
                    result.error(CompileError::MismatchedTypes {
                        context: rhs.data.context.clone(),
                        current_type: rhs_t,
                        needed_type: lhs_t,
                    });
                }
            }
            _ => (),
        }
    }
    result.ok(());
    result
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
            vast.new_node(VNode::VariableReference { var_id: vref });
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
    let var_ref = tree.new_node(VNode::VariableReference { var_id });

    //Creating the index if needed
    if let Some(idx) = index {
        let idx = tree.new_node(idx);
        tree.append_to(idx, var_ref)?;
    }

    Ok(tree)
}

///Compiles a single module into Verilog from an AST
pub fn compile_module(tree: &mut Tree<ASTNode>) -> CompileResult<(Module, Tree<VNode>)> {
    let mut result = CompileResult::new();

    //A little bit of a workaround in order to make this work well with the ? operator
    let head = singleerror!(
        result,
        tree.find_head().ok_or(CompileError::CouldNotFindASTHead)
    );

    // Type-check to make sure everything'll work
    {
        //Stores pairs of (variable ID, (highest used t-offset, lowest used t-offset))
        //This is needed to create the registers
        let variables: HashMap<String, VarBounds> = singleerror!(result, get_var_bounds(tree));

        //Before creating the tree, verify it
        logerror!(result, verify(tree, &variables));
    }

    let module = logerror!(result, Module::from_ast(tree, head));

    //Creating the Verilog AST can now officially begin

    let mut v_tree = Tree::new();
    let v_head = {
        let mut in_values: Vec<_> = module
            .inputs
            .iter()
            .map(|input| (input, module.variables.get(input).unwrap()))
            .map(|(variable, vtype)| (variable.to_string(), vtype.bit_size()))
            .collect();

        let out_values: Vec<_> = module
            .outputs
            .iter()
            .map(|input| (input, module.variables.get(input).unwrap()))
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

    let is_input = |name: &str| module.inputs.iter().any(|in_name| in_name == name);
    let is_output = |name: &str| module.outputs.iter().any(|out_name| out_name == name);

    // Create a VNode to hold things that occur at the positive clock edge (i.e. always @(posedge clk))
    let clock_edge = v_tree.new_node(VNode::AlwaysBegin {
        trigger: AlwaysBeginTriggerType::Posedge,
    });

    //Register chain creation for each variable
    for (name, vartype) in module.variables.clone().iter() {
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
                            var_name: name.clone(),
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

            singleerror!(result, v_tree.append_to(head, assign_node));
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

            let mut rhs = if let Some(reset_value) = reset_value {
                let mut conditional_tree = Tree::new();
                let conditional = conditional_tree.new_node(VNode::Ternary {});
                let rst_ref = conditional_tree.new_node(VNode::VariableReference {
                    var_id: "rst".to_owned(),
                });
                let reset_value = conditional_tree.new_node(VNode::NumberLiteral {
                    literal: *reset_value,
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
        singleerror!(result, v_tree.append_to(v_head, declaration));

        singleerror!(result, v_tree.append_to(declaration, reg_head));

        // x_0 <= x_next
        let mut lhs = singleerror!(result, compile_var_ref_from_string(var_id, Some(0), None));

        let reset_value = module.reset_values.get(var_id);

        let mut rhs = if let Some(reset_value) = reset_value {
            let mut conditional_tree = Tree::new();
            let conditional = conditional_tree.new_node(VNode::Ternary {});
            let rst_ref = conditional_tree.new_node(VNode::VariableReference {
                var_id: "rst".to_owned(),
            });
            let reset_value = conditional_tree.new_node(VNode::NumberLiteral {
                literal: *reset_value,
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
                    var_id: format!("{}_next", var_id),
                })
            }
        };

        let assign_vnode = v_tree.new_node(VNode::AssignKeyword {});
        singleerror!(result, v_tree.append_to(head, assign_vnode));

        singleerror!(result, v_tree.append_to(assign_vnode, lhs_vnode));

        if !rhs.is_combinatorial() {
            let mut rhs = rhs.clone();
            rhs.shift_time(1);
            let mut rhs = singleerror!(result, compile_ir_expression(&rhs));
            singleerror!(result, v_tree.append_tree(assign_vnode, &mut rhs));
        } else {
            let mut rhs = singleerror!(result, compile_ir_expression(rhs));
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
        singleerror!(result, v_tree.append_to(v_head, clock_edge));
    }

    result.ok((module, v_tree));
    result
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
