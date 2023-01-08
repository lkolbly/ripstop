use serde::{Deserialize, Serialize};

use crate::{ast::Type, ir::ModuleDeclaration};
use std::collections::HashMap;

use crate::{
    ast::{ASTNode, ASTNodeType},
    error::{CompileError, CompileResult},
    ir::{BinaryOperator, Expression, LogicalExpression, Module, UnaryOperator},
    logerror, noncriterr, singleerror,
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
                context: tree[this_node.children.clone()[0]].data.context.clone(),
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
                context: tree[this_node.children.clone()[0]].data.context.clone(),
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
                let children = tree[n].children.clone();
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
fn get_var_bounds(
    tree: &Tree<ASTNode>,
    modules: &Vec<ModuleDeclaration>,
) -> Result<HashMap<String, VarBounds>, CompileError> {
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
                doc_comment: _,
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
            ASTNodeType::ModuleInstantiation { module, instance } => {
                let module = modules
                    .iter()
                    .filter(|m| &m.name == module)
                    .next()
                    // TODO: Make this an error
                    .expect("Couldn't find module");
                for (vartype, varname) in module.inputs.iter().chain(module.outputs.iter()) {
                    variables.insert(
                        format!("{}.{}", instance, varname),
                        VarBounds::new(0, VarScope::Local, *vartype),
                    );
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
                let lhs = tree.get_node(nodeid).unwrap().children[0];
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
            vast.new_node(VNode::NumberLiteral { literal: *literal });
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

pub fn compile_document(
    tree: &mut Tree<ASTNode>,
) -> CompileResult<(Vec<ModuleDeclaration>, Vec<Module>, Tree<VNode>)> {
    let mut result = CompileResult::new();

    let declarations = logerror!(result, get_module_declarations(tree));

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
                    noncriterr!(result, compile_module(tree, child, &declarations))
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

fn get_module_declarations(tree: &mut Tree<ASTNode>) -> CompileResult<Vec<ModuleDeclaration>> {
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
                if let Some(module) = noncriterr!(result, ModuleDeclaration::from_ast(tree, *child))
                {
                    modules.push(module);
                }
            }
            ASTNodeType::ExternModuleDeclaration {
                id,
                doc_comment,
                in_values,
                out_values,
            } => {
                if let Some(module) = noncriterr!(result, ModuleDeclaration::from_ast(tree, *child))
                {
                    modules.push(module);
                }
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
) -> CompileResult<(Module, Tree<VNode>)> {
    let mut result = CompileResult::new();

    // Type-check to make sure everything'll work
    {
        //Stores pairs of (variable ID, (highest used t-offset, lowest used t-offset))
        //This is needed to create the registers
        let variables: HashMap<String, VarBounds> =
            singleerror!(result, get_var_bounds(tree, modules));

        //Before creating the tree, verify it
        logerror!(result, verify(tree, &variables));
    }

    let module = logerror!(result, Module::from_ast(tree, head, modules));

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

    // Instantiate each module instantiation
    for (instance, module) in module.instantiations.iter() {
        let module = modules
            .iter()
            .filter(|m| &m.name == module)
            .next()
            .expect("Couldn't find module");

        let v_instance = v_tree.new_node(VNode::ModuleInstantiation {
            module: module.name.clone(),
            instance: instance.to_owned(),
        });

        for (vartype, portname, varname, declare) in module
            .inputs
            .iter()
            .chain(module.outputs.iter())
            .map(|(t, n)| (*t, n.to_owned(), format!("{}_{}", instance, n), true))
            .chain(std::iter::once((
                Type::Bit,
                "clk".to_owned(),
                "clk".to_owned(),
                false,
            )))
            .chain(std::iter::once((
                Type::Bit,
                "rst".to_owned(),
                "rst".to_owned(),
                false,
            )))
        {
            let v_port = v_tree.new_node(VNode::ModuleInstantiationVariable {
                portname: portname.to_owned(),
                variable: varname.clone(),
            });
            singleerror!(result, v_tree.append_to(v_instance, v_port));

            if declare {
                // Also declare that variable as a wire
                let v_decl = v_tree.new_node(VNode::WireDeclare {
                    bits: vartype.bit_size(),
                });
                let v_varref = v_tree.new_node(VNode::VariableReference {
                    var_id: varname.clone(),
                });
                singleerror!(result, v_tree.append_to(v_decl, v_varref));
                singleerror!(result, v_tree.append_to(v_head, v_decl));

                // And assign it appropriately
                let is_input = module
                    .inputs
                    .iter()
                    .filter(|(_, n)| n == &portname)
                    .next()
                    .is_some();
                if is_input {
                    let v_assign = v_tree.new_node(VNode::AssignKeyword {});
                    let lhs = v_tree.new_node(VNode::VariableReference {
                        var_id: varname.clone(),
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
                        var_id: varname.clone(),
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
    for (name, _vartype) in module.variables.clone().iter() {
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

fn variable_name_relative(var_id: &str, index: i64) -> String {
    let var_id = var_id.to_owned().replace(".", "_");
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
