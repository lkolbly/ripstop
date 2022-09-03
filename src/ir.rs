/// Intermediate Representation. This is the representation that sits between the
/// ASTNodes, which represent what was parsed, and the VNodes, which represent what
/// we're outputting.
///
/// The IR is time-aware.
use std::collections::HashMap;

use pest::prec_climber::Operator;

use crate::ast::{ASTNode, ASTNodeType, Type};
use crate::error::{CompileError, CompileResult};
use crate::parse::{NumberLiteral, Range};
use crate::tree::{NodeId, Tree};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TimeReference {
    Absolute(i64),
    Relative(i64),
}

impl TimeReference {
    fn from_ast(ast: &Tree<ASTNode>, node: NodeId) -> Self {
        match &ast.get_node(node).unwrap().data.node_type {
            ASTNodeType::TimeOffsetAbsolute { time } => Self::Absolute(*time),
            ASTNodeType::TimeOffsetRelative { offset } => Self::Relative(*offset),
            _ => {
                panic!("Unexpected node!");
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Negation,
    Index(Range),
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Addition,
    Subtraction,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEq,
    LessEq,
    Concatenate,
}

impl BinaryOperator {
    fn from_ast(n: ASTNodeType) -> Option<Self> {
        match n {
            ASTNodeType::Add => Some(Self::Addition),
            ASTNodeType::Subtract => Some(Self::Subtraction),
            ASTNodeType::BitwiseAnd => Some(Self::BitwiseAnd),
            ASTNodeType::BitwiseOr => Some(Self::BitwiseOr),
            ASTNodeType::BitwiseXor => Some(Self::BitwiseXor),
            ASTNodeType::Equal => Some(Self::Equal),
            ASTNodeType::NotEqual => Some(Self::NotEqual),
            ASTNodeType::Greater => Some(Self::Greater),
            ASTNodeType::Less => Some(Self::Less),
            ASTNodeType::GreaterEq => Some(Self::GreaterEq),
            ASTNodeType::LessEq => Some(Self::LessEq),
            ASTNodeType::Concatenate => Some(Self::Concatenate),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariableReference {
    pub variable: String,
    pub time: TimeReference,
}

impl VariableReference {
    fn from_ast(ast: &Tree<ASTNode>, node: NodeId) -> Self {
        match &ast.get_node(node).unwrap().data.node_type {
            ASTNodeType::VariableReference { var_id } => Self {
                variable: var_id.to_string(),
                time: TimeReference::from_ast(ast, ast.get_first_child(node).unwrap().id),
            },
            _ => {
                panic!("Unexpected node type");
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    UnaryOperation {
        operation: UnaryOperator,
        operatee: Box<Expression>,
    },

    BinaryOperation {
        operation: BinaryOperator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },

    Ternary {
        condition: Box<Expression>,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },

    VariableReference(VariableReference),

    NumberLiteral(NumberLiteral),
}

impl Expression {
    fn from_ast(ast: &Tree<ASTNode>, node: NodeId) -> Box<Self> {
        let node = match &ast.get_node(node).unwrap().data.node_type {
            ASTNodeType::BitwiseInverse => Self::UnaryOperation {
                operation: UnaryOperator::Negation,
                operatee: Self::from_ast(ast, ast.get_first_child(node).unwrap().id),
            },
            ASTNodeType::VariableReference { var_id: _ } => {
                Self::VariableReference(VariableReference::from_ast(ast, node))
            }
            ASTNodeType::NumberLiteral(literal) => Expression::NumberLiteral(*literal),
            ASTNodeType::Index { high, low } => Expression::UnaryOperation {
                operation: UnaryOperator::Index(Range {
                    low: *low as u32,
                    high: *high as u32,
                }),
                operatee: Self::from_ast(ast, ast.get_first_child(node).unwrap().id),
            },
            nodetype => {
                if let Some(operator) = BinaryOperator::from_ast(nodetype.clone()) {
                    Self::BinaryOperation {
                        operation: operator,
                        lhs: Self::from_ast(ast, ast.get_child_node(node, 0).unwrap().id),
                        rhs: Self::from_ast(ast, ast.get_child_node(node, 1).unwrap().id),
                    }
                } else {
                    todo!();
                }
            }
        };
        Box::new(node)
    }

    /// Shifts the time of this expression by the given amount
    pub fn shift_time(&mut self, offset: i64) {
        match self {
            Self::BinaryOperation {
                operation: _,
                lhs,
                rhs,
            } => {
                lhs.shift_time(offset);
                rhs.shift_time(offset);
            }
            Self::UnaryOperation {
                operation: _,
                operatee,
            } => {
                operatee.shift_time(offset);
            }
            Self::Ternary {
                condition,
                lhs,
                rhs,
            } => {
                condition.shift_time(offset);
                lhs.shift_time(offset);
                rhs.shift_time(offset);
            }
            Self::VariableReference(reference) => match &mut reference.time {
                TimeReference::Absolute(_) => {
                    panic!("Can't shift time of an absolute time (reset value on RHS?)");
                }
                TimeReference::Relative(varoff) => {
                    *varoff += offset;
                }
            },
            Self::NumberLiteral(_) => {}
            _ => unimplemented!(),
        }
    }

    /// Finds the oldest reference to the given variable in this expression
    fn get_oldest_reference(&self, variable: &str) -> Option<i64> {
        match self {
            Self::BinaryOperation {
                operation: _,
                lhs,
                rhs,
            } => {
                let lhs_oldest = lhs.get_oldest_reference(variable);
                let rhs_oldest = rhs.get_oldest_reference(variable);
                match (lhs_oldest, rhs_oldest) {
                    (Some(a), Some(b)) => Some(std::cmp::min(a, b)),
                    (Some(a), None) => Some(a),
                    (None, Some(b)) => Some(b),
                    (None, None) => None,
                }
            }
            Self::UnaryOperation {
                operation: _,
                operatee,
            } => operatee.get_oldest_reference(variable),
            Self::Ternary {
                condition,
                lhs,
                rhs,
            } => {
                let a = condition.get_oldest_reference(variable);
                let b = lhs.get_oldest_reference(variable);
                let c = rhs.get_oldest_reference(variable);
                let min = a;
                let min = match (min, b) {
                    (Some(min), Some(b)) => Some(std::cmp::min(min, b)),
                    (Some(min), None) => Some(min),
                    (None, Some(b)) => Some(b),
                    (None, None) => None,
                };
                match (min, c) {
                    (Some(min), Some(b)) => Some(std::cmp::min(min, b)),
                    (Some(min), None) => Some(min),
                    (None, Some(b)) => Some(b),
                    (None, None) => None,
                }
            }
            Self::VariableReference(reference) => {
                if reference.variable == variable {
                    if let TimeReference::Relative(offset) = reference.time {
                        Some(offset)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Self::NumberLiteral(_) => None,
            _ => {
                unimplemented!();
            }
        }
    }

    /// Determines whether the given expression must be computed combinatorially
    /// (i.e., there exists a variable that is referenced now)
    pub fn is_combinatorial(&self) -> bool {
        match self {
            Self::BinaryOperation {
                operation: _,
                lhs,
                rhs,
            } => lhs.is_combinatorial() || rhs.is_combinatorial(),
            Self::UnaryOperation {
                operation: _,
                operatee,
            } => operatee.is_combinatorial(),
            Self::Ternary {
                condition,
                lhs,
                rhs,
            } => condition.is_combinatorial() || lhs.is_combinatorial() || rhs.is_combinatorial(),
            Self::VariableReference(reference) => reference.time == TimeReference::Relative(0),
            Self::NumberLiteral(_) => false,
            _ => {
                unimplemented!("Can't determine whether {:?} is combinatorial", self);
            }
        }
    }

    fn depends_on_future(&self) -> bool {
        match self {
            Self::BinaryOperation {
                operation: _,
                lhs,
                rhs,
            } => lhs.depends_on_future() || rhs.depends_on_future(),
            Self::UnaryOperation {
                operation: _,
                operatee,
            } => operatee.depends_on_future(),
            Self::Ternary {
                condition,
                lhs,
                rhs,
            } => {
                condition.depends_on_future() || lhs.depends_on_future() || rhs.depends_on_future()
            }
            Self::VariableReference(VariableReference {
                time: TimeReference::Relative(offset),
                ..
            }) => *offset > 0,
            Self::VariableReference(_) => false,
            Self::NumberLiteral(_) => false,
            _ => {
                unimplemented!();
            }
        }
    }
}

#[derive(Debug)]
pub struct Block {
    pub assignments: HashMap<String, Box<Expression>>,
}

impl Block {
    fn from_ast(ast: &Tree<ASTNode>, children: &[NodeId], declaration_allowed: bool) -> Self {
        let mut assignments = Self::from_ast_with_offsets(ast, children, declaration_allowed);
        Self {
            assignments: assignments
                .drain()
                .map(|(variable, (offset, mut expr))| {
                    expr.shift_time(-offset);
                    if expr.depends_on_future() {
                        panic!("Expression depends on the future!");
                    }
                    (variable, expr)
                })
                .collect(),
        }
    }

    fn from_ast_with_offsets(
        ast: &Tree<ASTNode>,
        children: &[NodeId],
        declaration_allowed: bool,
    ) -> HashMap<String, (i64, Box<Expression>)> {
        let mut assignments = HashMap::new();
        for child in children.iter() {
            match &ast.get_node(*child).unwrap().data.node_type {
                ASTNodeType::Assign => {
                    let lhs =
                        VariableReference::from_ast(ast, ast.get_first_child(*child).unwrap().id);
                    match lhs.time {
                        TimeReference::Absolute(_) => {
                            if declaration_allowed {
                                // This is allowed, we'll ignore it
                            } else {
                                panic!("Reset values are not allowed in this context");
                            }
                        }
                        TimeReference::Relative(offset) => {
                            let rhs = Expression::from_ast(
                                ast,
                                ast.get_child_node(*child, 1).unwrap().id,
                            );
                            if assignments.contains_key(&lhs.variable) {
                                panic!("Can't assign variable multiple times!");
                            }
                            assignments.insert(lhs.variable, (offset, rhs));
                        }
                    }
                }
                ASTNodeType::Conditional => {
                    let mut new_assignments = Self::from_conditional_children(
                        ast,
                        ast.get_node(*child).unwrap().children.as_ref().unwrap(),
                    );
                    for (k, v) in new_assignments.drain() {
                        assignments.insert(k, v);
                    }
                }
                ASTNodeType::VariableDeclaration { .. } => {
                    if !declaration_allowed {
                        panic!("Declaration not allowed in this context");
                    }
                    // Otherwise, we purposefully ignore this
                }
                ASTNodeType::ModuleInstantiation { module, instance } => {
                    if !declaration_allowed {
                        panic!("Module instantiation not allowed in this context");
                    }
                    // Otherwise, we purposefully ignore this
                }
                _ => {
                    panic!(
                        "Unexpected node {:?}",
                        ast.get_node(*child).unwrap().data.node_type
                    );
                }
            }
        }
        assignments
    }

    /// Constructs a block from the children of a conditional node
    /// Note that this function is recursive: It gets called for each else-if
    /// in the chain, and for the else itself.
    fn from_conditional_children(
        ast: &Tree<ASTNode>,
        children: &[NodeId],
    ) -> HashMap<String, (i64, Box<Expression>)> {
        if children.len() == 0 {
            // This must be after a else-if case, but with no following else.
            return HashMap::new();
        }
        match &ast.get_node(children[0]).unwrap().data.node_type {
            ASTNodeType::Block => {
                // This is an unconditional else
                Block::from_ast_with_offsets(
                    ast,
                    ast.get_node(children[0])
                        .unwrap()
                        .children
                        .as_ref()
                        .unwrap(),
                    false,
                )
            }
            _ => {
                // This is the condition for the next branch
                let condition = Expression::from_ast(ast, children[0]);
                let mut iftrue = Block::from_ast_with_offsets(
                    ast,
                    ast.get_node(children[1])
                        .unwrap()
                        .children
                        .as_ref()
                        .unwrap(),
                    false,
                );
                let mut iffalse = Block::from_conditional_children(ast, &children[2..]);

                //let mut assignments = iftrue.assignments;
                let mut assignments = HashMap::new();
                for (variable, value) in iffalse.drain() {
                    if iftrue.contains_key(&variable) {
                        // Build a ternary

                        let (true_offset, iftrue) = iftrue.remove(&variable).unwrap();
                        let (false_offset, iffalse) = value;

                        if true_offset != false_offset {
                            panic!("A given variable must be assigned at the same time step across conditional branches");
                        }

                        assignments.insert(
                            variable,
                            (
                                true_offset,
                                Box::new(Expression::Ternary {
                                    condition: condition.clone(),
                                    lhs: iftrue,
                                    rhs: iffalse,
                                }),
                            ),
                        );
                    } else {
                        // Build a ternary where the if-true is the previous value
                        let (false_offset, iffalse) = value;
                        let iftrue = Box::new(Expression::VariableReference(VariableReference {
                            variable: variable.clone(),
                            time: TimeReference::Relative(false_offset - 1),
                        }));

                        assignments.insert(
                            variable,
                            (
                                false_offset,
                                Box::new(Expression::Ternary {
                                    condition: condition.clone(),
                                    lhs: iftrue,
                                    rhs: iffalse,
                                }),
                            ),
                        );
                    }
                }

                // Everything left in iftrue will not be set in the false branch
                for (variable, value) in iftrue.drain() {
                    let (true_offset, iftrue) = value;
                    let iffalse = Box::new(Expression::VariableReference(VariableReference {
                        variable: variable.clone(),
                        time: TimeReference::Relative(true_offset - 1),
                    }));

                    assignments.insert(
                        variable,
                        (
                            true_offset,
                            Box::new(Expression::Ternary {
                                condition: condition.clone(),
                                lhs: iftrue,
                                rhs: iffalse,
                            }),
                        ),
                    );
                }

                assignments
            }
        }
    }

    pub fn variable_reference_range(&self, varname: &str) -> std::ops::Range<i64> {
        let mut min = std::i64::MAX;
        let max = 0;
        for (_, v) in self.assignments.iter() {
            min = std::cmp::min(min, v.get_oldest_reference(varname).unwrap_or(0));
        }

        // TODO: Tighten this down a bit
        (min..max)
    }
}

#[derive(Debug)]
pub struct ModuleDeclaration {
    pub name: String,
    pub inputs: Vec<(Type, String)>,
    pub outputs: Vec<(Type, String)>,
}

impl ModuleDeclaration {
    pub fn from_ast(ast: &Tree<ASTNode>, node: NodeId) -> CompileResult<Self> {
        let mut result = CompileResult::new();

        let (id, in_values, out_values) = match &ast[node].data.node_type {
            ASTNodeType::ModuleDeclaration {
                id,
                in_values,
                out_values,
            } => (id, in_values, out_values),
            _ => {
                panic!("Tried to compile module which wasn't of type Node::ModuleDeclaration");
            }
        };

        result.ok(Self {
            name: id.to_string(),
            inputs: in_values.to_owned(),
            outputs: out_values.to_owned(),
        });
        result
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub inputs: Vec<String>,
    pub outputs: Vec<String>,
    pub instantiations: HashMap<String, String>,
    pub variables: HashMap<String, Type>,
    pub reset_values: HashMap<String, (NumberLiteral, i64)>,
    pub block: Block,
}

impl Module {
    pub fn from_ast(
        ast: &Tree<ASTNode>,
        head: NodeId,
        modules: &Vec<ModuleDeclaration>,
    ) -> CompileResult<Self> {
        let mut result = CompileResult::new();

        let (id, in_values, out_values) = match &ast[head].data.node_type {
            ASTNodeType::ModuleDeclaration {
                id,
                in_values,
                out_values,
            } => (id, in_values, out_values),
            _ => {
                panic!("Tried to compile module which wasn't of type Node::ModuleDeclaration");
            }
        };

        // Get all the module instantiations
        let instantiations: HashMap<String, String> = ast
            .get_node(head)
            .unwrap()
            .children
            .as_ref()
            .unwrap()
            .iter()
            .flat_map(|n| match &ast.get_node(*n).unwrap().data.node_type {
                ASTNodeType::ModuleInstantiation { module, instance } => {
                    Some((instance.to_owned(), module.to_owned()))
                }
                _ => None,
            })
            .collect();

        // Grab all the variable declarations
        let variables: HashMap<_, _> = ast
            .get_node(head)
            .unwrap()
            .children
            .as_ref()
            .unwrap()
            .iter()
            .flat_map(|n| match &ast.get_node(*n).unwrap().data.node_type {
                ASTNodeType::VariableDeclaration { var_type, var_id } => {
                    Some((var_id.to_string(), *var_type))
                }
                _ => None,
            })
            .chain(in_values.iter().map(|(a, b)| (b.to_string(), *a)))
            .chain(out_values.iter().map(|(a, b)| (b.to_string(), *a)))
            .chain(instantiations.iter().flat_map(|(instance, module)| {
                let module = match modules.iter().filter(|m| &m.name == module).next() {
                    Some(m) => m,
                    None => panic!("Could not find module declaration {}", module),
                };
                let mut module_variables = vec![];
                for (var_type, var_name) in module.inputs.iter().chain(module.outputs.iter()) {
                    module_variables
                        .push((format!("{}.{}", instance, var_name), var_type.to_owned()));
                }
                module_variables
            }))
            .collect();

        // Grab all the reset values
        let reset_values: HashMap<_, _> = ast
            .get_node(head)
            .unwrap()
            .children
            .as_ref()
            .unwrap()
            .iter()
            .flat_map(|n| {
                match &ast.get_node(*n).unwrap().data.node_type {
                    ASTNodeType::Assign => {
                        let lhs =
                            VariableReference::from_ast(ast, ast.get_first_child(*n).unwrap().id);
                        if lhs.time == TimeReference::Absolute(0)
                            || lhs.time == TimeReference::Absolute(-1)
                        {
                            // This is a reset value
                            let timeval = match lhs.time {
                                TimeReference::Absolute(n) => n,
                                _ => unreachable!(),
                            };
                            let rhs =
                                Expression::from_ast(ast, ast.get_child_node(*n, 1).unwrap().id);
                            match *rhs {
                                Expression::NumberLiteral(literal) => {
                                    Some((lhs.variable, (literal, timeval)))
                                }
                                _ => {
                                    result.error(CompileError::InvalidResetValue {
                                        context: ast
                                            .get_child_node(*n, 1)
                                            .unwrap()
                                            .data
                                            .context
                                            .clone(),
                                    });
                                    None
                                }
                            }
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            })
            .collect();

        // Now build the assigns themselves
        let block = Block::from_ast(
            ast,
            ast.get_node(head).unwrap().children.as_ref().unwrap(),
            true,
        );

        result.ok(Self {
            name: id.to_string(),
            inputs: in_values.iter().map(|(_, name)| name.to_string()).collect(),
            outputs: out_values
                .iter()
                .map(|(_, name)| name.to_string())
                .collect(),
            instantiations,
            variables,
            reset_values,
            block,
        });

        result
    }
}
