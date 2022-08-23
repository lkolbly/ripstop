/// Intermediate Representation. This is the representation that sits between the
/// ASTNodes, which represent what was parsed, and the VNodes, which represent what
/// we're outputting.
///
/// The IR is time-aware.
use std::collections::HashMap;

use crate::ast::{ASTNode, ASTNodeType, Type};
use crate::compile::CompileError;
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
            ASTNodeType::Add => Self::BinaryOperation {
                operation: BinaryOperator::Addition,
                lhs: Self::from_ast(ast, ast.get_child_node(node, 0).unwrap().id),
                rhs: Self::from_ast(ast, ast.get_child_node(node, 1).unwrap().id),
            },
            ASTNodeType::BitwiseInverse => Self::UnaryOperation {
                operation: UnaryOperator::Negation,
                operatee: Self::from_ast(ast, ast.get_first_child(node).unwrap().id),
            },
            ASTNodeType::VariableReference { var_id } => {
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
            _ => {
                todo!();
            }
        };
        Box::new(node)
    }

    /// Shifts the time of this expression by the given amount
    pub fn shift_time(&mut self, offset: i64) {
        match self {
            Self::BinaryOperation {
                operation,
                lhs,
                rhs,
            } => {
                lhs.shift_time(offset);
                rhs.shift_time(offset);
            }
            Self::UnaryOperation {
                operation,
                operatee,
            } => {
                operatee.shift_time(offset);
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
                operation,
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
                operation,
                operatee,
            } => operatee.get_oldest_reference(variable),
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
                operation,
                lhs,
                rhs,
            } => lhs.is_combinatorial() || rhs.is_combinatorial(),
            Self::UnaryOperation {
                operation,
                operatee,
            } => operatee.is_combinatorial(),
            Self::VariableReference(reference) => reference.time == TimeReference::Relative(0),
            Self::NumberLiteral(_) => false,
            _ => {
                unimplemented!("Can't determine whether {:?} is combinatorial", self);
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
                            let mut rhs = Expression::from_ast(
                                ast,
                                ast.get_child_node(*child, 1).unwrap().id,
                            );
                            rhs.shift_time(-offset);
                            if assignments.contains_key(&lhs.variable) {
                                panic!("Can't assign variable multiple times!");
                            }
                            assignments.insert(lhs.variable, rhs);
                        }
                    }
                }
                ASTNodeType::Conditional => {
                    unimplemented!();
                }
                ASTNodeType::VariableDeclaration { .. } => {
                    if !declaration_allowed {
                        panic!("Declaration not allowed in this context");
                    }
                    // Otherwise, we purposefully ignore this
                }
                _ => {
                    panic!("Unexpected node");
                }
            }
        }
        Self { assignments }
    }

    pub fn variable_reference_range(&self, varname: &str) -> std::ops::Range<i64> {
        let mut min = std::i64::MAX;
        let mut max = 0; //std::i64::MIN;
        for (k, v) in self.assignments.iter() {
            min = std::cmp::min(min, v.get_oldest_reference(varname).unwrap_or(0));
        }

        // TODO: Tighten this down a bit
        (min..max)
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub inputs: Vec<String>,
    pub outputs: Vec<String>,
    pub variables: HashMap<String, Type>,
    pub reset_values: HashMap<String, NumberLiteral>,
    pub block: Block,
}

impl Module {
    pub fn from_ast(ast: &Tree<ASTNode>, node: NodeId) -> Result<Self, CompileError> {
        let head = ast.find_head().ok_or(CompileError::CouldNotFindASTHead)?;

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
                            let rhs =
                                Expression::from_ast(ast, ast.get_child_node(*n, 1).unwrap().id);
                            let literal = match *rhs {
                                Expression::NumberLiteral(literal) => literal,
                                _ => {
                                    panic!("RHS of reset value must be a literal");
                                }
                            };
                            Some((lhs.variable, literal))
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

        Ok(Self {
            name: id.to_string(),
            inputs: in_values.iter().map(|(_, name)| name.to_string()).collect(),
            outputs: out_values
                .iter()
                .map(|(_, name)| name.to_string())
                .collect(),
            variables,
            reset_values,
            block,
        })
    }
}
