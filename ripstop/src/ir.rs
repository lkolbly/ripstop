/// Intermediate Representation. This is the representation that sits between the
/// ASTNodes, which represent what was parsed, and the VNodes, which represent what
/// we're outputting.
///
/// The IR is time-aware.
use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::ast::{ASTNode, ASTNodeType, StringContext};
use crate::error::{CompileError, CompileResult};
use crate::parse::{NumberLiteral, Range};
use crate::tree::{NodeId, Tree};
use crate::types::{Type, TypeDatabase};
use crate::{logerror, noncriterr};

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

#[derive(Debug, Clone, Copy)]
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
    pub variable: VariablePath,
    pub time: TimeReference,
}

impl VariableReference {
    fn from_ast(ast: &Tree<ASTNode>, node: NodeId) -> Self {
        match &ast.get_node(node).unwrap().data.node_type {
            ASTNodeType::VariableReference { var_id } => Self {
                variable: VariablePath::from_string(var_id.to_string()),
                time: TimeReference::from_ast(ast, ast.get_first_child(node).unwrap().id),
            },
            _ => {
                panic!("Unexpected node type");
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum LogicalExpression {
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

#[derive(Debug, Clone)]
pub struct Expression {
    pub expr: LogicalExpression,
    pub context: StringContext,
}

impl Expression {
    fn from_ast(ast: &Tree<ASTNode>, nodeid: NodeId) -> Box<Self> {
        let node = match &ast.get_node(nodeid).unwrap().data.node_type {
            ASTNodeType::BitwiseInverse => LogicalExpression::UnaryOperation {
                operation: UnaryOperator::Negation,
                operatee: Self::from_ast(ast, ast.get_first_child(nodeid).unwrap().id),
            },
            ASTNodeType::VariableReference { var_id: _ } => {
                LogicalExpression::VariableReference(VariableReference::from_ast(ast, nodeid))
            }
            ASTNodeType::NumberLiteral(literal) => {
                LogicalExpression::NumberLiteral(literal.clone())
            }
            ASTNodeType::Index { high, low } => LogicalExpression::UnaryOperation {
                operation: UnaryOperator::Index(Range {
                    low: *low as u32,
                    high: *high as u32,
                }),
                operatee: Self::from_ast(ast, ast.get_first_child(nodeid).unwrap().id),
            },
            nodetype => {
                if let Some(operator) = BinaryOperator::from_ast(nodetype.clone()) {
                    LogicalExpression::BinaryOperation {
                        operation: operator,
                        lhs: Self::from_ast(ast, ast.get_child_node(nodeid, 0).unwrap().id),
                        rhs: Self::from_ast(ast, ast.get_child_node(nodeid, 1).unwrap().id),
                    }
                } else {
                    todo!();
                }
            }
        };
        Box::new(Self {
            expr: node,
            context: ast.get_node(nodeid).unwrap().data.context.clone(),
        })
    }

    /// Shifts the time of this expression by the given amount
    pub fn shift_time(&mut self, offset: i64) {
        match &mut self.expr {
            LogicalExpression::BinaryOperation {
                operation: _,
                lhs,
                rhs,
            } => {
                lhs.shift_time(offset);
                rhs.shift_time(offset);
            }
            LogicalExpression::UnaryOperation {
                operation: _,
                operatee,
            } => {
                operatee.shift_time(offset);
            }
            LogicalExpression::Ternary {
                condition,
                lhs,
                rhs,
            } => {
                condition.shift_time(offset);
                lhs.shift_time(offset);
                rhs.shift_time(offset);
            }
            LogicalExpression::VariableReference(reference) => match &mut reference.time {
                TimeReference::Absolute(_) => {
                    panic!("Can't shift time of an absolute time (reset value on RHS?)");
                }
                TimeReference::Relative(varoff) => {
                    *varoff += offset;
                }
            },
            LogicalExpression::NumberLiteral(_) => {}
        }
    }

    /// Finds the oldest reference to the given variable in this expression
    fn get_oldest_reference(&self, variable: &VariablePath) -> Option<i64> {
        match &self.expr {
            LogicalExpression::BinaryOperation {
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
            LogicalExpression::UnaryOperation {
                operation: _,
                operatee,
            } => operatee.get_oldest_reference(variable),
            LogicalExpression::Ternary {
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
            LogicalExpression::VariableReference(reference) => {
                if reference.variable.subset_of(&variable) {
                    if let TimeReference::Relative(offset) = reference.time {
                        Some(offset)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            LogicalExpression::NumberLiteral(_) => None,
        }
    }

    /// Determines whether the given expression must be computed combinatorially
    /// (i.e., there exists a variable that is referenced now)
    pub fn is_combinatorial(&self) -> bool {
        match &self.expr {
            LogicalExpression::BinaryOperation {
                operation: _,
                lhs,
                rhs,
            } => lhs.is_combinatorial() || rhs.is_combinatorial(),
            LogicalExpression::UnaryOperation {
                operation: _,
                operatee,
            } => operatee.is_combinatorial(),
            LogicalExpression::Ternary {
                condition,
                lhs,
                rhs,
            } => condition.is_combinatorial() || lhs.is_combinatorial() || rhs.is_combinatorial(),
            LogicalExpression::VariableReference(reference) => {
                reference.time == TimeReference::Relative(0)
            }
            LogicalExpression::NumberLiteral(_) => false,
        }
    }

    fn depends_on_future(&self) -> bool {
        match &self.expr {
            LogicalExpression::BinaryOperation {
                operation: _,
                lhs,
                rhs,
            } => lhs.depends_on_future() || rhs.depends_on_future(),
            LogicalExpression::UnaryOperation {
                operation: _,
                operatee,
            } => operatee.depends_on_future(),
            LogicalExpression::Ternary {
                condition,
                lhs,
                rhs,
            } => {
                condition.depends_on_future() || lhs.depends_on_future() || rhs.depends_on_future()
            }
            LogicalExpression::VariableReference(VariableReference {
                time: TimeReference::Relative(offset),
                ..
            }) => *offset > 0,
            LogicalExpression::VariableReference(_) => false,
            LogicalExpression::NumberLiteral(_) => false,
        }
    }

    fn perform_arithemetic(operator: BinaryOperator, lhs: u128, rhs: u128) -> u128 {
        match operator {
            BinaryOperator::Addition => lhs.checked_add(rhs).expect("Need bigint"),
            BinaryOperator::Subtraction => {
                lhs.checked_sub(rhs).expect("Need negative literal support")
            }
            BinaryOperator::BitwiseAnd => lhs & rhs,
            BinaryOperator::BitwiseOr => lhs | rhs,
            BinaryOperator::BitwiseXor => lhs ^ rhs,
            _ => {
                panic!("Must call perform_arithmetic with an arithmetical operator!");
            }
        }
    }

    fn perform_comparison(operator: BinaryOperator, lhs: u128, rhs: u128) -> bool {
        match operator {
            BinaryOperator::Equal => lhs == rhs,
            BinaryOperator::Greater => lhs > rhs,
            BinaryOperator::GreaterEq => lhs >= rhs,
            BinaryOperator::Less => lhs < rhs,
            BinaryOperator::LessEq => lhs <= rhs,
            BinaryOperator::NotEqual => lhs != rhs,
            _ => {
                panic!("Must call perform_comparison with a comparison operator!");
            }
        }
    }

    fn combine_literals(
        &self,
        operator: BinaryOperator,
        lhs: u128,
        lhs_minimum_size: usize,
        rhs: u128,
        rhs_minimum_size: usize,
    ) -> CompileResult<(Expression, Vec<(Type, StringContext)>)> {
        let mut result = CompileResult::new();
        match operator {
            BinaryOperator::Addition
            | BinaryOperator::Subtraction
            | BinaryOperator::BitwiseAnd
            | BinaryOperator::BitwiseOr
            | BinaryOperator::BitwiseXor => {
                let newval = Self::perform_arithemetic(operator, lhs, rhs);
                let new_size_bits: usize = (newval + 1)
                    .next_power_of_two()
                    .trailing_zeros()
                    .try_into()
                    .unwrap();
                let new_size_bits = new_size_bits.max(lhs_minimum_size).max(rhs_minimum_size);
                result.ok((
                    Expression {
                        expr: LogicalExpression::NumberLiteral(NumberLiteral {
                            value: newval,
                            ty: Type::Literal {
                                minimum_size: new_size_bits,
                            },
                        }),
                        context: self.context.clone(),
                    },
                    vec![(
                        Type::Literal {
                            minimum_size: new_size_bits,
                        },
                        self.context.clone(),
                    )],
                ));
            }
            BinaryOperator::Equal
            | BinaryOperator::Greater
            | BinaryOperator::GreaterEq
            | BinaryOperator::Less
            | BinaryOperator::LessEq
            | BinaryOperator::NotEqual => {
                let newval = Self::perform_comparison(operator, lhs, rhs);
                result.ok((
                    Expression {
                        expr: LogicalExpression::NumberLiteral(NumberLiteral {
                            value: if newval { 1 } else { 0 },
                            ty: Type::Bits { size: 1 },
                        }),
                        context: self.context.clone(),
                    },
                    vec![(Type::Bits { size: 1 }, self.context.clone())],
                ))
            }
            BinaryOperator::Concatenate => {
                todo!("Throw an error that concatenating two number literals is not allowed");
            }
        }
        result
    }

    /// This is a quick hack to work around Bit != Bits<1>
    fn debit(ty: Type) -> Type {
        match ty {
            Type::Bit => Type::Bits { size: 1 },
            x => x,
        }
    }

    fn get_addlike_binary_op_type(
        &self,
        variables: &HashMap<VariablePath, Type>,
        operator: BinaryOperator,
        lhs: Expression,
        lhs_type: Type,
        rhs: Expression,
        rhs_type: Type,
    ) -> CompileResult<(Expression, Vec<(Type, StringContext)>)> {
        let mut result = CompileResult::new();
        let lhs_type = Self::debit(lhs_type);
        let rhs_type = Self::debit(rhs_type);
        match (&lhs_type, &rhs_type) {
            (Type::Bits { size: lhs_size }, Type::Bits { size: rhs_size }) => {
                // Both sides are bits: They must be the same size
                if lhs_size == rhs_size {
                    result.ok((self.clone(), vec![(lhs_type.clone(), self.context.clone())]));
                } else {
                    result.error(CompileError::MismatchedTypes {
                        context: self.context.clone(),
                        current_type: lhs_type.clone(),
                        needed_type: rhs_type.clone(),
                    });
                }
            }
            (Type::Bits { size: typed_size }, Type::Literal { minimum_size })
            | (Type::Literal { minimum_size }, Type::Bits { size: typed_size }) => {
                // One side is a literal: The typed size must be sufficiently large
                if *typed_size >= *minimum_size {
                    result.ok((
                        self.clone(),
                        vec![(Type::Bits { size: *typed_size }, self.context.clone())],
                    ));
                } else {
                    result.error(CompileError::LiteralTooBig {
                        context: self.context.clone(),
                        typed_size: *typed_size,
                        literal_size: *minimum_size,
                    });
                }
            }
            (
                Type::Literal {
                    minimum_size: lhs_minimum_size,
                },
                Type::Literal {
                    minimum_size: rhs_minimum_size,
                },
            ) => {
                // Both sides are literals: Make the resulting type "sufficiently large"
                let lhs = match &lhs.expr {
                    LogicalExpression::NumberLiteral(lit) => lit,
                    _ => panic!("The type is literal, but the value is not?"),
                };
                let rhs = match &rhs.expr {
                    LogicalExpression::NumberLiteral(lit) => lit,
                    _ => panic!("The type is literal, but the value is not?"),
                };
                result = self.combine_literals(
                    operator,
                    lhs.value,
                    *lhs_minimum_size,
                    rhs.value,
                    *rhs_minimum_size,
                );
            }
            _ => {
                result.error(CompileError::MismatchedTypes {
                    context: self.context.clone(),
                    current_type: lhs_type.clone(),
                    needed_type: rhs_type.clone(),
                });
            }
        }
        result
    }

    fn get_comparisonlike_binary_op_type(
        &self,
        variables: &HashMap<VariablePath, Type>,
        operator: BinaryOperator,
        lhs: Expression,
        lhs_type: Type,
        rhs: Expression,
        rhs_type: Type,
    ) -> CompileResult<(Expression, Vec<(Type, StringContext)>)> {
        let mut result = CompileResult::new();
        let lhs_type = Self::debit(lhs_type);
        let rhs_type = Self::debit(rhs_type);
        match (&lhs_type, &rhs_type) {
            (Type::Bits { size: lhs_size }, Type::Bits { size: rhs_size }) => {
                // Both sides are bits: They must be the same size
                if lhs_size == rhs_size {
                    result.ok((
                        self.clone(),
                        vec![(Type::Bits { size: 1 }, self.context.clone())],
                    ));
                } else {
                    result.error(CompileError::MismatchedTypes {
                        context: self.context.clone(),
                        current_type: lhs_type.clone(),
                        needed_type: rhs_type.clone(),
                    });
                }
            }
            (Type::Bits { size: typed_size }, Type::Literal { minimum_size })
            | (Type::Literal { minimum_size }, Type::Bits { size: typed_size }) => {
                // One side is a literal: The typed size must be sufficiently large
                if *typed_size >= *minimum_size {
                    result.ok((
                        self.clone(),
                        vec![(Type::Bits { size: 1 }, self.context.clone())],
                    ));
                } else {
                    result.error(CompileError::LiteralTooBig {
                        context: self.context.clone(),
                        typed_size: *typed_size,
                        literal_size: *minimum_size,
                    });
                }
            }
            (
                Type::Literal {
                    minimum_size: lhs_minimum_size,
                },
                Type::Literal {
                    minimum_size: rhs_minimum_size,
                },
            ) => {
                // Both sides are literals: Make the resulting type "sufficiently large"
                let lhs = match &lhs.expr {
                    LogicalExpression::NumberLiteral(lit) => lit,
                    _ => panic!("The type is literal, but the value is not?"),
                };
                let rhs = match &rhs.expr {
                    LogicalExpression::NumberLiteral(lit) => lit,
                    _ => panic!("The type is literal, but the value is not?"),
                };
                result = self.combine_literals(
                    operator,
                    lhs.value,
                    *lhs_minimum_size,
                    rhs.value,
                    *rhs_minimum_size,
                );
            }
            _ => {
                result.error(CompileError::MismatchedTypes {
                    context: self.context.clone(),
                    current_type: lhs_type.clone(),
                    needed_type: rhs_type.clone(),
                });
            }
        }
        result
    }

    fn get_concatlike_binary_op_type(
        &self,
        variables: &HashMap<VariablePath, Type>,
        operator: BinaryOperator,
        lhs: Expression,
        lhs_type: Type,
        rhs: Expression,
        rhs_type: Type,
    ) -> CompileResult<(Expression, Vec<(Type, StringContext)>)> {
        let mut result = CompileResult::new();
        let lhs_type = Self::debit(lhs_type);
        let rhs_type = Self::debit(rhs_type);
        match (&lhs_type, &rhs_type) {
            (Type::Bits { size: lhs_size }, Type::Bits { size: rhs_size }) => {
                // Both sides are bits: The resulting type is the sum
                result.ok((
                    self.clone(),
                    vec![(
                        Type::Bits {
                            size: lhs_size + rhs_size,
                        },
                        self.context.clone(),
                    )],
                ));
            }
            _ => {
                // Both sides must be bits, anything else is an error
                todo!("Both sides of a concat operator must be bits<N>, no literals permitted");
            }
        }
        result
    }

    fn get_literal_value(&self) -> u128 {
        match &self.expr {
            LogicalExpression::NumberLiteral(lit) => lit.value,
            _ => {
                panic!("Can't call get_literal_value on non-literal");
            }
        }
    }

    /// This function both gets the expression's type and performs typechecking
    ///
    /// Note that it returns a vec - in some situations, like conditionals, the
    /// conditional may not have a single known type (since we don't know what we're
    /// assigning to). For this, we return all the types from both branches, and let
    /// the block code handle generating errors.
    ///
    /// If type checking passes, then this will return a single element.
    pub fn get_type(
        &self,
        variables: &HashMap<VariablePath, Type>,
    ) -> CompileResult<(Expression, Vec<(Type, StringContext)>)> {
        let mut result = CompileResult::new();

        match &self.expr {
            LogicalExpression::NumberLiteral(NumberLiteral { ty, .. }) => {
                result.ok((self.clone(), vec![(ty.clone(), self.context.clone())]));
            }
            LogicalExpression::VariableReference(VariableReference { variable, .. }) => {
                if let Some(ty) = variables.get(variable) {
                    result.ok((
                        self.clone(),
                        vec![(Self::debit(ty.clone()), self.context.clone())],
                    ));
                } else {
                    result.error(CompileError::UndeclaredVariable {
                        context: self.context.clone(),
                    });
                }
            }
            LogicalExpression::BinaryOperation {
                operation,
                lhs,
                rhs,
            } => {
                // For binary operations, there are generally three cases:
                // - Both sides are Bits
                //     In this case, both sides must be the same type.
                // - Both sides are Literal
                //     In this case, the resulting type is adjusted to fit the value.
                // - One side is literal, other is bits
                //     In this case, the bits type must be bigger than minimum size

                let (lhs, lhs_type) = logerror!(result, lhs.get_type(variables));
                let (rhs, rhs_type) = logerror!(result, rhs.get_type(variables));

                let lhs_type = Self::debit(lhs_type[0].0.clone());
                let rhs_type = Self::debit(rhs_type[0].0.clone());

                match operation {
                    BinaryOperator::Addition
                    | BinaryOperator::Subtraction
                    | BinaryOperator::BitwiseAnd
                    | BinaryOperator::BitwiseOr
                    | BinaryOperator::BitwiseXor => {
                        result = self.get_addlike_binary_op_type(
                            variables,
                            *operation,
                            lhs,
                            lhs_type.clone(),
                            rhs,
                            rhs_type.clone(),
                        );
                    }
                    BinaryOperator::Equal
                    | BinaryOperator::Greater
                    | BinaryOperator::GreaterEq
                    | BinaryOperator::Less
                    | BinaryOperator::LessEq
                    | BinaryOperator::NotEqual => {
                        result = self.get_comparisonlike_binary_op_type(
                            variables,
                            *operation,
                            lhs,
                            lhs_type.clone(),
                            rhs,
                            rhs_type.clone(),
                        );
                    }
                    BinaryOperator::Concatenate => {
                        result = self.get_concatlike_binary_op_type(
                            variables,
                            *operation,
                            lhs,
                            lhs_type.clone(),
                            rhs,
                            rhs_type.clone(),
                        );
                    }
                }
            }
            LogicalExpression::UnaryOperation {
                operation,
                operatee,
            } => {
                // For now, unary operations are simple: The operand must be a bits type
                let (operand, operand_type) = logerror!(result, operatee.get_type(variables));
                let operand_type = Self::debit(operand_type[0].0.clone());

                match operand_type {
                    Type::Bits { size } => {
                        match operation {
                            UnaryOperator::Negation => {
                                result.ok((
                                    self.clone(),
                                    vec![(operand_type.clone(), self.context.clone())],
                                ));
                            }
                            UnaryOperator::Index(idx) => {
                                if idx.high > size as u32 {
                                    todo!("Throw an error: You can't index a variable that's too small");
                                } else {
                                    result.ok((
                                        self.clone(),
                                        vec![(
                                            Type::Bits {
                                                size: (idx.high - idx.low + 1) as usize,
                                            },
                                            self.context.clone(),
                                        )],
                                    ));
                                }
                            }
                        }
                    }
                    _ => {
                        result.error(CompileError::MismatchedTypes {
                            context: self.context.clone(),
                            current_type: operand_type.clone(),
                            // TODO: This is wrong, we need to make a specific error message for this case (unary ops require bits<N> type)
                            needed_type: Type::Bits { size: 1 },
                        });
                    }
                }
            }
            LogicalExpression::Ternary {
                condition,
                lhs,
                rhs,
            } => {
                // First, check the condition
                let (condition_expr, condition_type) =
                    logerror!(result, condition.get_type(variables));
                assert!(
                    condition_type.len() == 1,
                    "It's unclear what could cause this condition"
                );
                match &condition_type[0].0 {
                    Type::Bit | Type::Bits { size: 1 } => {
                        // We're good
                    }
                    _ => {
                        result.error(CompileError::MismatchedTypes {
                            context: condition_expr.context.clone(),
                            current_type: condition_type[0].0.clone(),
                            needed_type: Type::Bits { size: 1 },
                        });
                    }
                }

                // Check each branch, they must be the same or coercable to the same
                let (lhs, lhs_type) = logerror!(result, lhs.get_type(variables));
                let (rhs, rhs_type) = logerror!(result, rhs.get_type(variables));

                if lhs_type.len() > 1 || rhs_type.len() > 1 {
                    todo!("Need to handle conditionals whose branches fail");
                }

                let lhs_type = Self::debit(lhs_type[0].0.clone());
                let rhs_type = Self::debit(rhs_type[0].0.clone());

                match (&lhs_type, &rhs_type) {
                    (Type::Bits { size: lhs_size }, Type::Bits { size: rhs_size }) => {
                        if *lhs_size == *rhs_size {
                            result
                                .ok((self.clone(), vec![(lhs_type.clone(), self.context.clone())]));
                        } else {
                            result.ok((
                                self.clone(),
                                vec![
                                    (lhs_type.clone(), lhs.context.clone()),
                                    (rhs_type.clone(), rhs.context.clone()),
                                ],
                            ));
                        }
                    }
                    (Type::Bits { size: lhs_size }, Type::Literal { minimum_size }) => {
                        if minimum_size <= lhs_size {
                            // Rewrite the smaller number literal
                            result.ok((
                                Expression {
                                    expr: LogicalExpression::Ternary {
                                        condition: condition.clone(),
                                        lhs: Box::new(lhs.clone()),
                                        rhs: Box::new(Expression {
                                            expr: LogicalExpression::NumberLiteral(NumberLiteral {
                                                ty: Type::Bits { size: *lhs_size },
                                                value: rhs.get_literal_value(),
                                            }),
                                            context: rhs.context.clone(),
                                        }),
                                    },
                                    context: self.context.clone(),
                                },
                                vec![(Type::Bits { size: *lhs_size }, self.context.clone())],
                            ));
                        } else {
                            result.ok((
                                self.clone(),
                                vec![
                                    (lhs_type.clone(), lhs.context.clone()),
                                    (rhs_type.clone(), rhs.context.clone()),
                                ],
                            ));
                        }
                    }
                    (Type::Literal { minimum_size }, Type::Bits { size: rhs_size }) => {
                        if minimum_size <= rhs_size {
                            // Rewrite the smaller number literal
                            result.ok((
                                Expression {
                                    expr: LogicalExpression::Ternary {
                                        condition: condition.clone(),
                                        lhs: Box::new(Expression {
                                            expr: LogicalExpression::NumberLiteral(NumberLiteral {
                                                ty: Type::Bits { size: *rhs_size },
                                                value: lhs.get_literal_value(),
                                            }),
                                            context: lhs.context.clone(),
                                        }),
                                        rhs: Box::new(rhs.clone()),
                                    },
                                    context: self.context.clone(),
                                },
                                vec![(Type::Bits { size: *rhs_size }, self.context.clone())],
                            ));
                        } else {
                            result.ok((
                                self.clone(),
                                vec![
                                    (lhs_type.clone(), lhs.context.clone()),
                                    (rhs_type.clone(), rhs.context.clone()),
                                ],
                            ));
                        }
                    }
                    (
                        Type::Literal {
                            minimum_size: lhs_minimum_size,
                        },
                        Type::Literal {
                            minimum_size: rhs_minimum_size,
                        },
                    ) => {
                        if lhs_minimum_size < rhs_minimum_size {
                            result.ok((
                                Expression {
                                    expr: LogicalExpression::Ternary {
                                        condition: condition.clone(),
                                        lhs: Box::new(Expression {
                                            expr: LogicalExpression::NumberLiteral(NumberLiteral {
                                                ty: Type::Bits {
                                                    size: *rhs_minimum_size,
                                                },
                                                value: lhs.get_literal_value(),
                                            }),
                                            context: lhs.context.clone(),
                                        }),
                                        rhs: Box::new(Expression {
                                            expr: LogicalExpression::NumberLiteral(NumberLiteral {
                                                ty: Type::Bits {
                                                    size: *rhs_minimum_size,
                                                },
                                                value: rhs.get_literal_value(),
                                            }),
                                            context: rhs.context.clone(),
                                        }),
                                    },
                                    context: self.context.clone(),
                                },
                                vec![(
                                    Type::Bits {
                                        size: *rhs_minimum_size,
                                    },
                                    self.context.clone(),
                                )],
                            ));
                        } else {
                            result.ok((
                                Expression {
                                    expr: LogicalExpression::Ternary {
                                        condition: condition.clone(),
                                        lhs: Box::new(Expression {
                                            expr: LogicalExpression::NumberLiteral(NumberLiteral {
                                                ty: Type::Bits {
                                                    size: *lhs_minimum_size,
                                                },
                                                value: lhs.get_literal_value(),
                                            }),
                                            context: lhs.context.clone(),
                                        }),
                                        rhs: Box::new(Expression {
                                            expr: LogicalExpression::NumberLiteral(NumberLiteral {
                                                ty: Type::Bits {
                                                    size: *lhs_minimum_size,
                                                },
                                                value: rhs.get_literal_value(),
                                            }),
                                            context: rhs.context.clone(),
                                        }),
                                    },
                                    context: self.context.clone(),
                                },
                                vec![(
                                    Type::Bits {
                                        size: *lhs_minimum_size,
                                    },
                                    self.context.clone(),
                                )],
                            ));
                        }
                    }
                    _ => {
                        panic!("Unexpected types");
                    }
                }
            }
        }
        result
    }
}

#[cfg(test)]
mod typecheck_test {
    use super::*;
    use std::collections::HashMap;

    fn typedb() -> HashMap<VariablePath, Type> {
        let mut db = HashMap::new();
        db.insert(VariablePath::from_root("foo_u17"), Type::Bits { size: 17 });
        db.insert(
            VariablePath::from_root("condition_u1"),
            Type::Bits { size: 1 },
        );
        db
    }

    fn mk_context() -> StringContext {
        StringContext {
            line: 1,
            col: 1,
            line_str: "potato".to_owned(),
            node_str: "potato".to_owned(),
        }
    }

    fn mk_lit(lit: NumberLiteral) -> Expression {
        Expression {
            expr: LogicalExpression::NumberLiteral(lit),
            context: mk_context(),
        }
    }

    fn mk_varref(var: VariablePath) -> Expression {
        Expression {
            expr: LogicalExpression::VariableReference(VariableReference {
                variable: var,
                time: TimeReference::Relative(0),
            }),
            context: mk_context(),
        }
    }

    fn mk_binop(op: BinaryOperator, lhs: Expression, rhs: Expression) -> Expression {
        Expression {
            expr: LogicalExpression::BinaryOperation {
                operation: op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            context: mk_context(),
        }
    }

    fn mk_unop(op: UnaryOperator, operand: Expression) -> Expression {
        Expression {
            expr: LogicalExpression::UnaryOperation {
                operation: op,
                operatee: Box::new(operand),
            },
            context: mk_context(),
        }
    }

    fn mk_ternary(condition: Expression, iftrue: Expression, iffalse: Expression) -> Expression {
        Expression {
            expr: LogicalExpression::Ternary {
                condition: Box::new(condition),
                lhs: Box::new(iftrue),
                rhs: Box::new(iffalse),
            },
            context: mk_context(),
        }
    }

    fn singletype(mut ts: Vec<(Type, StringContext)>) -> Type {
        assert_eq!(ts.len(), 1);
        ts[0].0.clone()
    }

    #[test]
    fn typecheck_literal() {
        let expr = mk_lit(NumberLiteral {
            ty: Type::Literal { minimum_size: 15 },
            value: 5,
        });
        let (e, ts) = expr.get_type(&HashMap::new()).unwrap();
        assert_eq!(singletype(ts), Type::Literal { minimum_size: 15 });

        let expr = mk_lit(NumberLiteral {
            ty: Type::Literal { minimum_size: 17 },
            value: 6,
        });
        let (e, ts) = expr.get_type(&HashMap::new()).unwrap();
        assert_eq!(singletype(ts), Type::Literal { minimum_size: 17 });
    }

    #[test]
    fn typecheck_variable() {
        let expr = mk_varref(VariablePath::from_root("foo_u17"));
        let (e, ts) = expr.get_type(&typedb()).unwrap();
        assert_eq!(singletype(ts), Type::Bits { size: 17 });
    }

    #[test]
    fn typecheck_add_variables() {
        let expr = mk_binop(
            BinaryOperator::Addition,
            mk_varref(VariablePath::from_root("foo_u17")),
            mk_varref(VariablePath::from_root("foo_u17")),
        );
        let (e, ts) = expr.get_type(&typedb()).unwrap();
        assert_eq!(singletype(ts), Type::Bits { size: 17 });
    }

    #[test]
    fn typecheck_add_variable_lit() {
        // If the variable is bigger, that's fine
        let expr = mk_binop(
            BinaryOperator::Addition,
            mk_varref(VariablePath::from_root("foo_u17")),
            mk_lit(NumberLiteral {
                ty: Type::Literal { minimum_size: 5 },
                value: 19,
            }),
        );
        let (e, ts) = expr.get_type(&typedb()).unwrap();
        assert_eq!(singletype(ts), Type::Bits { size: 17 });

        // If the literal is bigger, that's not acceptable
        let expr = mk_binop(
            BinaryOperator::Addition,
            mk_varref(VariablePath::from_root("foo_u17")),
            mk_lit(NumberLiteral {
                ty: Type::Literal { minimum_size: 100 },
                value: 19,
            }),
        );
        assert_eq!(expr.get_type(&typedb()).errors.len(), 1);
    }

    #[test]
    fn typecheck_add_lit_lit() {
        let expr = mk_binop(
            BinaryOperator::Addition,
            mk_lit(NumberLiteral {
                ty: Type::Literal { minimum_size: 1 },
                value: 1,
            }),
            mk_lit(NumberLiteral {
                ty: Type::Literal { minimum_size: 15 },
                value: 32767,
            }),
        );
        let (e, ts) = expr.get_type(&typedb()).unwrap();
        assert_eq!(singletype(ts), Type::Literal { minimum_size: 16 });
    }

    #[test]
    fn typecheck_unary_op() {
        let expr = mk_unop(
            UnaryOperator::Negation,
            mk_varref(VariablePath::from_root("foo_u17")),
        );
        let (e, ts) = expr.get_type(&typedb()).unwrap();
        assert_eq!(singletype(ts), Type::Bits { size: 17 });
    }

    #[test]
    fn typecheck_ternary() {
        let expr = mk_ternary(
            mk_varref(VariablePath::from_root("condition_u1")),
            mk_varref(VariablePath::from_root("foo_u17")),
            mk_lit(NumberLiteral {
                ty: Type::Literal { minimum_size: 17 },
                value: 5,
            }),
        );
        let (_, ts) = expr.get_type(&typedb()).unwrap();
        assert_eq!(singletype(ts), Type::Bits { size: 17 });

        // Error if the condition isn't bits<1>
        let expr = mk_ternary(
            mk_varref(VariablePath::from_root("foo_u17")),
            mk_varref(VariablePath::from_root("foo_u17")),
            mk_varref(VariablePath::from_root("foo_u17")),
        );
        let res = expr.get_type(&typedb());
        assert_eq!(res.errors.len(), 1);

        // If the branches don't agree, return both types
        let expr = mk_ternary(
            mk_varref(VariablePath::from_root("condition_u1")),
            mk_varref(VariablePath::from_root("condition_u1")),
            mk_varref(VariablePath::from_root("foo_u17")),
        );
        let (_, ts) = expr.get_type(&typedb()).unwrap();
        assert_eq!(ts.len(), 2);
        assert_eq!(ts[0].0, Type::Bits { size: 1 });
        assert_eq!(ts[1].0, Type::Bits { size: 17 });

        // If one of the branches is a compatible literal, coerce it
        let expr = mk_ternary(
            mk_varref(VariablePath::from_root("condition_u1")),
            mk_varref(VariablePath::from_root("foo_u17")),
            mk_lit(NumberLiteral {
                ty: Type::Literal { minimum_size: 5 },
                value: 3,
            }),
        );
        let (_, ts) = expr.get_type(&typedb()).unwrap();
        assert_eq!(singletype(ts), Type::Bits { size: 17 });

        // If one of the branches is a bigger literal, return both
        let expr = mk_ternary(
            mk_varref(VariablePath::from_root("condition_u1")),
            mk_varref(VariablePath::from_root("foo_u17")),
            mk_lit(NumberLiteral {
                ty: Type::Literal { minimum_size: 100 },
                value: 314159265,
            }),
        );
        let (_, ts) = expr.get_type(&typedb()).unwrap();
        assert_eq!(ts.len(), 2);
        assert_eq!(ts[0].0, Type::Bits { size: 17 });
        assert_eq!(ts[1].0, Type::Literal { minimum_size: 100 });
    }
}

#[derive(Debug)]
pub struct Block {
    pub assignments: HashMap<VariablePath, Box<Expression>>,
}

impl Block {
    fn from_ast(
        ast: &Tree<ASTNode>,
        children: &[NodeId],
        declaration_allowed: bool,
    ) -> CompileResult<Self> {
        let mut result = CompileResult::new();
        let mut assignments = logerror!(
            result,
            Self::from_ast_with_offsets(ast, children, declaration_allowed)
        );
        let (mut assignments, mut errors) = assignments
            .drain()
            .map(|(variable, (offset, mut expr))| {
                expr.shift_time(-offset);
                if expr.depends_on_future() {
                    Err(CompileError::Causality {
                        var_name: variable.display(),
                        context: expr.context.clone(),
                    })
                } else {
                    Ok((variable, expr))
                }
            })
            .partition::<Vec<Result<(_, _), CompileError>>, _>(Result::is_ok);
        errors
            .drain(..)
            .for_each(|e| result.error(e.err().unwrap()));
        let assignments: HashMap<_, _> = assignments.drain(..).map(|a| a.unwrap()).collect();
        result.ok(Self {
            assignments: assignments,
        });
        result
    }

    fn from_ast_with_offsets(
        ast: &Tree<ASTNode>,
        children: &[NodeId],
        declaration_allowed: bool,
    ) -> CompileResult<HashMap<VariablePath, (i64, Box<Expression>)>> {
        let mut result = CompileResult::new();
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
                                result.error(CompileError::MultipleAssignment {
                                    var_name: lhs.variable.display(),
                                    context: ast.get_node(*child).unwrap().data.context.clone(),
                                });
                                return result;
                            }
                            assignments.insert(lhs.variable, (offset, rhs));
                        }
                    }
                }
                ASTNodeType::Conditional => {
                    let mut new_assignments = logerror!(
                        result,
                        Self::from_conditional_children(
                            ast,
                            ast.get_node(*child).unwrap().children.as_ref(),
                        )
                    );
                    for (k, v) in new_assignments.drain() {
                        if assignments.contains_key(&k) {
                            result.error(CompileError::MultipleAssignment {
                                var_name: k.display(),
                                context: ast.get_node(*child).unwrap().data.context.clone(),
                            });
                            return result;
                        }
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
        result.ok(assignments);
        result
    }

    /// Constructs a block from the children of a conditional node
    /// Note that this function is recursive: It gets called for each else-if
    /// in the chain, and for the else itself.
    fn from_conditional_children(
        ast: &Tree<ASTNode>,
        children: &[NodeId],
    ) -> CompileResult<HashMap<VariablePath, (i64, Box<Expression>)>> {
        let mut result = CompileResult::new();
        if children.len() == 0 {
            // This must be after a else-if case, but with no following else.
            result.ok(HashMap::new());
            return result;
        }
        let okres = match &ast.get_node(children[0]).unwrap().data.node_type {
            ASTNodeType::Block => {
                // This is an unconditional else
                logerror!(
                    result,
                    Block::from_ast_with_offsets(
                        ast,
                        ast.get_node(children[0]).unwrap().children.as_ref(),
                        false,
                    )
                )
            }
            _ => {
                // This is the condition for the next branch
                let condition = Expression::from_ast(ast, children[0]);
                let mut iftrue = logerror!(
                    result,
                    Block::from_ast_with_offsets(
                        ast,
                        ast.get_node(children[1]).unwrap().children.as_ref(),
                        false,
                    )
                );
                let mut iffalse = logerror!(
                    result,
                    Block::from_conditional_children(ast, &children[2..])
                );

                //let mut assignments = iftrue.assignments;
                let mut assignments = HashMap::new();
                for (variable, value) in iffalse.drain() {
                    if iftrue.contains_key(&variable) {
                        // Build a ternary

                        let (true_offset, iftrue) = iftrue.remove(&variable).unwrap();
                        let (false_offset, iffalse) = value;

                        if true_offset != false_offset {
                            result.error(CompileError::DifferentTimeAssignsInConditional {
                                var_name: variable.display(),
                                first_assignment: iftrue.context.clone(),
                                second_assignment: iffalse.context.clone(),
                            });
                        }

                        assignments.insert(
                            variable,
                            (
                                true_offset,
                                Box::new(Expression {
                                    expr: LogicalExpression::Ternary {
                                        condition: condition.clone(),
                                        lhs: iftrue,
                                        rhs: iffalse,
                                    },
                                    context: ast
                                        .get_node(children[0])
                                        .unwrap()
                                        .data
                                        .context
                                        .clone(),
                                }),
                            ),
                        );
                    } else {
                        // Build a ternary where the if-true is the previous value
                        let (false_offset, iffalse) = value;
                        let iftrue = Box::new(Expression {
                            expr: LogicalExpression::VariableReference(VariableReference {
                                variable: variable.clone(),
                                time: TimeReference::Relative(false_offset - 1),
                            }),
                            context: ast.get_node(children[0]).unwrap().data.context.clone(),
                        });

                        assignments.insert(
                            variable,
                            (
                                false_offset,
                                Box::new(Expression {
                                    expr: LogicalExpression::Ternary {
                                        condition: condition.clone(),
                                        lhs: iftrue,
                                        rhs: iffalse,
                                    },
                                    context: ast
                                        .get_node(children[0])
                                        .unwrap()
                                        .data
                                        .context
                                        .clone(),
                                }),
                            ),
                        );
                    }
                }

                // Everything left in iftrue will not be set in the false branch
                for (variable, value) in iftrue.drain() {
                    let (true_offset, iftrue) = value;
                    let iffalse = Box::new(Expression {
                        expr: LogicalExpression::VariableReference(VariableReference {
                            variable: variable.clone(),
                            time: TimeReference::Relative(true_offset - 1),
                        }),
                        context: ast.get_node(children[0]).unwrap().data.context.clone(),
                    });

                    assignments.insert(
                        variable,
                        (
                            true_offset,
                            Box::new(Expression {
                                expr: LogicalExpression::Ternary {
                                    condition: condition.clone(),
                                    lhs: iftrue,
                                    rhs: iffalse,
                                },
                                context: ast.get_node(children[0]).unwrap().data.context.clone(),
                            }),
                        ),
                    );
                }

                assignments
            }
        };
        result.ok(okres);
        result
    }

    pub fn variable_reference_range(&self, varname: &VariablePath) -> std::ops::Range<i64> {
        let mut min = std::i64::MAX;
        let max = 0;
        for (_, v) in self.assignments.iter() {
            min = std::cmp::min(min, v.get_oldest_reference(varname).unwrap_or(0));
        }

        // TODO: Tighten this down a bit
        min..max
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ModuleDeclaration {
    pub name: String,
    pub is_extern: bool,
    pub inputs: Vec<(Type, String)>,
    pub outputs: Vec<(Type, String)>,
}

impl ModuleDeclaration {
    pub fn from_ast(
        ast: &Tree<ASTNode>,
        node: NodeId,
        types: &TypeDatabase,
    ) -> CompileResult<Self> {
        let mut result = CompileResult::new();

        let (id, is_extern, in_values, out_values) = match &ast[node].data.node_type {
            ASTNodeType::ModuleDeclaration {
                id,
                doc_comment: _,
                in_values,
                out_values,
            } => (id, false, in_values, out_values),
            ASTNodeType::ExternModuleDeclaration {
                id,
                doc_comment: _,
                in_values,
                out_values,
            } => (id, true, in_values, out_values),
            _ => {
                panic!("Tried to compile module which wasn't of type Node::ModuleDeclaration");
            }
        };

        let in_values: Vec<_> = in_values
            .iter()
            .cloned()
            .map(|(t, name)| (types.lookup(&t).unwrap(), name))
            .collect();
        let out_values: Vec<_> = out_values
            .iter()
            .cloned()
            .map(|(t, name)| (types.lookup(&t).unwrap(), name))
            .collect();

        result.ok(Self {
            name: id.to_string(),
            is_extern,
            inputs: in_values.to_owned(),
            outputs: out_values.to_owned(),
        });
        result
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
enum VariablePathSegment {
    Name(String),
}

/// Represents a path to a variable
///
/// For example, "foo" is a variable path. So is "foo.bar",
/// and "some_instance.x", and "foo.arr[5]", and "foo.arr"
///
/// Note that if "some_instance" is a module instantiation, then
/// "some_instance" itself isn't a variable path
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct VariablePath {
    segments: Vec<VariablePathSegment>,
}

impl std::fmt::Display for VariablePath {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut is_first = true;
        for segment in self.segments.iter() {
            if !is_first {
                write!(f, ".")?;
            }
            match segment {
                VariablePathSegment::Name(s) => write!(f, "{}", s)?,
            }
            is_first = false;
        }
        Ok(())
    }
}

impl VariablePath {
    pub fn from_root<T: Into<String>>(name: T) -> Self {
        Self {
            segments: vec![VariablePathSegment::Name(name.into())],
        }
    }

    pub fn from_string<T: Into<String>>(name: T) -> Self {
        let name: String = name.into();
        let segments: Vec<_> = name
            .split(".")
            .map(|name| VariablePathSegment::Name(name.to_owned()))
            .collect();
        Self { segments }
    }

    /// Create a new path where this path is "under" the other path
    pub fn put_under(&self, other: Self) -> Self {
        let segments: Vec<_> = other
            .segments
            .iter()
            .chain(self.segments.iter())
            .cloned()
            .collect();
        Self { segments }
    }

    pub fn display(&self) -> String {
        format!("{}", self)
    }

    /// Return true if this path is a subset of (or the same as) the other path
    pub fn subset_of(&self, other: &Self) -> bool {
        if self.segments.len() < other.segments.len() {
            return false;
        }
        self.segments
            .iter()
            .zip(other.segments.iter())
            .all(|(my_segment, other_segment)| my_segment == other_segment)
    }

    /// Find all the leaf paths if this variable is of the given type
    pub fn leaf_paths(&self, ty: &Type) -> Vec<(VariablePath, Type)> {
        match ty {
            Type::Bit | Type::Bits { .. } => {
                vec![(self.clone(), ty.clone())]
            }
            Type::Struct(s) => s
                .fields
                .iter()
                .flat_map(|field| {
                    let mut member_path = self.clone();
                    member_path
                        .segments
                        .push(VariablePathSegment::Name(field.name.clone()));
                    member_path.leaf_paths(&field.member_type)
                })
                .collect(),
            Type::Literal { .. } => {
                panic!("Literal types are not expressable");
            }
            Type::None => {
                panic!("This type should never exist!");
            }
        }
    }

    /// Find all child paths from this path given the type. Note the distinction from
    /// leaf_paths, which returns only leaf paths: This function returns leafs and all
    /// parents up to (and including) self.
    pub fn child_paths(&self, ty: &Type) -> Vec<(VariablePath, Type)> {
        match ty {
            Type::Bit | Type::Bits { .. } => {
                vec![(self.clone(), ty.clone())]
            }
            Type::Struct(s) => s
                .fields
                .iter()
                .flat_map(|field| {
                    let mut member_path = self.clone();
                    member_path
                        .segments
                        .push(VariablePathSegment::Name(field.name.clone()));
                    member_path.leaf_paths(&field.member_type)
                })
                .chain(std::iter::once((self.clone(), ty.clone())))
                .collect(),
            Type::Literal { .. } => {
                panic!("Literal types are not expressable");
            }
            Type::None => {
                panic!("This type should never exist!");
            }
        }
    }

    pub fn to_verilog_name(&self) -> String {
        let names: Vec<String> = self
            .segments
            .iter()
            .map(|x| match x {
                VariablePathSegment::Name(s) => s.to_owned(),
            })
            .collect();
        names.join("_")
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub doc_comment: String,
    pub inputs: Vec<String>,
    pub outputs: Vec<String>,
    pub instantiations: HashMap<String, String>,
    pub variables: HashMap<VariablePath, Type>,
    pub reset_values: HashMap<VariablePath, (NumberLiteral, i64)>,
    pub block: Block,
}

impl Module {
    pub fn from_ast(
        ast: &Tree<ASTNode>,
        head: NodeId,
        modules: &Vec<ModuleDeclaration>,
        types: &TypeDatabase,
    ) -> CompileResult<Self> {
        let mut result = CompileResult::new();

        let (id, doc_comment, in_values, out_values) = match &ast[head].data.node_type {
            ASTNodeType::ModuleDeclaration {
                id,
                doc_comment,
                in_values,
                out_values,
            } => (id, doc_comment, in_values, out_values),
            _ => {
                panic!("Tried to compile module which wasn't of type Node::ModuleDeclaration");
            }
        };

        // Get all the module instantiations
        let mut instantiations = HashMap::new();
        for (instance, module) in ast.get_node(head).unwrap().children.iter().flat_map(|n| {
            match &ast.get_node(*n).unwrap().data.node_type {
                ASTNodeType::ModuleInstantiation { module, instance } => {
                    Some((instance.to_owned(), module.to_owned()))
                }
                _ => None,
            }
        }) {
            if instantiations.contains_key(&instance) {
                result.error(CompileError::DuplicateInstantiation {
                    module_name: module.clone(),
                    instance_name: instance.clone(),
                    location: id.clone(),
                });
            }
            instantiations.insert(instance, module);
        }

        // Grab all the variable declarations
        let variables: HashMap<_, _> = ast
            .get_node(head)
            .unwrap()
            .children
            .iter()
            .flat_map(|n| match &ast.get_node(*n).unwrap().data.node_type {
                ASTNodeType::VariableDeclaration { var_type, var_id } => {
                    Some((var_id.to_string(), var_type))
                }
                _ => None,
            })
            .chain(in_values.iter().map(|(a, b)| (b.to_string(), a)))
            .chain(out_values.iter().map(|(a, b)| (b.to_string(), a)))
            .map(|(name, t)| (name, types.lookup(t).unwrap()))
            .collect();

        // Expand all the values into their "leaf" primary variables
        let variables: HashMap<VariablePath, Type> = variables
            .iter()
            .flat_map(|(varname, ty)| {
                let varname = VariablePath::from_root(varname);
                varname.child_paths(ty)
            })
            .chain(instantiations.iter().flat_map(|(instance, module)| {
                let module = match modules.iter().filter(|m| &m.name == module).next() {
                    Some(m) => m,
                    None => {
                        result.error(CompileError::UndeclaredModule {
                            module_name: module.clone(),
                            instance_name: instance.clone(),
                        });
                        return vec![];
                    }
                };
                let mut module_variables = vec![];
                for (var_type, var_name) in module.inputs.iter().chain(module.outputs.iter()) {
                    let path = VariablePath {
                        segments: vec![
                            VariablePathSegment::Name(instance.clone()),
                            VariablePathSegment::Name(var_name.clone()),
                        ],
                    };
                    for path in path.child_paths(var_type).into_iter() {
                        module_variables.push(path);
                    }
                }
                module_variables
            }))
            .collect();

        // Grab all the reset values
        let reset_values: HashMap<_, _> = ast
            .get_node(head)
            .unwrap()
            .children
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
                            match rhs.expr {
                                LogicalExpression::NumberLiteral(literal) => {
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
        let mut block = logerror!(
            result,
            Block::from_ast(ast, ast.get_node(head).unwrap().children.as_ref(), true,)
        );

        // Type-check everything
        let mut new_assignments = HashMap::new();
        for (varname, expr) in block.assignments.iter() {
            let (new_expr, expr_type) = match noncriterr!(result, expr.get_type(&variables)) {
                Some(x) => x,
                None => continue,
            };
            if expr_type.len() == 0 {
                assert!(result.errors.len() > 0);
                continue;
            }
            let var_type = match variables.get(varname) {
                Some(x) => x,
                None => {
                    result.error(CompileError::UndeclaredVariable {
                        context: new_expr.context.clone(),
                    });
                    continue;
                }
            };
            let var_type = Expression::debit(var_type.clone());
            let var_type = &var_type;
            if expr_type.len() > 1 {
                // Couldn't unify the types, emit an error for each incorrect one
                for (ty, context) in expr_type.iter() {
                    if ty != var_type {
                        result.error(CompileError::MismatchedTypes {
                            context: new_expr.context.clone(),
                            current_type: ty.clone(),
                            needed_type: var_type.clone(),
                        });
                    }
                }
            } else if &expr_type[0].0 != var_type {
                // Unification succeeded, but got the wrong value
                result.error(CompileError::MismatchedTypes {
                    context: new_expr.context.clone(),
                    current_type: expr_type[0].0.clone(),
                    needed_type: var_type.clone(),
                });
            } else {
                // Update the expr with the type-coerced one
                new_assignments.insert(varname.clone(), Box::new(new_expr));
            }
        }
        block.assignments = new_assignments;

        // Don't successfully compile if we had any errors
        if result.errors.len() > 0 {
            return result;
        }

        result.ok(Self {
            name: id.to_string(),
            doc_comment: doc_comment.to_string(),
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::types::*;
    use std::sync::Arc;

    #[test]
    fn test_path_from_str() {
        assert_eq!(VariablePath::from_string("foo").display(), "foo");
        assert_eq!(
            VariablePath::from_string("foo.bar.baz").display(),
            "foo.bar.baz"
        );
    }

    #[test]
    fn test_leaf_is_leaf() {
        let var = VariablePath::from_root("foo");
        let ty = Type::Bits { size: 16 };
        let paths = var.leaf_paths(&ty);
        assert_eq!(paths.len(), 1);
        assert_eq!(paths[0].0, var);
    }

    #[test]
    fn test_path_leafs() {
        let var = VariablePath::from_root("foo");
        let ty = Type::Struct(Arc::new(StructDefinition {
            name: "some_struct_def".to_string(),
            fields: vec![
                StructField {
                    name: "one_bit".to_string(),
                    member_type: Type::Bit,
                },
                StructField {
                    name: "some_more_bits".to_string(),
                    member_type: Type::Bits { size: 16 },
                },
                StructField {
                    name: "another_struct".to_string(),
                    member_type: Type::Struct(Arc::new(StructDefinition {
                        name: "second_struct_def".to_string(),
                        fields: vec![
                            StructField {
                                name: "a".to_string(),
                                member_type: Type::Bits { size: 5 },
                            },
                            StructField {
                                name: "b".to_string(),
                                member_type: Type::Bit,
                            },
                        ],
                    })),
                },
            ],
        }));
        let paths = var.leaf_paths(&ty);
        let paths: Vec<_> = paths.iter().map(|(x, _)| x.clone()).collect();

        assert!(paths.iter().all(|path| path.subset_of(&var)));
        assert!(paths.iter().all(|path| !var.subset_of(&path)));
        assert!(paths.iter().all(|path| path.subset_of(&path)));
    }
}
