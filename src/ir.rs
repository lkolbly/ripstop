/// Intermediate Representation. This is the representation that sits between the
/// ASTNodes, which represent what was parsed, and the VNodes, which represent what
/// we're outputting.
///
/// The IR is time-aware.

use std::collections::HashMap;

use crate::ast::Type;
use crate::parse::{NumberLiteral, Range};

#[derive(Debug, Clone)]
pub enum TimeReference {
    Absolute(i64),
    Relative(i64),
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

pub struct Module {
    inputs: Vec<String>,
    outputs: Vec<String>,
    variables: HashMap<String, Type>,
    assignments: HashMap<String, Expression>,
}

pub enum Expression {
    UnaryOperation {
        operation: UnaryOperator,
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

    VariableReference {
        variable: String,
        time: TimeReference,
    },

    NumberLiteral(NumberLiteral),
}
