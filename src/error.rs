use crate::ast::{StringContext, Type};
use std::collections::{HashMap, HashSet};

use crate::{
    ast::{ASTNode, ASTNodeType},
    ir::{BinaryOperator, Expression, Module, UnaryOperator},
    tree::{NodeId, Tree, TreeError},
    verilog_ast::{AlwaysBeginTriggerType, VNode},
};

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
    IndexOutOfBounds {
        context: StringContext,
    },
    CannotIndexType {
        context: StringContext,
        invalid_type: Type,
    },
    /// When a set of nodes have types which do not make sense in relation to eachother.
    /// For example, this error will be raised when adding a `ModuleDeclaration` to a `VariableReference` and it will contain data for those two nodes
    InvalidNodeTypes {
        nodes: Vec<ASTNode>,
    },

    InvalidResetValue {
        context: StringContext,
    },

    VariableNotAssigned {
        var_name: String
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
            CompileError::IndexOutOfBounds { context } => include_position(context, "Index out of bounds"),
            CompileError::CannotIndexType { context, invalid_type } => include_position(context, &format!("Cannot index type {}", invalid_type)),
            CompileError::InvalidResetValue { context } => include_position(context, &format!("Invalid reset value")),
            CompileError::VariableNotAssigned { var_name } => write!(f, "Variable '{}' not assigned", var_name),
        }
    }
}

pub struct CompileResult<T> {
    pub result: Option<T>,
    pub errors: Vec<CompileError>,
}

impl<T> CompileResult<T> {
    pub fn new() -> Self {
        Self {
            result: None,
            errors: vec![],
        }
    }

    pub fn error(&mut self, error: CompileError) {
        self.errors.push(error);
    }

    pub fn ok(&mut self, value: T) {
        self.result = Some(value);
    }
}

/// result should be a CompileResult
#[macro_export]
macro_rules! logerror {
    ($errors: ident, $result: expr) => {{
        $errors.errors.append(&mut $result.errors);
        match $result.result {
            Some(x) => x,
            None => {
                return $errors;
            }
        }
    }};
    ($errors: ident, $result: expr, $or_else: expr) => {
        errors.errors.push(result.errors);
        match result {
            Some(x) => x,
            None => or_else,
        }
    };
}

/// result should be a Result<T, CompileError>
#[macro_export]
macro_rules! singleerror {
    ($errors: ident, $result: expr) => {{
        match $result {
            Ok(x) => x,
            Err(e) => {
                $errors.errors.push(CompileError::from(e));
                return $errors;
            }
        }
    }};
}
