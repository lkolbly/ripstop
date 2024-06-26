use crate::ast::{ASTNode, StringContext};
use crate::tree::TreeError;
use crate::types::Type;

pub enum CompileError {
    CouldNotFindASTHead,
    TreeError {
        err: TreeError,
    },
    ParseError {
        expected: Vec<String>,
        location: StringContext, //pest::error::LineColLocation,
    },
    NumberParseError {
        error: Box<dyn std::error::Error>,
        context: StringContext,
    },
    UndeclaredVariable {
        context: StringContext,
        variable: String,
        parent_var: Option<(String, Type)>,
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
        var_name: String,
    },

    MultipleAssignment {
        var_name: String,
        context: StringContext,
    },
    DifferentTimeAssignsInConditional {
        var_name: String,
        first_assignment: StringContext,
        second_assignment: StringContext,
    },
    Causality {
        var_name: String,
        context: StringContext,
    },
    DuplicateInstantiation {
        module_name: String,
        instance_name: String,
        location: String,
    },
    UndeclaredModule {
        module_name: String,
        instance_name: String,
    },
    LiteralTooBig {
        context: StringContext,
        typed_size: usize,
        literal_size: usize,
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
            CompileError::ParseError { expected, location } => include_position(location, &format!("Parse error: Expected one of {:?}", expected)),
            CompileError::UndeclaredVariable { context, variable, parent_var } => {
                include_position(context, &format!("Undeclared variable {}", variable))?;
                if let Some((parent_var, parent_ty)) = parent_var {
                    write!(f, "\nNote: Variable {} has type {}", parent_var, parent_ty)?;
                }
                Ok(())
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
            CompileError::MultipleAssignment { var_name, context } => include_position(context, &format!("Cannot assign to variable {} multiple times", var_name)),
            CompileError::DifferentTimeAssignsInConditional { var_name, first_assignment, second_assignment } => {
                write!(f, "Variables must be assigned at the same time offset in both branches of a conditional\n")?;
                write!(f, "First assignment at {}\n", first_assignment)?;
                write!(f, "Second assignment at {}", second_assignment)
            },
            CompileError::Causality { var_name, context } => include_position(context, &format!("Expression depends on the future")),
            CompileError::DuplicateInstantiation { module_name, instance_name, location } => {
                write!(f, "Instantiated {} as {} in module {} twice", module_name, instance_name, location)
            }
            CompileError::UndeclaredModule { module_name, instance_name } => {
                write!(f, "Could not find module {} instantated as {}", module_name, instance_name)
            }
            CompileError::LiteralTooBig { context, typed_size, literal_size } => {
                include_position(context, &format!("Attempted to add a {literal_size}-bit literal to a {typed_size}-bit variable, which is not permitted"))
            }
            CompileError::NumberParseError { error, context } => {
                include_position(context, &format!("Error: {}", error))
            }
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

    pub fn unwrap(self) -> T {
        if self.errors.len() > 0 {
            panic!("CompileResult has errors: {:?}", self.errors);
        }
        self.result
            .expect("If result is None, errors must be non-empty")
    }
}

/// result should be a CompileResult
#[macro_export]
macro_rules! logerror {
    ($errors: ident, $result: expr) => {{
        let mut res = $result;
        $errors.errors.append(&mut res.errors);
        match res.result {
            Some(x) => x,
            None => {
                return $errors;
            }
        }
    }};
    ($errors: ident, $result: expr, $or_else: expr) => {
        let res = $result;
        $errors.errors.push(res.errors);
        match res {
            Some(x) => x,
            None => $or_else,
        }
    };
}

#[macro_export]
macro_rules! noncriterr {
    ($errors: ident, $result: expr) => {{
        let mut res = $result;
        $errors.errors.append(&mut res.errors);
        res.result
    }};
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
