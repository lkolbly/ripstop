/// Intermediate Representation. This is the representation that sits between the
/// ASTNodes, which represent what was parsed, and the VNodes, which represent what
/// we're outputting.
///
/// The IR is time-aware.
use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::ast::{ASTNode, ASTNodeType, StringContext};
use crate::error::{CompileError, CompileResult};
use crate::logerror;
use crate::parse::{NumberLiteral, Range};
use crate::tree::{NodeId, Tree};
use crate::types::{Type, TypeDatabase};

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
            ASTNodeType::NumberLiteral(literal) => LogicalExpression::NumberLiteral(*literal),
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

    /// This function both gets the expression's type and performs typechecking
    pub fn get_type(&self, variables: &HashMap<String, Type>) -> CompileResult<Type> {
        let mut result = CompileResult::new();
        match &self.expr {
            LogicalExpression::BinaryOperation {
                operation,
                lhs,
                rhs,
            } => {
                match operation {
                    BinaryOperator::Addition
                    | BinaryOperator::Subtraction
                    | BinaryOperator::BitwiseAnd
                    | BinaryOperator::BitwiseOr
                    | BinaryOperator::BitwiseXor
                    | BinaryOperator::Equal
                    | BinaryOperator::NotEqual
                    | BinaryOperator::Greater
                    | BinaryOperator::GreaterEq
                    | BinaryOperator::Less
                    | BinaryOperator::LessEq => {
                        // Must be the same type
                    }
                    BinaryOperator::Concatenate => {
                        // Type is the sum of the two children types
                    }
                }
            }
            LogicalExpression::UnaryOperation {
                operation,
                operatee,
            } => {
                match operation {
                    UnaryOperator::Negation => {
                        // Same type
                    }
                    UnaryOperator::Index(range) => {
                        // The subset (...assuming the range is valid for this type)
                    }
                }
            }
            LogicalExpression::VariableReference(VariableReference { variable, .. }) => {
                //
            }
            LogicalExpression::NumberLiteral(literal) => {
                let nbits = literal.size_bits;
            }
            LogicalExpression::Ternary {
                condition,
                lhs,
                rhs,
            } => {
                let condition_type = logerror!(result, condition.get_type(variables));
                if condition_type.bit_size() != 1 {
                    result.error(CompileError::MismatchedTypes {
                        context: condition.context.clone(),
                        current_type: condition_type,
                        needed_type: Type::Bit,
                    });
                }

                // lhs and rhs must be the same type
                let lhs_type = logerror!(result, lhs.get_type(variables));
                let rhs_type = logerror!(result, rhs.get_type(variables));
                if lhs_type != rhs_type {
                    // TODO: This really isn't the right way to handle these errors. Type errors should be caught
                    // at the assignment, inside the block, since the user doesn't see it as a ternary.
                    result.error(CompileError::MismatchedTypes {
                        context: condition.context.clone(),
                        current_type: lhs_type.clone(),
                        needed_type: rhs_type,
                    });
                }

                result.ok(lhs_type);
            }
        }
        todo!();
        result
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
        println!("{:#?}", reset_values);

        // Now build the assigns themselves
        let block = logerror!(
            result,
            Block::from_ast(ast, ast.get_node(head).unwrap().children.as_ref(), true,)
        );

        // Type-check everything
        /*for (varname, expr) in block.assignments.iter() {
            let expr_type = noncriterr!(result, expr.get_type(&variables));
            let var_type = variables.get(varname).unwrap();
            if let Some(expr_type) = expr_type {
                if expr_type.bit_size() != var_type.bit_size() {
                    // Error!
                }
            }
        }*/

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
