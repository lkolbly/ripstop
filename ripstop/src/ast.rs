use crate::parse::Rule;
use pest::iterators::Pair;
use serde::{Deserialize, Serialize};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ASTType {
    pub name: String,
    pub generic_parameter: Option<usize>,
}

/// Struct for storing the context of an AST node, such as its position within the input string and the input string representing the node.
#[derive(Clone, Debug)]
pub struct StringContext {
    /// Line number of the start of this node within the input string.
    pub line: usize,

    /// Column number of the start of this node within the input string.
    pub col: usize,

    /// The section of the input string that is the line containing the start of this node.
    pub line_str: String,

    /// The section of the input string that makes up this node.
    pub node_str: String,
}

impl StringContext {
    pub fn new(pair: Pair<Rule>) -> StringContext {
        let pair_str = pair.as_str();
        let pos = match pair.clone().tokens().next().unwrap() {
            pest::Token::Start { rule: _, pos } => pos,
            _ => unreachable!(),
        };
        let (line, col) = pos.line_col();

        StringContext {
            line,
            col,
            line_str: pos.line_of().to_string(),
            node_str: pair_str.to_string(),
        }
    }

    pub fn from_location(toparse: &str, location: &pest::error::LineColLocation) -> StringContext {
        match location {
            pest::error::LineColLocation::Pos((line, col)) => {
                let line_of = toparse
                    .split('\n')
                    .nth(*line - 1)
                    .expect("Got a parse error on a line that doesn't exist!");
                StringContext {
                    line: *line,
                    col: *col,
                    line_str: line_of.to_string(),
                    node_str: "".to_string(),
                }
            }
            pest::error::LineColLocation::Span(_, _) => {
                todo!();
            }
        }
    }
}

impl Display for StringContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "line {} col {}:\n{}\n{}^",
            self.line,
            self.col,
            self.line_str.trim_end(),
            " ".repeat(self.col - 1)
        )
    }
}

#[derive(Clone)]
pub struct ASTNode {
    pub node_type: ASTNodeType,
    pub context: StringContext,
}

impl ASTNode {
    pub fn new(node_type: ASTNodeType, pair: Pair<Rule>) -> ASTNode {
        ASTNode {
            node_type,
            context: StringContext::new(pair),
        }
    }
}

impl std::fmt::Debug for ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.node_type)
    }
}

#[derive(Debug, Clone)]
pub enum ASTNodeType {
    /// This is the head of a document. It contains modules, struct definitions, constants, etc
    Document,
    //This is the head of a module. The code within a module is entirely children of the module
    ModuleDeclaration {
        id: String,
        doc_comment: String,
        in_values: Vec<(ASTType, String)>,
        out_values: Vec<(ASTType, String)>,
    },
    ExternModuleDeclaration {
        id: String,
        doc_comment: String,
        in_values: Vec<(ASTType, String)>,
        out_values: Vec<(ASTType, String)>,
    },
    ModuleInstantiation {
        module: String,
        instance: String,
    },

    /// This is a struct definition
    StructDefinition {
        name: String,
    },
    StructVariable {
        name: String,
        member_type: ASTType,
    },

    //Some examples of generated VariableReferences:
    //my_var[t] => var_id: "my_var", t_offset: 0
    //my_var[t - 10] => var_id: "my_var", t_offset: -10
    //var_with_num8ers[t-2] => var_id: "var_with_num8ers", t_offset: -2
    VariableReference {
        var_id: String,
    },
    Index {
        high: usize,
        low: usize,
    },
    TimeOffsetRelative {
        offset: i64,
    },
    TimeOffsetAbsolute {
        time: i64,
    },

    /// Conditional nodes may have many children nodes.
    /// The nodes will alternate between the condition (as an expression)
    /// and the block of statements for that condition. If there is an
    /// unconditional else, it will appear as an extra condition_block
    /// at the end.
    Conditional,

    /// Just combines multiple statements into one
    Block,

    //Unary operators only have one child
    //Maybe extract operators into their own enum of sorts (or maybe just unary/binary ops)? Might not be helpful though
    BitwiseInverse,
    Add,
    Subtract,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEq,
    LessEq,
    Concatenate,

    Assign,
    VariableDeclaration {
        var_type: ASTType,
        var_id: String,
    },
    NumberLiteral(crate::parse::NumberLiteral),
}

//This ripstop code:
/*
module hello() -> (bit led) {
    led[t] = ~led[t-1]; // Perform the bitwise NOT
}
*/

//Should become something like:
/*
ModuleDeclaration {
    id: "hello",
    in_values: [],
    out_values: [(Bit, "led")]
    children: [
        Assign {
            lhs: VariableReference {
                var_id: "led",
                t_offset: 0,
            }
            rhs: BitwiseInverse {
                child: VariableReference {
                    var_id: "led",
                    t_offset: 1,
                }
            }
        }
    ]
}
*/
