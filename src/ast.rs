use crate::parse::{NumberLiteral, Rule};
use pest::iterators::Pair;

#[derive(Debug, Clone)]
pub enum Type {
    Bit,
}

/// Struct for storing the context of an AST node, such as its position within the input string and the input string representing the node.
#[derive(Clone)]
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
    //This is the head of a module. The code within a module is entirely children of the module
    ModuleDeclaration {
        id: String,
        in_values: Vec<(Type, String)>,
        out_values: Vec<(Type, String)>,
    },
    //Some examples of generated VariableReferences:
    //my_var[t] => var_id: "my_var", t_offset: 0
    //my_var[t - 10] => var_id: "my_var", t_offset: 10
    //var_with_num8ers[t-2] => var_id: "var_with_num8ers", t_offset: 2
    //my_var[t + 10] =x> this doesn't work, can't reference a future clock value
    VariableReference {
        var_id: String,
    },
    TimeOffsetRelative {
        offset: i64,
    },
    TimeOffsetAbsolute {
        time: i64,
    },
    //Unary operators only have one child
    //Maybe extract operators into their own enum of sorts (or maybe just unary/binary ops)? Might not be helpful though
    BitwiseInverse,
    Add,
    Subtract,
    Assign,
    VariableDeclaration {
        var_type: Type,
        var_id: String,
    },
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
