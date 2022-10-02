use std::fmt;

use crate::tree::{NodeId, Tree};

#[derive(Debug, Clone)]
pub enum AlwaysBeginTriggerType {
    ///*
    // OnDepencyUpdate,
    ///posedge clk
    Posedge,
}

impl fmt::Display for AlwaysBeginTriggerType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            // AlwaysBeginTriggerType::OnDepencyUpdate => "*",
            AlwaysBeginTriggerType::Posedge => "posedge clk",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
pub enum VNode {
    Document,

    ///Copypasta'd from ast::Node
    ModuleDeclaration {
        id: String,
        /// Tuples of (variable name, number of bits)
        in_values: Vec<(String, usize)>,
        out_values: Vec<(String, usize)>,
    },

    ModuleInstantiation {
        module: String,
        instance: String,
    },

    ModuleInstantiationVariable {
        portname: String,
        variable: String,
    },

    ///Declares each child as a register in a chain (i.e. `reg a, b, c, d[31:0];`)
    RegisterDeclare {
        bits: usize,
    },
    /// Declares each child as a wire of the given number of bits (i.e. `wire[N:0] a, b, c`)
    WireDeclare {
        bits: usize,
    },
    VariableReference {
        var_id: String,
    },

    ///Feel free to rename. This represents `assign var_a = var_b`
    AssignKeyword {},
    ///`lhs <= rhs`
    ClockAssign {},
    ///Represents a statement of the form: `always @([trigger]) begin [children] end`
    AlwaysBegin {
        trigger: AlwaysBeginTriggerType,
    },

    //Unary ops have one child
    /// Copied from ast::Node
    BitwiseInverse {},

    //Binary operators have two children
    /// Copied from ast::Node
    Add {},

    /// Copied from ast::Node
    Subtract {},

    BitwiseAnd {},
    BitwiseOr {},
    BitwiseXor {},
    Equal {},
    NotEqual {},
    Greater {},
    GreaterEq {},
    Less {},
    LessEq {},
    Concatenate {},
    BitwiseRightShift {
        amount: u32,
    },

    /// An index into a single value or range of an array. This is the parent of what it indexes
    ///
    /// `[high:low]`
    Index {
        high: usize,
        low: usize,
    },

    Ternary {},
    NumberLiteral {
        literal: crate::parse::NumberLiteral,
    },
}

pub fn verilog_ast_to_string(head: NodeId, tree: &Tree<VNode>, num_whitespace: usize) -> String {
    let next_num_whitespace = num_whitespace + 4;

    let whitespace = " ".repeat(num_whitespace);
    let next_whitespace = " ".repeat(next_num_whitespace);

    let this_node = &tree[head];
    let children = this_node.children.clone();

    //let mut iter = tree.iter_subtree(head);

    /// Gets the string that is the combination of all the children strings, each joined by a newline followed by a specified number of spaces
    fn get_children_string(
        children: Vec<NodeId>,
        tree: &Tree<VNode>,
        num_whitespace: usize,
    ) -> String {
        children
            .iter()
            .map(|n| verilog_ast_to_string(*n, tree, num_whitespace))
            .collect::<Vec<_>>()
            .join(&format!("\n{}", ""))
    }

    fn get_comma_separated_children_string(
        children: Vec<NodeId>,
        tree: &Tree<VNode>,
        num_whitespace: usize,
    ) -> String {
        children
            .iter()
            .map(|n| verilog_ast_to_string(*n, tree, num_whitespace))
            .collect::<Vec<_>>()
            .join(&format!("\n{}", ","))
    }

    let s = match &this_node.data {
        VNode::Document => {
            let children_string = get_children_string(children, tree, 0);
            children_string
        }
        VNode::ModuleDeclaration {
            id,
            in_values,
            out_values,
        } => {
            let children_string = get_children_string(children, tree, next_num_whitespace);

            //An vec of both the in and out values, each prefixed by `input ` or `output `
            let all_values: Vec<String> = {
                let in_values = in_values
                    .iter()
                    .map(|(v, nbits)| format!("{next_whitespace}input[{}:0] {v}", nbits - 1));
                let out_values = out_values
                    .iter()
                    .map(|(v, nbits)| format!("{next_whitespace}output[{}:0] {v}", nbits - 1));
                in_values.chain(out_values).collect()
            };
            let all_values_string = all_values.join(",\n");

            format!("module {id} (\n{all_values_string}\n);\n{children_string}\nendmodule")
        }
        VNode::ModuleInstantiation { module, instance } => {
            let children_string =
                get_comma_separated_children_string(children, tree, next_num_whitespace);
            format!("{whitespace}{module} {instance} (\n{children_string}\n{whitespace});")
        }
        VNode::ModuleInstantiationVariable { portname, variable } => {
            format!("{whitespace}.{portname}({variable})")
        }
        VNode::RegisterDeclare { bits } => {
            if !children.is_empty() {
                let vars = children
                    .iter()
                    .map(|n| verilog_ast_to_string(*n, tree, 0))
                    .collect::<Vec<_>>();

                format!("{whitespace}reg[{}:0] {};", bits - 1, vars.join(", "))
            } else {
                String::new()
            }
        }
        VNode::WireDeclare { bits } => {
            if !children.is_empty() {
                let vars = children
                    .iter()
                    .map(|n| verilog_ast_to_string(*n, tree, 0))
                    .collect::<Vec<_>>();

                format!("{whitespace}wire[{}:0] {};", bits - 1, vars.join(", "))
            } else {
                String::new()
            }
        }
        VNode::AlwaysBegin { trigger } => {
            let children_string = get_children_string(children, tree, next_num_whitespace);
            format!(
                "{whitespace}always @({}) begin\n{}\n{whitespace}end",
                trigger, children_string
            )
        }
        VNode::VariableReference { var_id } => var_id.to_string(),
        //For assignments and operators (and other one-line things), the children have no need for whitespace
        VNode::AssignKeyword {} => {
            format!(
                "{whitespace}assign {} = {};",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::ClockAssign {} => {
            format!(
                "{whitespace}{} <= {};",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::BitwiseInverse {} => {
            format!(
                "{whitespace}~({})",
                verilog_ast_to_string(children[0], tree, 0),
            )
        }
        VNode::Add {} => {
            format!(
                "{whitespace}(({}) + ({}))",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::Subtract {} => {
            format!(
                "{whitespace}(({}) - ({}))",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::BitwiseAnd {} => {
            format!(
                "{whitespace}(({}) & ({}))",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::BitwiseOr {} => {
            format!(
                "{whitespace}(({}) | ({}))",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::BitwiseXor {} => {
            format!(
                "{whitespace}(({}) ^ ({}))",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::Equal {} => {
            format!(
                "{whitespace}(({}) == ({}))",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::NotEqual {} => {
            format!(
                "{whitespace}(({}) != ({}))",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::Greater {} => {
            format!(
                "{whitespace}(({}) > ({}))",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::GreaterEq {} => {
            format!(
                "{whitespace}(({}) >= ({}))",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::Less {} => {
            format!(
                "{whitespace}(({}) < ({}))",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::LessEq {} => {
            format!(
                "{whitespace}(({}) <= ({}))",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::Concatenate {} => {
            format!(
                "{whitespace}{{({}), ({})}}",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::Index { high, low } => {
            let width = high - low + 1;
            let mask: u32 = (1 << width) - 1;
            format!(
                "{whitespace}((({}) >> {low}) & {width}'d{mask})",
                verilog_ast_to_string(children[0], tree, 0),
            )
        }
        VNode::BitwiseRightShift { amount } => {
            format!(
                "{whitespace}(({}) >> {amount})",
                verilog_ast_to_string(children[0], tree, 0),
            )
        }
        VNode::Ternary {} => {
            let condition = verilog_ast_to_string(children[0], tree, 0);
            let iftrue = verilog_ast_to_string(children[1], tree, 0);
            let iffalse = verilog_ast_to_string(children[2], tree, 0);
            format!("{whitespace}({condition}) ? ({iftrue}) : ({iffalse})")
        }
        VNode::NumberLiteral { literal } => {
            let size = literal.size_bits;
            let value = literal.value;
            format!("{whitespace}{size}'d{value}")
        }
    };

    s
}
