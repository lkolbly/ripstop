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
    ///Copypasta'd from ast::Node
    ModuleDeclaration {
        id: String,
        in_values: Vec<String>,
        out_values: Vec<String>,
    },

    RegisterDeclare {
        vars: Vec<String>,
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
    ///Copied from ast::Node
    BitwiseInverse {},

    //Binary operators have two children
    ///Copied from ast::Node
    Add {},

    ///Copied from ast::Node
    Subtract {},
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
        children: Option<Vec<NodeId>>,
        tree: &Tree<VNode>,
        num_whitespace: usize,
    ) -> String {
        children
            .unwrap()
            .iter()
            .map(|n| verilog_ast_to_string(*n, tree, num_whitespace))
            .collect::<Vec<_>>()
            .join(&format!("\n{}", ""))
    }

    /*for n in iter {
        let this_node = &tree[head];
        let children = this_node.children.clone();
    }*/

    let s = match &this_node.data {
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
                    .map(|v| format!("{next_whitespace}input {v}"));
                let out_values = out_values
                    .iter()
                    .map(|v| format!("{next_whitespace}output {v}"));
                in_values.chain(out_values).collect()
            };
            let all_values_string = all_values.join(",\n");

            format!("module {id} (\n{all_values_string}\n);\n{children_string}\nendmodule")
        }
        VNode::RegisterDeclare { vars } => format!("{whitespace}reg {};", vars.join(", ")),
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
            let children = children.unwrap();
            format!(
                "{whitespace}assign {} = {};",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::ClockAssign {} => {
            let children = children.unwrap();
            format!(
                "{whitespace}{} <= {};",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::BitwiseInverse {} => {
            let children = children.unwrap();
            format!(
                "{whitespace}~({})",
                verilog_ast_to_string(children[0], tree, 0),
            )
        }
        VNode::Add {} => {
            let children = children.unwrap();
            format!(
                "{whitespace}{} + {}",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
        VNode::Subtract {} => {
            let children = children.unwrap();
            format!(
                "{whitespace}{} - {}",
                verilog_ast_to_string(children[0], tree, 0),
                verilog_ast_to_string(children[1], tree, 0)
            )
        }
    };

    s
}
