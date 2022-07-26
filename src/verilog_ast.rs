use std::fmt;

use crate::tree::{Node, NodeId, Tree};

#[derive(Debug, Clone)]
pub enum AlwaysBeginTriggerType {
    ///*
    OnDepencyUpdate,
    ///posedge clk
    Posedge,
}

impl fmt::Display for AlwaysBeginTriggerType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            AlwaysBeginTriggerType::OnDepencyUpdate => "*",
            AlwaysBeginTriggerType::Posedge => "poesdge clk",
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

pub fn verilog_ast_to_string(head: NodeId, tree: &Tree<VNode>) -> String {
    let this_node = &tree[head];
    let children = this_node.children.clone();

    //Maybe I should make a recurse function, but not at the moment
    let s = match &this_node.data {
        VNode::ModuleDeclaration {
            id,
            in_values,
            out_values,
        } => todo!(),
        VNode::RegisterDeclare { vars } => format!("reg {};", vars.join(", ")),
        VNode::AlwaysBegin { trigger } => {
            let children_string = children
                .unwrap()
                .iter()
                .map(|n| verilog_ast_to_string(*n, tree))
                .collect::<Vec<_>>()
                .join("\n");
            format!("always @({}) begin\n{}end", trigger, children_string)
        }
        VNode::VariableReference { var_id } => format!("{var_id}"),
        VNode::AssignKeyword {} => {
            let children = children.unwrap();
            format!(
                "assign {} = {};",
                verilog_ast_to_string(children[0], tree),
                verilog_ast_to_string(children[1], tree)
            )
        }
        VNode::ClockAssign {} => {
            let children = children.unwrap();
            format!(
                "{} <= {};",
                verilog_ast_to_string(children[0], tree),
                verilog_ast_to_string(children[1], tree)
            )
        }
        VNode::BitwiseInverse {} => {
            let children = children.unwrap();
            format!("~({})", verilog_ast_to_string(children[0], tree),)
        }
        VNode::Add {} => {
            let children = children.unwrap();
            format!(
                "{} + {}",
                verilog_ast_to_string(children[0], tree),
                verilog_ast_to_string(children[1], tree)
            )
        }
        VNode::Subtract {} => {
            let children = children.unwrap();
            format!(
                "{} - {}",
                verilog_ast_to_string(children[0], tree),
                verilog_ast_to_string(children[1], tree)
            )
        }
    };

    s
}
