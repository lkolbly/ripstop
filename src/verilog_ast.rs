use std::fmt;

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
        children: Vec<VNode>,
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

impl std::fmt::Display for VNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //Maybe I should make a recurse function, but not at the moment
        let s = match self {
            VNode::ModuleDeclaration {
                id,
                in_values,
                out_values,
                children,
            } => todo!(),
            VNode::RegisterDeclare { vars } => format!("reg {};", vars.join(", ")),
            VNode::AlwaysBegin { children, trigger } => {
                let children_string = children
                    .iter()
                    .map(|n| n.to_string())
                    .collect::<Vec<_>>()
                    .join("\n");
                format!("always @({}) begin\n{}end", trigger, children_string)
            }
            VNode::VariableReference { var_id } => format!("{var_id}"),
            VNode::AssignKeyword { lhs, rhs } => format!("assign {lhs} = {rhs};"),
            VNode::ClockAssign { lhs, rhs } => format!("{lhs} <= {rhs};"),
            VNode::BitwiseInverse { child } => format!("~({child})"),
            VNode::Add { lhs, rhs } => format!("{lhs} + {rhs}"),
            VNode::Subtract { lhs, rhs } => format!("{lhs} - {rhs}"),
        };

        write!(f, "{}", s)
    }
}
