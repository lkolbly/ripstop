pub enum VNode {
    ///Copypasta'd from ast::Node
    ModuleDeclaration {
        id: String,
        in_values: Vec<String>,
        out_values: Vec<String>,

        children: Vec<VNode>,
    },

    RegisterDeclare {
        var_id: String,
    },
    VariableReference {
        var_id: String,
    },

    ///Feel free to rename. This represents 'assign var_a = var_b'
    AssignKeyword {
        lhs: Box<VNode>,
        rhs: Box<VNode>,
    },
    ///'lhs <= rhs'
    ClockAssign {
        lhs: Box<VNode>,
        rhs: Box<VNode>,
    },
    ///Represents a statement of the form: 'always @(...) begin ... end'
    AlwaysBegin {
        children: Vec<VNode>,
    },

    //Unary ops have one child
    ///Copied from ast::Node
    BitwiseInverse {
        child: Box<VNode>,
    },

    //Binary operators have two children
    ///Copied from ast::Node
    Add {
        lhs: Box<VNode>,
        rhs: Box<VNode>,
    },

    ///Copied from ast::Node
    Subtract {
        lhs: Box<VNode>,
        rhs: Box<VNode>,
    },

    ///Copied from ast::Node
    Assign {
        lhs: Box<VNode>,
        rhs: Box<VNode>,
    },
}
