pub enum Node {
    ///Copypasta'd from ast::Node
    ModuleDeclaration {
        id: String,
        in_values: Vec<String>,
        out_values: Vec<String>,

        children: Vec<Node>,
    },

    RegisterDeclare {
        var_id: String,
    },
    VariableReference {
        var_id: String,
    },

    ///Feel free to rename. This represents 'assign var_a = var_b'
    AssignKeyword {
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    ///'lhs <= rhs'
    ClockAssign {
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    ///Represents a statement of the form: 'always @(...) begin ... end'
    AlwaysBegin {
        children: Vec<Node>,
    },

    //Unary ops have one child
    ///Copied from ast::Node
    BitwiseInverse {
        child: Box<Node>,
    },

    //Binary operators have two children
    ///Copied from ast::Node
    Add {
        lhs: Box<Node>,
        rhs: Box<Node>,
    },

    ///Copied from ast::Node
    Subtract {
        lhs: Box<Node>,
        rhs: Box<Node>,
    },

    ///Copied from ast::Node
    Assign {
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
}



impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        for 

        write!(f,)
    }
}
