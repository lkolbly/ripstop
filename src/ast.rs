use std::collections::HashSet;

#[derive(Debug, Clone)]
pub enum Type {
    Bit,
}

//TODO: Better error system (currently panics, only slightly better than crashing)
//Perhaps create a verification error enum and return a Vec<VerificationError>

///Kind of broken because of variable scope
pub fn verify_node(node: &Node, parent_declared_vars: &HashSet<String>) -> Result<(), ()> {
    todo!();

    //All variables currently declared and in-scope
    //For a variable to be considered declared:
    //The node where it was created must be a parent of where it is accessed

    let mut scope_declared_vars: HashSet<String> = parent_declared_vars.clone();

    match node {
        Node::ModuleDeclaration {
            id,
            in_values,
            out_values,
            children,
        } => {
            //Declare input values
            for v in in_values {
                if scope_declared_vars.insert(v.1.clone()) == false {
                    panic!("Variable {} already declared in this scope!", v.1);
                }
            }
            //Declare output values
            for v in out_values {
                if scope_declared_vars.insert(v.1.clone()) == false {
                    panic!("Variable {} already declared in this scope!", v.1);
                }
            }
            //Check children for validity as well, using the updated declared vars
            for c in children {
                if let Err(_) = verify_node(c, &scope_declared_vars) {
                    return Err(());
                };
            }
        }
        Node::VariableReference { var_id, t_offset } => {
            //If the variable doesn't exist
            if let None = scope_declared_vars.get(var_id) {
                panic!("Variable {} not yet declared", var_id);
            }
        }
        Node::BitwiseInverse { child } => {
            //&Box<Node> -> Box<Node> -> Node -> &Node
            //Whether or not the type of the child is valid for a bitwise inverse.
            //This is a somewhat temporary solution, as it doesn't care about the variables' types
            let child_type_valid: bool = match &**child {
                Node::VariableReference { var_id, t_offset } => true,
                _ => false,
            };
            if child_type_valid == false {
                return Err(());
            }

            //If the child type is valid, check to make sure the child itself is valid
            if let Err(_) = verify_node(child, &scope_declared_vars) {
                return Err(());
            };
        }
        Node::Assign { lhs, rhs } => {
            //Type-verify both sides of the assignment
            //At the moment, this is kind of clunky and hard to maintain
            match &**lhs {
                Node::VariableReference {
                    var_id: _,
                    t_offset: _,
                } => (),
                _ => return Err(()),
            }

            //The right side can be anything which returns the same type as the left side
            match &**rhs {
                Node::VariableReference {
                    var_id: _,
                    t_offset: _,
                } => (),
                Node::BitwiseInverse { child } => {
                    if let Err(_) = verify_node(child, &scope_declared_vars) {
                        return Err(());
                    }
                }
                _ => return Err(()),
            }

            //Confirm each of the children now that their types are known to be correct
            if let Err(_) = verify_node(&**lhs, &scope_declared_vars) {
                return Err(());
            }
            if let Err(_) = verify_node(&**rhs, &scope_declared_vars) {
                return Err(());
            }
        }
        _ => panic!("Tried verifying unrecognized Node type"),
    }

    Ok(())
}

//Once the tree is generated, if any child has an invalid return type, it will return an error
//For example, if the lhs and rhs of an assignment have different types, the compiler will return an error
#[derive(Debug, Clone)]
pub enum Node {
    //This is the head of a module. The code within a module is entirely children of the module
    ModuleDeclaration {
        id: String,
        in_values: Vec<(Type, String)>,
        out_values: Vec<(Type, String)>,

        children: Vec<Node>,
    },
    //Some examples of generated VariableReferences:
    //my_var[t] => var_id: "my_var", t_offset: 0
    //my_var[t - 10] => var_id: "my_var", t_offset: 10
    //var_with_num8ers[t-2] => var_id: "var_with_num8ers", t_offset: 2
    //my_var[t + 10] =x> this doesn't work, can't reference a future clock value
    VariableReference {
        var_id: String,
        t_offset: i64,
    },
    //Unary operators only have one child
    //Maybe extract operators into their own enum of sorts (or maybe just unary/binary ops)? Might not be helpful though
    BitwiseInverse {
        child: Box<Node>,
    },

    //Binary operators have two children
    Add {
        lhs: Box<Node>,
        rhs: Box<Node>,
    },

    Subtract {
        lhs: Box<Node>,
        rhs: Box<Node>,
    },

    Assign {
        lhs: Box<Node>,
        rhs: Box<Node>,
    },

    VariableDeclaration {
        var_type: Type,
        var_id: String,
    },
    //TODO: Constants, [Some way of handling scope], etc.
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
