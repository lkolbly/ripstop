use std::{
    cell::{RefCell, RefMut},
    collections::{HashMap, HashSet},
    rc::Rc,
};

#[derive(Debug, Clone)]
pub enum Type {
    Bit,
}

//Some notes about this recurse_ast method:
//1- This is not in its final form
//2- This will not normally be optimally performant
//3- The current parameters are clunky, and will be changed if a better way to do this appears (as it stands I don't understand Fn vs. FnMut enough to decide what's best)

///Recursively applies function 'f' to each Node in the AST.
/// 'f' takes a mutable reference to a node and a mutable reference of some value of type T. 'f' is called on each Node and then its children
fn recurse_ast<T, F>(node: &mut Node, f: &mut F, t: &mut T)
where
    F: FnMut(&mut Node, &mut T) + Clone,
{
    (f)(node, t);

    match node {
        Node::ModuleDeclaration { children, .. } => {
            for c in children {
                recurse_ast(c, f, t);
            }
        }
        Node::BitwiseInverse { child } => recurse_ast(child, f, t),
        Node::Add { lhs, rhs } | Node::Subtract { lhs, rhs } | Node::Assign { lhs, rhs } => {
            recurse_ast(lhs, f, t);
            recurse_ast(rhs, f, t);
        }
        //Anthing without children doesn't need to be matched against
        _ => (),
    }
}

//Examples of future VerificationError variants: TypeMismatch, InvalidModuleReference
///Describes an error encountered during verification
#[derive(Debug, Clone)]
pub enum VerificationError {
    VariableNotDeclared { var_id: String },
    VariableAlreadyDeclared { var_id: String }, //This error is specific to code without scope, which is the current system
}

//TODO: Better error system (currently panics, only slightly better than crashing)
//Perhaps create a verification error enum and return a Vec<VerificationError>

//Note: this is not optimized for performance
pub fn verify_ast(node: &mut Node) -> Result<(), Vec<VerificationError>> {
    //IMPORTANT: The default t-offset of any unused variable is 0 for now (should change later)
    //This means that a[t-10] = b[t-10] + c[t-9] won't become a[t-1] = b[t-1] + c[t]

    //A HashMap storing pairs of the variable's name and its type and lowest t-offset (var[t - n] is pos offset, var[t + n] is neg offset)
    let mut vars: HashMap<String, (Type, i64)> = HashMap::new();
    let mut errs: Vec<VerificationError> = Vec::new();

    ///Takes a node and updates (only using the one node, not recursive) the vars hashmap to see if it has up-to-date t-offset and variable reference values.
    /// Returns any errors found when verifying this node
    fn update_vars(node: &Node, vars: &mut HashMap<String, (Type, i64)>) -> Vec<VerificationError> {
        match node {
            Node::ModuleDeclaration {
                id,
                in_values,
                out_values,
                children,
            } => {
                let mut all_values = in_values.clone();
                all_values.append(&mut out_values.clone());

                for v in all_values {
                    vars.insert(v.1, (v.0, 0));
                }

                Vec::new()
            }
            Node::VariableReference { var_id, t_offset } => {
                if let Some(v) = vars.get_mut(var_id) {
                    //A higher
                    if *t_offset < v.1 {
                        v.1 = *t_offset;
                    }
                    Vec::new()
                } else {
                    vec![VerificationError::VariableNotDeclared {
                        var_id: var_id.clone(),
                    }]
                }
            }
            Node::VariableDeclaration { var_type, var_id } => {
                if let None = vars.insert(var_id.clone(), (var_type.clone(), 0)) {
                    Vec::new()
                } else {
                    vec![VerificationError::VariableAlreadyDeclared {
                        var_id: var_id.clone(),
                    }]
                }
            }

            _ => Vec::new(),
        }
    }

    //Use update_vars to call recurse_ast() and get the updated set of variable references w/ highest t-offset
    {
        //A method which uses update_vars in a way which recurse_ast can be used
        fn update_vars_as_recurse_ast_closure(
            node: &mut Node,
            vals: &mut (
                &mut HashMap<String, (Type, i64)>,
                &mut Vec<VerificationError>,
            ),
        ) {
            let mut errs = update_vars(node, vals.0);
            vals.1.append(&mut errs);
        }
        recurse_ast(
            node,
            &mut update_vars_as_recurse_ast_closure,
            &mut (&mut vars, &mut errs),
        );
    }

    //Update the t-offset of each variable reference using 'vars'

    ///Takes a Node and update its t-offset based on 'vars'. Only affects VariableReference variants and doesn't return any errors
    fn update_node(node: &mut Node, vars: &HashMap<String, (Type, i64)>) {
        match node {
            Node::VariableReference { var_id, t_offset } => {
                *t_offset -= vars
                    .get(var_id)
                    .expect(&format!(
                        "Couldn't find variable '{}' in update_node()",
                        var_id
                    ))
                    .1
            }
            _ => (),
        }
    }

    //println!("Final vars:\n{:#?}", vars);

    //Apply update_node to the entire tree
    recurse_ast(
        node,
        &mut |node: &mut Node, vars: &mut HashMap<String, (Type, i64)>| update_node(node, vars),
        &mut vars,
    );

    //At the end, return Ok(()) only if there were no errors
    //Otherwise, return all the errors
    if errs.is_empty() {
        Ok(())
    } else {
        Err(errs)
    }
}

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
