use std::{
    cell::{RefCell, RefMut},
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::tree::Tree;

#[derive(Debug, Clone)]
pub enum Type {
    Bit,
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
pub fn verify_ast(tree: &mut Tree<Node>) -> Result<(), Vec<VerificationError>> {
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
                    vars.insert(v.1, (v.0, i64::MAX));
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
        if let Node::VariableReference { var_id, t_offset } = node {
            *t_offset -= vars
                .get(var_id)
                .unwrap_or_else(|| panic!("Couldn't find variable '{}' in update_node()", var_id))
                .1
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

#[derive(Debug, Clone)]
pub enum ASTNode {
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
        t_offset: i64,
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

//Once the tree is generated, if any child has an invalid return type, it will return an error
//For example, if the lhs and rhs of an assignment have different types, the compiler will return an error
#[derive(Debug, Clone)]
pub enum Node {
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
        t_offset: i64,
    },
    //Unary operators only have one child
    //Maybe extract operators into their own enum of sorts (or maybe just unary/binary ops)? Might not be helpful though
    BitwiseInverse {},

    //Binary operators have two children
    Add {},
    Subtract {},
    Assign {},

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
