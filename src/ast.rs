pub enum Type {
    Bit,
}

pub enum Node {
    //This is the head of a module. The code within a module is entirely a child of the module
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
        t_offset: u64,
    },
    //Unary operators only have one child
    //Maybe extract operators into their own enum of sorts (or maybe just unary/binary ops)? Might not be helpful though
    //Once the tree is generated, if the child has an invalid Node type, it will return an error
    BitwiseInverse {
        child: Box<Node>,
    },
    Assign {
        lhs: Box<Node>,
        rhs: Box<Node>,
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
