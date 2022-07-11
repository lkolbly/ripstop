use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::ast::{verify_node, Node, Type};

//Takes an input Ripsto&p AST and compiles it to a SystemVerilog string, writable to a .sv file
pub fn compile_ast(input: &Node) -> Result<String, ()> {
    //First, verify that the AST is valid
    /*if let Err(_) = verify_node(input, &HashSet::new()) {
        return Err(());
    }*/

    //Then, compile
    //Ok(compile_ast_unverified(input))
    Err(())
}

//Output format for a module:
//[module declaration] module ... (...);
//[]

//Notes: Every variable in ripstop is represented as a series of variables, one for each register needed
//For example, take the following line of code:
//my_var[t] = ~my_var[t-20];
//There are 21 registers needed to store each of the 21 states of this variable
//And they would be represented (in the compiled Verilog) as such:
//my_var_0
//my_var_1
//my_var_2
//...
//my_var_20
//And on every positive edge, they need to move up. In the final Verilog:
//always @(posedge clk) begin
//  my_var_0 <= my_var_1;
//end
//always @(posedge clk) begin
//  my_var_1 <= my_var_2;
//end
//always @(posedge clk) begin
//  my_var_19 <= my_var_20;
//end
//^^^The ripple process must be synchronous

/*fn compile_ast_unverified(input: &Node) -> String {
    //Recursively compile
    //THIS CANNOT BE FULLY DONE THIS WAY
    //This is just a temporary proof of concept
    match input {
        Node::ModuleDeclaration {
            id,
            in_values,
            out_values,
            children,
        } => {
            let mut in_values = vec![
                (Type::Bit, "clk".to_string()),
                (Type::Bit, "rst".to_string()),
            ];

            let mut module_declare: String = format!("module {id}(");

            for v in in_values {
                module_declare += format!("\n   input {},", v.1).as_str();
            }
            //An extra space between inputs and outputs
            module_declare += "\n";

            for v in out_values {
                let t = match v.0 {
                    Type::Bit => "bit",
                    _ => panic!("Cannot compile type {:?}", v.0),
                };
                module_declare += format!("\n   output {},", v.1).as_str();
            }

            //Close the module declaration and write everything else. Then, close with "endmodule"
            //Obviously this doesn't actually compile to verilog code. This is just a small test
            module_declare += "\n);";

            for c in children {
                module_declare += format!("\n   {}", compile_ast_unverified(&c)).as_str();
            }

            module_declare += "\nendmodule";

            module_declare
        }
        Node::VariableReference { var_id, t_offset } => format!("{}[t-{}]", var_id, t_offset),
        Node::BitwiseInverse { child } => format!("~{}", compile_ast_unverified(&child)),
        Node::Assign { lhs, rhs } => format!(
            "{} = {};",
            compile_ast_unverified(&lhs),
            compile_ast_unverified(&rhs)
        ),
    }
}*/
///Returns a list of all variables referenced in the input AST and the highest t-value referenced for each variable
fn get_referenced_variables_and_highest_t_offset(input: &Node) -> HashMap<String, u64> {
    //Takes a found variable reference and registers it appropriately, either adding it to the hashmap, incrementing the hashmap value, or leaving it alone
    fn register_reference(var_id: String, t_offset: u64, hm: &mut HashMap<String, u64>) {
        if let Some(current_t) = hm.get_mut(&var_id) {
            *current_t = (*current_t).max(t_offset);
        } else {
            hm.insert(var_id, t_offset);
        }
    }
    //Takes a hashmap of found variables and registers each one in the hashmap
    fn register_references(refs: &mut HashMap<String, u64>, hm: &mut HashMap<String, u64>) {
        for r in refs.drain() {
            register_reference(r.0, r.1, hm);
        }
    }
    let mut variables: HashMap<String, u64> = HashMap::new();

    //For each of these branches, register any variable references and search children recursively
    match input {
        Node::ModuleDeclaration {
            id,
            in_values,
            out_values,
            children,
        } => {
            //Register all the variables, both the inputs and outputs
            for v in in_values {
                variables.insert(v.1.clone(), 0);
            }
            for v in out_values {
                variables.insert(v.1.clone(), 0);
            }

            //Get the hashmap for each child and register the variables
            for c in children {
                let mut hm = get_referenced_variables_and_highest_t_offset(&c);
                register_references(&mut hm, &mut variables);
            }
        }
        Node::VariableReference { var_id, t_offset } => {
            register_reference(var_id.clone(), *t_offset, &mut variables);
        }
        Node::BitwiseInverse { child } => {
            let mut hm = get_referenced_variables_and_highest_t_offset(&child);
            register_references(&mut hm, &mut variables);
        }
        Node::Add { lhs, rhs } | Node::Assign { lhs, rhs } => {
            let mut lhs_hm = get_referenced_variables_and_highest_t_offset(&lhs);
            register_references(&mut lhs_hm, &mut variables);

            let mut rhs_hm = get_referenced_variables_and_highest_t_offset(&rhs);
            register_references(&mut rhs_hm, &mut variables);
        }
        _ => todo!(),
    }

    variables
}

fn compile_expression(input: &Node) -> String {
    match input {
        Node::VariableReference { var_id, t_offset } => format!("{}_{}", var_id, t_offset),
        Node::BitwiseInverse { child } => format!("~{}", compile_expression(child)),
        Node::Add { lhs, rhs } => {
            format!("{} + {}", compile_expression(lhs), compile_expression(rhs))
        }
        Node::Assign { lhs, rhs } => format!(
            "{} <= {};",
            compile_expression(lhs),
            compile_expression(rhs)
        ),
        _ => panic!("Called compile_expression on non-expression"),
    }
}

///Compiles a single module into Verilog from an AST
pub fn compile_module(input: &Node) -> String {
    if let Node::ModuleDeclaration {
        id,
        in_values,
        out_values,
        children,
    } = input
    {
        //Stores pairs of (variable ID, highest used t-offset)
        //This is needed to create the registers
        let variables: HashMap<String, u64> = get_referenced_variables_and_highest_t_offset(input);

        //rst and clk are always included as inputs
        let mut module_string = format!("module {id}(\n    input clk\n    input rst");

        //Module head, with IO declarations but no code

        for v in in_values {
            module_string += format!("\n    input {},", v.1).as_str();
        }
        //An extra space between inputs and outputs
        module_string += "\n";

        for v in out_values {
            module_string += format!("\n    output {},", v.1).as_str();
        }

        //Close the module head and write everything else. Then, close the module with "endmodule"
        module_string += "\n);\n";

        // Chaining block for all variables
        let mut chain_string = String::new();
        chain_string += "    always @(posedge clk) begin\n";

        //Register chain creation for each variable
        for v in &variables {
            //First, declare each register

            let mut declare_string = "    reg ".to_string();
            //Write all but the last declaration
            for i in 0..*v.1 {
                declare_string += format!("{}_{}, ", v.0, i).as_str();
            }
            //Then, write the last declaration to conform with Verilog syntax
            declare_string += format!("{}_{};", v.0, v.1).as_str();

            //Second, set up register chaining
            for i in (0..*v.1).rev() {
                let lhs = format!("{}_{}", v.0, i + 1);
                let rhs = format!("{}_{}", v.0, i);
                chain_string += format!("        {lhs} <= {rhs};\n").as_str();
            }

            let assign_string = format!("    assign {} = {}_{};", v.0, v.0, 0);

            module_string += format!(
                "{}\n{}\n\n",
                declare_string, assign_string
            )
            .as_str();
        }

        // Chaining is finished
        chain_string += "    end\n\n";
        module_string += chain_string.as_str();

        //User-defined logic compilation (recursive at the moment)
        for c in children {
            module_string += format!(
                "    always @(*) begin\n        {}\n    end",
                compile_expression(c)
            )
            .as_str();
        }

        //Thirdly, close the declaration with "endmodule"
        module_string += "\nendmodule";

        return module_string;
    } else {
        panic!("Tried to compile module which wasn't of type Node::ModuleDeclaration");
    }
}
