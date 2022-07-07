extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod compile;
mod parse;

use std::collections::HashSet;

use ast::*;
use compile::*;
use parse::parse;

fn main() {
    println!("Hello, world!");
    let a = Node::ModuleDeclaration {
        id: "hello".to_string(),
        in_values: vec![],
        out_values: vec![(Type::Bit, "led".to_string())],
        children: vec![Node::Assign {
            lhs: Box::new(Node::VariableReference {
                var_id: "led".to_string(),
                t_offset: 0,
            }),
            rhs: Box::new(Node::BitwiseInverse {
                child: Box::new(Node::VariableReference {
                    var_id: "led".to_string(),
                    t_offset: 1,
                }),
            }),
        }],
    };

    //println!("{:?}", verify_node(&a, &HashSet::new()));
    //println!("{}", compile_ast(&a).unwrap());
    println!("{}", compile_module(&a));

    let b = Node::ModuleDeclaration {
        id: "add".to_string(),
        in_values: vec![(Type::Bit, "b".to_string()), (Type::Bit, "c".to_string())],
        out_values: vec![(Type::Bit, "a".to_string())],
        children: vec![Node::Assign {
            lhs: Box::new(Node::VariableReference {
                var_id: "a".to_string(),
                t_offset: 0,
            }),
            rhs: Box::new(Node::Add {
                lhs: Box::new(Node::VariableReference {
                    var_id: "a".to_string(),
                    t_offset: 1,
                }),
                rhs: Box::new(Node::Add {
                    lhs: Box::new(Node::VariableReference {
                        var_id: "b".to_string(),
                        t_offset: 0,
                    }),
                    rhs: Box::new(Node::VariableReference {
                        var_id: "c".to_string(),
                        t_offset: 1,
                    }),
                }),
            }),
        }],
    };

    println!("{}", compile_module(&b));
}
