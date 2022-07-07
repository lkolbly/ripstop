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
    
    let a = parse("module hello() -> (bit led) {
        led[t] = ~led[t-1]; // Perform the bitwise NOT
    }");
    println!("{:#?}", a);

    //println!("{:?}", verify_node(&a, &HashSet::new()));
    //println!("{}", compile_ast(&a).unwrap());
    println!("{}", compile_module(&a));

    let b = parse("module add(bit b, bit c) -> (bit a) {
        a[t] = a[t-1] + b[t] + c[t-1];
    }");
    println!("{:#?}", b);

    println!("{}", compile_module(&b));
}
