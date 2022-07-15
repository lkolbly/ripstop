extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod compile;
mod parse;
mod verilog_ast;

use std::collections::HashSet;

use ast::*;
use compile::*;
use parse::parse;

fn main() {
    let mut a = parse(
        "module hello() -> (bit led) {
        led[t-1] = ~led[t-2]; // Perform the bitwise NOT
    }",
    );
    println!("{:#?}\n", a);

    println!("Pre-verify:\n\n{}\n\n", compile_module(&a));
    println!("{:#?}\n", a);

    verify_ast(&mut a).unwrap();

    println!("Post-verify:\n\n{}\n\n", compile_module(&a));
    println!("{:#?}\n", a);

    /*let b = parse(
        "module add(bit b, bit c) -> (bit a) {
        a[t] = a[t-1] + b[t] + c[t-1];
    }",
    );
    println!("{:#?}", b);

    println!("{:#?}", b);
    println!("{}", compile_module(&b));*/
}
