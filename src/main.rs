extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod compile;
mod parse;
mod tree;
mod verilog_ast;

use ast::*;
use compile::*;
use parse::parse;

use crate::verilog_ast::verilog_ast_to_string;

fn main() {
    let a = parse(
        "module hello() -> (bit led) {
        led[t-1] = ~led[t-2]; // Perform the bitwise NOT
    }",
    );
    println!("{:#?}\n", a);

    let v_a = compile_module(&a).unwrap();

    println!("Bare tree:\n\n{:#?}\n\n", v_a);

    println!(
        "Verilog Compiled:\n\n{}",
        verilog_ast_to_string(v_a.find_head().unwrap(), &v_a)
    )

    //println!("{:#?}\n", a);

    //verify_ast(&mut a).unwrap();

    //println!("Post-verify:\n\n{}\n\n", compile_module(&a));
    //println!("{:#?}\n", a);

    /*let b = parse(
        "module add(bit b, bit c) -> (bit a) {
        a[t] = a[t-1] + b[t] + c[t-1];
    }",
    );
    println!("{:#?}", b);

    println!("{:#?}", b);
    println!("{}", compile_module(&b));*/
}
