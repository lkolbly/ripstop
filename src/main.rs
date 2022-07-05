extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod parse;

use ast::*;
use parse::parse;

fn main() {
    println!("Hello, world!");
}
