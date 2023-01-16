extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod ast;
pub mod compile;
pub mod error;
pub mod ir;
pub mod parse;
pub mod simulation;
pub mod test;
pub mod tree;
pub mod types;
pub mod verilog_ast;

pub use crate::compile::compile_document;
