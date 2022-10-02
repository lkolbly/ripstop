extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::collections::HashMap;

pub mod ast;
pub mod compile;
pub mod error;
pub mod ir;
pub mod parse;
pub mod simulation;
pub mod tree;
pub mod verilog_ast;

pub use crate::compile::compile_document;
