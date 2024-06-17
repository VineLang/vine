#![allow(dead_code)]

use compile::Compiler;
use parser::VineParser;

mod ast;
mod compile;
mod lexer;
mod parser;
mod visit;

fn main() {
  let src = include_str!("../test.vi");
  let ast = VineParser::parse(src).unwrap();
  let mut compiler = Compiler::default();
  for item in &ast.items {
    compiler.compile_item(item);
  }
  let nets = compiler.nets;
  println!("{nets}");
}
