use std::{env::args, fs};

use vine::{compile::Compiler, parser::VineParser};

fn main() {
  let path = args().nth(1).expect("must supply path");
  let src = fs::read_to_string(path).unwrap();
  let ast = VineParser::parse(&src).unwrap();
  let mut compiler = Compiler::default();
  for item in &ast.items {
    compiler.compile_item(item);
  }
  let nets = compiler.nets;
  println!("{nets}");
}
