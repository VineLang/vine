#![allow(dead_code)]

mod ast;
mod lexer;
mod parser;
mod visit;

fn main() {
  let src = include_str!("../test.vi");
  _ = dbg!(parser::VineParser::parse(src));
  // let mut lex = lexer::TokenKind::lexer(src);

  // while let Some(token) = lex.next() {
  //   println!("{:?} {:?} {:?}", lex.span(), token, lex.slice());
  // }
}
