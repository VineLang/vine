use std::{env::args, fs};

use ivm::{heap::Heap, IVM};
use ivy::parser::IvyParser;

fn main() {
  let path = args().nth(1).expect("must supply path");
  let src = fs::read_to_string(path).unwrap();
  let mut nets = IvyParser::parse(&src).unwrap();
  nets.define_ext_fns();
  let mut globals = Vec::new();
  let globals = nets.serialize(&mut globals);
  let main = &globals[nets.get_index_of("::main").expect("missing main")];
  let heap = Heap::new();
  let mut ivm = IVM::new(&heap);
  ivm.boot(main);
  ivm.normalize();
  eprintln!("{}", ivm.stats);
}
