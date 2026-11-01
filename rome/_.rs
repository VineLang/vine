#!/usr/bin/env -S cargo -Zscript run -rq --manifest-path
---
[package]
name = "rome"
version = "0.1.0"
edition = "2024"

[dependencies]
ivy = { git = "https://github.com/VineLang/vine", version = "0.0.0" }
vine = { git = "https://github.com/VineLang/vine", version = "0.0.0" }
blake3 = "1.8.7"
---

use std::{
  collections::HashMap,
  env::{args, var},
  error::Error,
  fs,
  path::PathBuf,
};

use ivy::{
  engine::{Interaction, Rules, optimize},
  name::{FromTable, NameId, Table},
  net::{FlatNet, TreeNet},
  text::{
    ast::{self, Nets},
    parser::Parser,
  },
  translate::{Translator, common::replace_nilary},
};

use vine::{backend::optimizations, compiler::Guide};

fn main() -> Result<(), Box<dyn Error>> {
  let mut table = Table::default();
  let vi = &Guide::build(&mut table);

  let input = args().skip(1).map(fs::read_to_string).collect::<Result<String, _>>().unwrap();
  let pieces = pieces(&input);

  fs::create_dir_all("cache").unwrap();

  let start = pieces.partition_point(|piece| fs::exists(path(piece)).is_ok_and(|x| x));

  let mut nets = if start == 0 {
    initial_nets(vi, &mut table)
  } else {
    let nets = fs::read_to_string(path(pieces[start - 1]));
    Parser::parse(&mut table, &nets.unwrap()).unwrap().to_flat_nets().unwrap()
  };

  let debug = var("DEBUG").is_ok();
  for piece in &pieces[start..] {
    reduce(debug, vi, &mut table, &mut nets, piece);
    fs::write(path(piece), Nets::from_flat_nets(&nets).print(&table)).unwrap()
  }

  let main = table.add_path_name("rome:main");
  let mut net = TreeNet::from_flat(&nets[&main]);
  net.resolve_links();
  let output = ast::Net::from_tree(&net).print(&table);

  println!("{}", output);
  Ok(())
}

fn path(piece: &str) -> PathBuf {
  let hash = blake3::hash(piece.as_bytes());
  format!("cache/{hash}.iv").into()
}

fn initial_nets(vi: &Guide, table: &mut Table) -> HashMap<NameId, FlatNet> {
  let main = table.add_path_name("rome:main");
  let i = fs::read_to_string("_.vi.iv").unwrap();
  let mut nets = Parser::parse(table, &i).unwrap().to_flat_nets().unwrap();
  let mut net = FlatNet::default();
  let hole = net.make(vi.graft.with_children([vi.main]), []);
  let zero = net.make(vi.n32, []);
  let out = net.wire();
  net.add(vi.tuple, hole, [zero, out]);
  net.free.push(out);
  nets.insert(main, net);
  Translator::new(replace_nilary([vi.interface], vi.eraser)).translate_all(table, &mut nets);
  nets
}

fn pieces(str: &str) -> Vec<&str> {
  let mut pieces = Vec::new();
  let mut last_break = 0;
  for (i, char) in str.char_indices() {
    if char == '\n' && i > last_break + 256 {
      pieces.push(&str[..i]);
      last_break = i;
    }
  }
  pieces.push(str);
  pieces
}

fn reduce(
  debug: bool,
  vi: &Guide,
  table: &mut Table,
  nets: &mut HashMap<NameId, FlatNet>,
  input: &str,
) {
  let read_char = table.add_path_name("root:io:read_char");
  let rules = Rules::new((
    optimizations(vi, table),
    Interaction([(vi.ext, vi.n32)], |engine, _, net, ext, n| {
      if engine.name(ext).children[0] != read_char {
        return false;
      }
      let idx = engine.name(n).payload.as_u32().unwrap() as usize;
      let Some(char) = input[idx..].chars().next() else {
        return false;
      };
      let next = idx + char.len_utf8();
      if debug {
        print!("{}", &char);
      }
      let [n_, char_, next_] = engine.remove_node_n(ext);
      let [n] = engine.remove_node_n(n);
      engine.remove_wire(n, n_);
      let [char] = engine.insert_node_n(net, vi.n32.with_payload(char as u32));
      let [next] = engine.insert_node_n(net, vi.n32.with_payload(next as u32));
      engine.link(char, char_);
      engine.link(next, next_);
      true
    }),
  ));
  optimize(table, &rules, nets, &mut None, |_| true);
}
