use std::fmt::{Display, Write as _};

use crate::{
  name::{Name, NameId, Table},
  text::ast::{Expr, Net, Nets, Stmt},
};

impl Nets {
  pub fn print(&self, table: &Table) -> String {
    let mut printer = Printer::new(table);
    printer.print_nets(self);
    printer.output
  }
}

impl Net {
  pub fn print(&self, table: &Table) -> String {
    let mut printer = Printer::new(table);
    printer.print_net(self);
    printer.output
  }
}

struct Printer<'a> {
  table: &'a Table,
  indent: usize,
  output: String,
  at_start: bool,
}

impl Printer<'_> {
  fn new(table: &Table) -> Printer<'_> {
    Printer { table, indent: 0, output: String::new(), at_start: true }
  }

  fn print_nets(&mut self, nets: &Nets) {
    for (name, net) in &nets.nets {
      self.newline();
      self.print_name_id(*name);
      self.print(" ");
      self.print_net(net);
      self.newline();
    }
  }

  fn print_net(&mut self, net: &Net) {
    self.print_stmts(&net.stmts);
  }

  fn print_stmts(&mut self, stmts: &[Stmt]) {
    self.print("{");
    self.indent();
    self.newline();
    for stmt in stmts {
      self.print_stmt(stmt);
      self.newline();
    }
    self.outdent();
    self.print("}");
  }

  fn print_stmt(&mut self, (a, b): &Stmt) {
    self.print_expr(a);
    self.print(" = ");
    self.print_expr(b);
  }

  fn print_expr(&mut self, expr: &Expr) {
    match expr {
      Expr::Node(name, children) => {
        self.print_name(name);
        if !children.is_empty() {
          let multiline = children.iter().filter(|x| !is_leaf(x)).count() > 1;
          self.print("(");
          if multiline {
            self.indent();
            self.newline();
          }
          let mut start = true;
          for child in children {
            if !start {
              if multiline {
                self.newline();
              } else {
                self.print(" ");
              }
            }
            self.print_expr(child);
            start = false;
          }
          if multiline {
            self.newline();
            self.outdent();
          }
          self.print(")");
        }
      }
      Expr::Wire(wire) => self.print(wire),
      Expr::Free(None) => self.print("^"),
      Expr::Free(Some(n)) => self.print(format_args!("^{n}")),
      Expr::Interpolation(_) => unreachable!(),
      Expr::Subnet(expr, stmts) => {
        self.print_expr(expr);
        self.print_stmts(stmts);
      }
    }
  }

  fn print_name(&mut self, name: &Name) {
    self.print(self.table.path(name.path));
    if !name.payload.is_zero() {
      self.print(format_args!("#{}", name.payload));
    }
    if !name.children.is_empty() {
      let multiline =
        name.children.iter().filter(|&&n| !self.table.name(n).children.is_empty()).count() > 1;
      self.print("[");
      let mut start = true;
      if multiline {
        self.indent();
        self.newline();
      }
      for &child in &name.children {
        if !start {
          self.print(",");
          if multiline {
            self.newline();
          } else {
            self.print(" ");
          }
        }
        self.print_name_id(child);
        start = false;
      }
      if multiline {
        self.print(",");
        self.newline();
        self.outdent();
      }
      self.print("]");
    }
  }

  fn print_name_id(&mut self, id: NameId) {
    self.print_name(self.table.name(id));
  }

  fn indent(&mut self) {
    self.indent += 2;
  }

  fn outdent(&mut self) {
    self.indent -= 2;
  }

  fn newline(&mut self) {
    self.output.push('\n');
    self.at_start = true;
  }

  fn flush_indent(&mut self) {
    if self.at_start {
      for _ in 0..self.indent {
        self.output.push(' ');
      }
      self.at_start = false;
    }
  }

  fn print(&mut self, display: impl Display) {
    self.flush_indent();
    write!(&mut self.output, "{display}").unwrap();
  }
}

fn is_leaf(expr: &Expr) -> bool {
  match expr {
    Expr::Wire(_) | Expr::Free(_) | Expr::Interpolation(_) => true,
    Expr::Node(name, children) => name.children.is_empty() && children.is_empty(),
    Expr::Subnet(..) => false,
  }
}
