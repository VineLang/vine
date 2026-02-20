use std::fmt::{Display, Write as _};

use crate::{
  name::{Name, NameId, Table},
  text::ast::{Expr, Net, Nets, Stmt},
};

pub fn print(table: &Table, nets: &Nets) -> String {
  let mut printer = Printer { table, indent: 0, output: String::new(), at_start: true };
  printer.print_nets(nets);
  printer.output
}

struct Printer<'a> {
  table: &'a Table,
  indent: usize,
  output: String,
  at_start: bool,
}

impl Printer<'_> {
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
    self.print("{");
    self.indent();
    self.newline();
    for stmt in &net.stmts {
      self.print_stmt(stmt);
      self.newline();
    }
    self.outdent();
    self.print("}");
  }

  fn print_stmt(&mut self, stmt: &Stmt) {
    match stmt {
      Stmt::Expr(expr) => {
        self.print_expr(expr);
      }
      Stmt::Link(a, b) => {
        self.print_expr(a);
        self.print(" = ");
        self.print_expr(b);
      }
    }
  }

  fn print_expr(&mut self, expr: &Expr) {
    match expr {
      Expr::Node(name, children) => {
        self.print_name(name);
        if children.iter().all(|x| !matches!(x, Some(Expr::Node(..)))) {
          self.print("(");
          let mut start = false;
          for child in children {
            if !start {
              self.print(", ");
            }
            if let Some(child) = child {
              self.print_expr(child);
            } else {
              self.print("_");
            }
            start = false;
          }
          self.print(")");
        } else {
          self.print("(");
          self.indent();
          self.newline();
          for child in children {
            if let Some(child) = child {
              self.print_expr(child);
            } else {
              self.print("_");
            }
            self.print(",");
            self.newline();
          }
          self.outdent();
          self.print(")");
        }
      }
      Expr::Wire(wire) => self.print(wire),
      Expr::Free(None) => self.print(format_args!("^")),
      Expr::Free(Some(n)) => self.print(format_args!("^{n}")),
    }
  }

  fn print_name(&mut self, name: &Name) {
    self.print(self.table.path(name.path));
    if !name.children.is_empty() {
      self.print("[");
      let mut start = false;
      for &child in &name.children {
        if !start {
          self.print(", ");
        }
        self.print_name_id(child);
        start = false;
      }
      self.print("]");
    }
    if let Some(data) = &name.data {
      self.print(format_args!("#{data}"));
    }
  }

  fn print_name_id(&mut self, id: NameId) {
    self.print_name(self.table.name(id));
  }

  fn indent(&mut self) {
    self.indent += 2;
  }

  fn outdent(&mut self) {
    self.indent += 2;
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
