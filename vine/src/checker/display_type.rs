use std::fmt::Write;

use crate::checker::{Checker, Type};

impl<'core> Checker<'core, '_> {
  pub(super) fn display_type(&self, ty: &Type) -> String {
    let mut str = String::new();
    self._display_type(ty, &mut str);
    str
  }

  fn _display_type(&self, ty: &Type, str: &mut String) {
    match ty {
      Type::Bool => *str += "Bool",
      Type::N32 => *str += "N32",
      Type::F32 => *str += "F32",
      Type::Char => *str += "Char",
      Type::IO => *str += "IO",
      Type::Tuple(t) => {
        *str += "(";
        let mut first = true;
        for t in t {
          if !first {
            *str += ", ";
          }
          self._display_type(t, str);
          first = false;
        }
        if t.len() == 1 {
          *str += ",";
        }
        *str += ")"
      }
      Type::Fn(args, ret) => {
        *str += "fn(";
        let mut first = true;
        for t in args {
          if !first {
            *str += ", ";
          }
          self._display_type(t, str);
          first = false;
        }
        *str += ")";
        if **ret != Type::UNIT {
          *str += " -> ";
          self._display_type(ret, str);
        }
      }
      Type::Ref(ty) => {
        *str += "&";
        self._display_type(ty, str)
      }
      Type::Inverse(ty) => {
        *str += "~";
        self._display_type(ty, str)
      }
      Type::Adt(n, gens) => {
        write!(str, "{}", self.resolver.defs[*n].canonical.segments.last().unwrap()).unwrap();
        if !gens.is_empty() {
          *str += "[";
          let mut first = true;
          for t in gens {
            if !first {
              *str += ", ";
            }
            self._display_type(t, str);
            first = false;
          }
          *str += "]";
        }
      }
      Type::Opaque(n) => *str += self.generics[*n].0 .0,
      Type::Var(v) => match &self.state.vars[*v] {
        Ok(t) => self._display_type(t, str),
        _ => write!(str, "?{v:?}").unwrap(),
      },
      Type::Never => *str += "!",
      Type::Error(_) => *str += "??",
    }
  }
}
