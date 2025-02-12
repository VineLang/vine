use std::fmt::Write;

use crate::checker::{Checker, Type};

impl<'core> Checker<'core, '_> {
  pub(super) fn display_type(&self, ty: &Type<'core>) -> String {
    let mut str = String::new();
    self._display_type(ty, &mut str);
    str
  }

  fn _display_type(&self, ty: &Type<'core>, str: &mut String) {
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
      Type::Object(e) => {
        if e.is_empty() {
          *str += "{}"
        } else {
          *str += "{ ";
          let mut first = true;
          for (v, t) in e {
            if !first {
              *str += ", ";
            }
            *str += v.0 .0;
            *str += ": ";
            self._display_type(t, str);
            first = false;
          }
          *str += " }";
        }
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
        if **ret != Type::NIL {
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
      Type::Adt(adt_id, params) => {
        *str += self.chart.adts[*adt_id].name.0 .0;
        self._display_type_params(str, params);
      }
      Type::Trait(trait_id, params) => {
        *str += self.chart.defs[self.chart.traits[*trait_id].def].name.0 .0;
        self._display_type_params(str, params);
      }
      Type::Opaque(n) => *str += self.chart.generics[self.cur_generics].type_params[*n].0 .0,
      Type::Var(v) => match &self.unifier.vars[*v].bound {
        Some((_, t)) => self._display_type(t, str),
        _ => write!(str, "?{v:?}").unwrap(),
      },
      Type::Never => *str += "!",
      Type::Error(_) => *str += "??",
      Type::Fresh(_) => unreachable!(),
    }
  }

  fn _display_type_params(&self, str: &mut String, params: &[Type<'core>]) {
    if !params.is_empty() {
      *str += "[";
      let mut first = true;
      for t in params {
        if !first {
          *str += ", ";
        }
        self._display_type(t, str);
        first = false;
      }
      *str += "]";
    }
  }
}
