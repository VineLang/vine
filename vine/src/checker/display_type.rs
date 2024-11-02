use crate::checker::{Checker, Type};

impl Checker<'_> {
  pub(super) fn display_type(&self, ty: &Type) -> String {
    match ty {
      Type::U32 => "u32".into(),
      Type::F32 => "f32".into(),
      Type::IO => "IO".into(),
      Type::Tuple(t) => {
        let mut string = "(".to_owned();
        let mut first = true;
        for t in t {
          if !first {
            string += ", ";
          }
          string += &self.display_type(t);
          first = false;
        }
        if t.len() == 1 {
          string += ",";
        }
        string + ")"
      }
      Type::Fn(args, ret) => {
        let mut string = "fn(".to_owned();
        let mut first = true;
        for t in args {
          if !first {
            string += ", ";
          }
          string += &self.display_type(t);
          first = false;
        }
        string += ")";
        if **ret != Type::UNIT {
          string += " -> ";
          string += &self.display_type(ret);
        }
        string
      }
      Type::Ref(ty) => format!("&{}", self.display_type(ty)),
      Type::Inverse(ty) => format!("~{}", self.display_type(ty)),
      Type::Adt(n, gens) => {
        let mut string = self.defs[*n].canonical.to_string();
        if !gens.is_empty() {
          string += "[";
          let mut first = true;
          for t in gens {
            if !first {
              string += ", ";
            }
            string += &self.display_type(t);
            first = false;
          }
          string += "]";
        }
        string
      }
      Type::Opaque(n) => self.generics[*n].0 .0.into(),
      Type::Var(v) => match &self.state.vars[*v] {
        Ok(t) => self.display_type(t),
        _ => format!("?{v}"),
      },
      Type::Error(_) => "??".to_string(),
    }
  }
}
