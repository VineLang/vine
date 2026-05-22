use std::fmt::Write;

use crate::structures::{
  chart::{Chart, FnId},
  signatures::FnSig,
  types::{ImplType, Inverted, Type, TypeKind, TypeNode, TypeState, Types},
};

impl Types {
  pub fn show(&self, chart: &Chart, ty: Type) -> String {
    show(self, chart, |s| s.show_type(ty))
  }

  pub fn show_fn_sig(&self, chart: &Chart, sig: &FnSig) -> String {
    show(self, chart, |s| s.show_fn_sig(sig))
  }

  pub fn show_impl_type(&self, chart: &Chart, impl_ty: &ImplType) -> String {
    show(self, chart, |s| s.show_impl_type(impl_ty))
  }
}

fn show(types: &Types, chart: &Chart, f: impl FnOnce(&mut ShowTypes)) -> String {
  let mut show_types = ShowTypes { types, chart, str: String::new() };
  f(&mut show_types);
  show_types.str
}

struct ShowTypes<'a> {
  types: &'a Types,
  chart: &'a Chart,
  str: String,
}

impl<'a> ShowTypes<'a> {
  fn show_type(&mut self, mut ty: Type) {
    loop {
      break match &self.types.types[ty.idx()] {
        TypeNode::Child(parent) => {
          ty = parent.invert_if(ty.inv());
          continue;
        }
        TypeNode::Root { state: TypeState::Unknown(..), .. } => {
          write!(self.str, "{}?{}", if ty.inv().0 { "~" } else { "" }, ty.idx().0).unwrap()
        }
        TypeNode::Root { state: TypeState::Known(inv, kind), .. } => {
          let mut inv = *inv ^ ty.inv();
          if inv.0 && !kind.self_dual() {
            self.str += "~";
            inv = Inverted(false);
          }
          match kind {
            TypeKind::Default => {
              self.str += "_";
            }
            TypeKind::Tuple(els) => {
              self.str += "(";
              self.show_comma_separated(els, inv);
              if els.len() == 1 {
                self.str += ",";
              }
              self.str += ")"
            }
            TypeKind::Object(entries) => {
              if entries.is_empty() {
                self.str += "{}"
              } else {
                self.str += "{ ";
                let mut first = true;
                for (key, &val) in entries {
                  if !first {
                    self.str += ", ";
                  }
                  self.str += &key.0;
                  self.str += ": ";
                  self.show_type(val.invert_if(inv));
                  first = false;
                }
                self.str += " }";
              }
            }
            TypeKind::Ref(ty) => {
              self.str += "&";
              self.show_type(*ty)
            }
            TypeKind::Key(key) => {
              self.str += ".";
              self.str += &key.0;
            }
            TypeKind::Fn(fn_id) => {
              self.str += "fn ";
              match fn_id {
                FnId::Concrete(fn_id) => {
                  self.str += &self.chart.defs[self.chart.concrete_fns[*fn_id].def].path;
                }
                FnId::Abstract(trait_id, fn_id) => {
                  self.str += &self.chart.defs[self.chart.traits[*trait_id].def].path;
                  self.str += "::";
                  self.str += &self.chart.traits[*trait_id].fns[*fn_id].name.0;
                }
              }
            }
            TypeKind::Closure(closure_id, flex, sig) => {
              write!(self.str, "fn{} <{}>", flex.sigil(), closure_id.0).unwrap();
              self.show_fn_sig(sig);
            }
            TypeKind::Opaque(type_id, params) => {
              self.str += &self.chart.opaque_types[*type_id].name.0;
              self.show_params(params, inv);
            }
            TypeKind::Struct(struct_id, _, params) => {
              self.str += &self.chart.structs[*struct_id].name.0;
              self.show_params(params, inv);
            }
            TypeKind::Enum(enum_id, params) => {
              self.str += &self.chart.enums[*enum_id].name.0;
              self.show_params(params, inv);
            }
            TypeKind::IfConst(const_id, then, else_) => {
              self.str += "if const ";
              self.str += &self.chart.defs[self.chart.concrete_consts[*const_id].def].path;
              self.str += " { ";
              self.show_type(then.invert_if(inv));
              self.str += " }";
              if !self.types.is_nil(*else_) {
                self.str += " else { ";
                self.show_type(else_.invert_if(inv));
                self.str += " }";
              }
            }
            TypeKind::Union(union_id, _, params) => {
              self.str += &self.chart.unions[*union_id].name.0;
              self.show_params(params, inv);
            }
            TypeKind::Param(_, name) => {
              self.str += &name.0;
            }
            TypeKind::Never => self.str += "!",
            TypeKind::Error(_) => self.str += "??",
          }
        }
      };
    }
  }

  fn show_params(&mut self, params: &[Type], inv: Inverted) {
    if !params.is_empty() {
      self.str += "[";
      self.show_comma_separated(params, inv);
      self.str += "]";
    }
  }

  fn show_comma_separated(&mut self, tys: &[Type], inv: Inverted) {
    let mut first = true;
    for &ty in tys {
      if !first {
        self.str += ", ";
      }
      self.show_type(ty.invert_if(inv));
      first = false;
    }
  }

  fn show_fn_sig(&mut self, sig: &FnSig) {
    self.str += "(";
    let mut first = true;
    for (name, &ty) in sig.names.iter().zip(&sig.param_tys) {
      if !first {
        self.str += ", ";
      }
      self.str += match name {
        Some(name) => &name.0,
        None => "...",
      };
      self.str += ": ";
      self.show_type(ty);
      first = false;
    }
    self.str += ")";
    if !self.types.is_nil(sig.ret_ty) {
      self.str += " -> ";
      self.show_type(sig.ret_ty);
    }
  }

  fn show_anonymous_fn_sig(&mut self, params: impl IntoIterator<Item = Type>, ret: Type) {
    self.str += "(";
    let mut first = true;
    for ty in params {
      if !first {
        self.str += ", ";
      }
      self.show_type(ty);
      first = false;
    }
    self.str += ")";
    if !self.types.is_nil(ret) {
      self.str += " -> ";
      self.show_type(ret);
    }
  }

  fn show_impl_type(&mut self, ty: &ImplType) {
    match ty {
      ImplType::Trait(trait_id, params) => {
        if self.chart.builtins.fn_ == Some(*trait_id)
          && let [receiver, params, ret] = **params
          && let Some((params_inv, TypeKind::Tuple(params))) = self.types.kind(params)
        {
          self.str += "fn ";
          self.show_type(receiver);
          let params = params.iter().map(|t| t.invert_if(params_inv));
          self.show_anonymous_fn_sig(params, ret);
        } else {
          self.str += &self.chart.traits[*trait_id].name.0;
          self.show_params(params, Inverted(false));
        }
      }
      ImplType::Error(_) => self.str += "??",
    }
  }
}
