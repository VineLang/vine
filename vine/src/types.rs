use std::{collections::BTreeMap, fmt::Write, ops::BitAnd};

use logos::Span;
use vine_util::{idx::IdxVec, new_idx};

use crate::{
  ast::{Flex, Ident},
  chart::{AdtId, Chart, TraitDefId, TypeDefId, ValueDefId},
  diag::ErrorGuaranteed,
  tir::ClosureId,
};

new_idx!(pub Type);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind<'core> {
  Opaque(TypeDefId),
  Tuple(Vec<Type>),
  Object(BTreeMap<Ident<'core>, Type>),
  Adt(AdtId, Vec<Type>),
  Ref(Type),
  Inverse(Type),
  Trait(TraitDefId, Vec<Type>),
  Closure(ValueDefId, ClosureId, Flex),
  Param(usize, Ident<'core>),
  Never,
  Error(ErrorGuaranteed),
}

#[derive(Debug)]
enum TypeNode<'core> {
  Root(Option<TypeKind<'core>>, usize),
  Child(Type),
}

#[derive(Debug)]
pub struct Types<'core> {
  types: IdxVec<Type, TypeNode<'core>>,
  error: Option<Type>,
  nil: Option<Type>,
}

impl<'core> Types<'core> {
  pub fn new(&mut self, kind: TypeKind<'core>) -> Type {
    self.types.push(TypeNode::Root(Some(kind), 1))
  }

  pub fn new_var(&mut self) -> Type {
    self.types.push(TypeNode::Root(None, 1))
  }

  pub fn error(&mut self, err: ErrorGuaranteed) -> Type {
    *self
      .error
      .get_or_insert_with(|| self.types.push(TypeNode::Root(Some(TypeKind::Error(err)), 1)))
  }

  pub fn nil(&mut self) -> Type {
    *self
      .nil
      .get_or_insert_with(|| self.types.push(TypeNode::Root(Some(TypeKind::Tuple(vec![])), 1)))
  }

  pub fn inverse(&mut self, ty: Type) -> Type {
    let ty = self.find(ty);
    if let TypeNode::Root(Some(TypeKind::Inverse(ty)), _) = self.types[ty] {
      ty
    } else {
      self.new(TypeKind::Inverse(ty))
    }
  }

  pub fn unify(&mut self, a: Type, b: Type) -> bool {
    let a = self.find_mut(a);
    let b = self.find_mut(b);

    if a == b {
      return true;
    }

    let (a_node, b_node) = self.types.get2_mut(a, b).unwrap();
    let TypeNode::Root(a_kind, a_size) = &mut *a_node else { unreachable!() };
    let TypeNode::Root(b_kind, b_size) = &mut *b_node else { unreachable!() };

    if a_kind.is_none() || b_kind.is_none() {
      let kind = a_kind.take().or(b_kind.take());
      if a_size > b_size {
        *a_kind = kind;
        *a_size += *b_size;
        *b_node = TypeNode::Child(a);
      } else {
        *b_kind = kind;
        *b_size += *a_size;
        *a_node = TypeNode::Child(b);
      }
      return true;
    }

    let mut a_kind = a_kind.take().unwrap();
    let mut b_kind = b_kind.take().unwrap();

    let success = match (&mut a_kind, &mut b_kind) {
      (TypeKind::Error(_), _) | (_, TypeKind::Error(_)) => todo!(),
      (TypeKind::Opaque(i), TypeKind::Opaque(j)) => *i == *j,
      (TypeKind::Param(i, _), TypeKind::Param(j, _)) => *i == *j,
      (TypeKind::Tuple(a), TypeKind::Tuple(b)) if a.len() == b.len() => {
        a.iter().zip(b).map(|(a, b)| self.unify(*a, *b)).fold(true, BitAnd::bitand)
      }
      (TypeKind::Object(a), TypeKind::Object(b)) if a.len() == b.len() => a
        .iter()
        .map(|(k, a)| if let Some(b) = b.get(k) { self.unify(*a, *b) } else { false })
        .fold(true, BitAnd::bitand),
      (TypeKind::Adt(i, a), TypeKind::Adt(j, b)) if *i == *j && a.len() == b.len() => {
        a.iter().zip(b).map(|(a, b)| self.unify(*a, *b)).fold(true, BitAnd::bitand)
      }
      (TypeKind::Ref(a), TypeKind::Ref(b)) => self.unify(*a, *b),
      (TypeKind::Inverse(a), TypeKind::Inverse(b)) => self.unify(*a, *b),
      (TypeKind::Trait(i, a), TypeKind::Trait(j, b)) if *i == *j && a.len() == b.len() => {
        a.iter().zip(b).map(|(a, b)| self.unify(*a, *b)).fold(true, BitAnd::bitand)
      }
      // TypeKind::Closure(value_def_id, closure_id, flex) => todo!(),
      (TypeKind::Never, TypeKind::Never) => todo!(),
      _ => false,
    };

    let (a_node, b_node) = self.types.get2_mut(a, b).unwrap();
    let TypeNode::Root(a_kind_ref, a_size) = &mut *a_node else { unreachable!() };
    let TypeNode::Root(b_kind_ref, b_size) = &mut *b_node else { unreachable!() };

    *a_kind_ref = Some(a_kind);
    *b_kind_ref = Some(b_kind);

    if success {
      if a_size > b_size {
        *a_size += *b_size;
        *b_node = TypeNode::Child(a);
      } else {
        *b_size += *a_size;
        *a_node = TypeNode::Child(b);
      }
    }

    success
  }

  fn find(&self, ty: Type) -> Type {
    let mut cur = ty;
    while let TypeNode::Child(parent) = self.types[cur] {
      cur = parent;
    }
    cur
  }

  fn find_mut(&mut self, ty: Type) -> Type {
    let root = self.find(ty);
    let mut cur = ty;
    while let TypeNode::Child(parent) = &mut self.types[cur] {
      cur = *parent;
      *parent = root;
    }
    ty
  }

  pub(crate) fn kind(&self, ty: Type) -> &Option<TypeKind> {
    let ty = self.find(ty);
    let TypeNode::Root(kind, _) = &self.types[ty] else { unreachable!() };
    kind
  }

  pub fn show(&self, chart: &Chart, ty: Type) -> String {
    let mut str = String::new();
    self._show(chart, ty, &mut str);
    str
  }

  fn _show(&self, chart: &Chart, mut ty: Type, str: &mut String) {
    loop {
      match &self.types[ty] {
        TypeNode::Child(parent) => {
          ty = *parent;
          continue;
        }
        TypeNode::Root(None, _) => write!(str, "?{}", ty.0).unwrap(),
        TypeNode::Root(Some(kind), _) => match kind {
          TypeKind::Opaque(id) => *str += chart.defs[chart.types[*id].def].path,
          TypeKind::Tuple(els) => {
            *str += "(";
            let mut first = true;
            for &el in els {
              if !first {
                *str += ", ";
              }
              self._show(chart, el, str);
              first = false;
            }
            if els.len() == 1 {
              *str += ",";
            }
            *str += ")"
          }
          TypeKind::Object(entries) => {
            if entries.is_empty() {
              *str += "{}"
            } else {
              *str += "{ ";
              let mut first = true;
              for (key, &val) in entries {
                if !first {
                  *str += ", ";
                }
                *str += key.0 .0;
                *str += ": ";
                self._show(chart, val, str);
                first = false;
              }
              *str += " }";
            }
          }
          TypeKind::Ref(ty) => {
            *str += "&";
            self._show(chart, *ty, str)
          }
          TypeKind::Inverse(ty) => {
            *str += "~";
            self._show(chart, *ty, str)
          }
          TypeKind::Adt(adt_id, params) => {
            *str += chart.adts[*adt_id].name.0 .0;
            self._show_params(chart, params, str);
          }
          TypeKind::Trait(trait_id, params) => {
            *str += chart.defs[chart.traits[*trait_id].def].name.0 .0;
            self._show_params(chart, params, str);
          }
          TypeKind::Closure(value_def_id, closure_id, flex) => todo!(),
          TypeKind::Param(_, name) => *str += name.0 .0,
          TypeKind::Never => *str += "!",
          TypeKind::Error(_) => *str += "??",
        },
      }
    }
  }

  fn _show_params(&self, chart: &Chart, params: &[Type], str: &mut String) {
    if !params.is_empty() {
      *str += "[";
      let mut first = true;
      for &ty in params {
        if !first {
          *str += ", ";
        }
        self._show(chart, ty, str);
        first = false;
      }
      *str += "]";
    }
  }
}
