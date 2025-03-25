use std::{
  collections::{hash_map::Entry, BTreeMap, HashMap},
  fmt::Write,
};

use vine_util::{
  idx::{IdxVec, IntMap},
  new_idx,
};

use crate::{
  ast::Ident,
  chart::{AdtId, Chart, TraitDefId, TypeDefId, ValueDefId},
  diag::ErrorGuaranteed,
  tir::{ClosureId, TirImpl},
};

new_idx!(pub Type);

#[derive(Debug, Clone)]
pub enum TypeKind<'core> {
  Opaque(TypeDefId),
  Tuple(Vec<Type>),
  Object(BTreeMap<Ident<'core>, Type>),
  Adt(AdtId, Vec<Type>),
  Ref(Type),
  Inverse(Type),
  Fn(ValueDefId, Vec<Type>, Vec<TirImpl>, Vec<Type>, Type),
  Closure(ClosureId),
  Param(usize, Ident<'core>),
  Never,
  Error(ErrorGuaranteed),
}

#[derive(Debug)]
enum TypeNode<'core> {
  Root(Option<TypeKind<'core>>, usize),
  Child(Type),
}

#[derive(Debug, Clone)]
pub enum ImplType {
  Trait(TraitDefId, Vec<Type>),
  Fn(Type, Vec<Type>, Type),
  Error(ErrorGuaranteed),
}

#[derive(Debug, Default)]
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

  pub fn unify(&mut self, a: Type, b: Type) -> UnifyResult {
    let a = self.find_mut(a);
    let b = self.find_mut(b);

    if a == b {
      return Success;
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
      return Success;
    }

    let mut a_kind = a_kind.take().unwrap();
    let mut b_kind = b_kind.take().unwrap();

    let result =
      match (&mut a_kind, &mut b_kind) {
        (TypeKind::Error(e), _) | (_, TypeKind::Error(e)) => Unknown(*e),
        (TypeKind::Opaque(i), TypeKind::Opaque(j)) if *i == *j => Success,
        (TypeKind::Param(i, _), TypeKind::Param(j, _)) if *i == *j => Success,
        (TypeKind::Closure(i), TypeKind::Closure(j)) if *i == *j => Success,
        (TypeKind::Tuple(a), TypeKind::Tuple(b)) if a.len() == b.len() => {
          UnifyResult::all(a.iter().zip(b).map(|(a, b)| self.unify(*a, *b)))
        }
        (TypeKind::Object(a), TypeKind::Object(b)) => {
          UnifyResult::from_bool(a.len() == b.len()).and(UnifyResult::all(
            a.iter()
              .map(|(k, a)| if let Some(b) = b.get(k) { self.unify(*a, *b) } else { Failure }),
          ))
        }
        (TypeKind::Adt(i, a), TypeKind::Adt(j, b)) if *i == *j && a.len() == b.len() => {
          UnifyResult::all(a.iter().zip(b).map(|(a, b)| self.unify(*a, *b)))
        }
        (TypeKind::Ref(a), TypeKind::Ref(b)) => self.unify(*a, *b),
        (TypeKind::Inverse(a), TypeKind::Inverse(b)) => self.unify(*a, *b),
        (TypeKind::Fn(a, p, x, _, _), TypeKind::Fn(b, q, y, _, _)) if a == b && x == y => {
          UnifyResult::all(p.iter().zip(q).map(|(a, b)| self.unify(*a, *b)))
        }
        (TypeKind::Never, TypeKind::Never) => Success,
        _ => Failure,
      };

    let (a_node, b_node) = self.types.get2_mut(a, b).unwrap();
    let TypeNode::Root(a_kind_ref, a_size) = &mut *a_node else { unreachable!() };
    let TypeNode::Root(b_kind_ref, b_size) = &mut *b_node else { unreachable!() };

    *a_kind_ref = Some(a_kind);
    *b_kind_ref = Some(b_kind);

    if result.is_success() {
      if a_size > b_size {
        *a_size += *b_size;
        *b_node = TypeNode::Child(a);
      } else {
        *b_size += *a_size;
        *a_node = TypeNode::Child(b);
      }
    }

    result
  }

  pub fn unify_impl_type(&mut self, a: &ImplType, b: &ImplType) -> UnifyResult {
    match (a, b) {
      (ImplType::Error(e), _) | (_, ImplType::Error(e)) => Unknown(*e),
      (ImplType::Fn(p, a, x), ImplType::Fn(q, b, y)) if a.len() == b.len() => self
        .unify(*p, *q)
        .and(UnifyResult::all(a.iter().zip(b).map(|(a, b)| self.unify(*a, *b))))
        .and(self.unify(*x, *y)),
      (ImplType::Trait(i, a), ImplType::Trait(j, b)) if i == j => {
        UnifyResult::all(a.iter().zip(b).map(|(a, b)| self.unify(*a, *b)))
      }
      _ => Failure,
    }
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

  pub(crate) fn kind(&self, ty: Type) -> &Option<TypeKind<'core>> {
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
          TypeKind::Closure(closure_id) => {
            *str += "fn#";
            write!(*str, "{}", closure_id.0).unwrap();
          }
          TypeKind::Fn(value_id, params, _, _, _) => {
            *str += "fn";
            *str += " ";
            *str += chart.defs[chart.values[*value_id].def].name.0 .0;
            self._show_params(chart, params, str);
          }
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
      self._show_comma_separated(chart, params, str);
      *str += "]";
    }
  }

  fn _show_comma_separated(&self, chart: &Chart<'_>, tys: &[Type], str: &mut String) {
    let mut first = true;
    for &ty in tys {
      if !first {
        *str += ", ";
      }
      self._show(chart, ty, str);
      first = false;
    }
  }

  pub fn show_impl_type(&self, chart: &Chart, ty: &ImplType) -> String {
    let mut str = String::new();
    match ty {
      ImplType::Trait(trait_id, params) => {
        str += chart.defs[chart.traits[*trait_id].def].name.0 .0;
        self._show_params(chart, params, &mut str);
      }
      ImplType::Fn(recv, params, ret) => {
        str += "fn ";
        self._show(chart, *recv, &mut str);
        str += "(";
        self._show_comma_separated(chart, params, &mut str);
        str += ")";
        if !matches!(self.kind(*ret), Some(TypeKind::Tuple(x)) if x.is_empty()) {
          str += " -> ";
          self._show(chart, *ret, &mut str);
        }
      }
      ImplType::Error(_) => str += "??",
    }
    str
  }

  pub fn export(&mut self) -> TypeExport<'core, '_> {
    let mut export =
      TypeExport { mapping: HashMap::default(), source: self, dest: Types::default() };
    export.dest.error = export.source.error.map(|t| export.export(t));
    export.dest.nil = export.source.nil.map(|t| export.export(t));
    export
  }
}

#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnifyResult {
  Success,
  Failure,
  Unknown(ErrorGuaranteed),
}

use UnifyResult::*;

impl UnifyResult {
  pub fn and(self, other: Self) -> Self {
    match (self, other) {
      (Failure, _) | (_, Failure) => Failure,
      (Unknown(e), _) | (_, Unknown(e)) => Unknown(e),
      (Success, Success) => Success,
    }
  }

  pub fn all(i: impl IntoIterator<Item = Self>) -> Self {
    i.into_iter().fold(Success, Self::and)
  }

  pub fn from_bool(b: bool) -> Self {
    if b {
      Success
    } else {
      Failure
    }
  }

  pub fn is_success(self) -> bool {
    matches!(self, Success)
  }

  pub fn is_ok(self) -> bool {
    !matches!(self, Failure)
  }
}

pub struct TypeExport<'core, 'ctx> {
  mapping: IntMap<Type, Type>,
  source: &'ctx Types<'core>,
  dest: Types<'core>,
}

impl<'core, 'ctx> TypeExport<'core, 'ctx> {
  pub fn export(&mut self, ty: Type) -> Type {
    let old = self.source.find(ty);
    match self.mapping.entry(old) {
      Entry::Occupied(e) => *e.get(),
      Entry::Vacant(e) => {
        let new = self.dest.new_var();
        e.insert(new);
        if let Some(old_kind) = self.source.kind(old) {
          let kind = old_kind.map(|t| self.export(t));
          let TypeNode::Root(new_kind, _) = &mut self.dest.types[new] else { unreachable!() };
          *new_kind = Some(kind)
        }
        new
      }
    }
  }

  pub fn finish(self) -> Types<'core> {
    self.dest
  }
}

impl<'core> TypeKind<'core> {
  fn map(&self, mut f: impl FnMut(Type) -> Type) -> Self {
    match self {
      TypeKind::Opaque(i) => TypeKind::Opaque(*i),
      TypeKind::Tuple(els) => TypeKind::Tuple(els.iter().copied().map(f).collect()),
      TypeKind::Object(els) => TypeKind::Object(els.iter().map(|(&k, &v)| (k, f(v))).collect()),
      TypeKind::Adt(i, els) => TypeKind::Adt(*i, els.iter().copied().map(f).collect()),
      TypeKind::Ref(t) => TypeKind::Ref(f(*t)),
      TypeKind::Inverse(t) => TypeKind::Inverse(f(*t)),
      TypeKind::Fn(i, p, x, q, r) => TypeKind::Fn(
        *i,
        p.iter().copied().map(&mut f).collect(),
        x.clone(),
        q.iter().copied().map(&mut f).collect(),
        f(*r),
      ),
      TypeKind::Closure(i) => TypeKind::Closure(*i),
      TypeKind::Param(i, n) => TypeKind::Param(*i, *n),
      TypeKind::Never => TypeKind::Never,
      TypeKind::Error(err) => TypeKind::Error(*err),
    }
  }
}
