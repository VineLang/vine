use std::{
  collections::{hash_map::Entry, BTreeMap, HashMap},
  fmt::{self, Debug, Write},
};

use vine_util::{
  idx::{IdxVec, IntMap},
  multi_iter, new_idx,
};

use crate::{
  ast::{Ident, Span},
  chart::{Chart, DefId, EnumId, StructId, TraitDefId, TypeDefId},
  diag::ErrorGuaranteed,
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Type(usize);
new_idx!(pub TypeIdx; n => ["t{n}"]);

const INV_BIT: usize = isize::MIN as usize;
impl Type {
  fn new(inv: bool, idx: TypeIdx) -> Type {
    Type(idx.0).invert_if(inv)
  }

  fn inv(&self) -> bool {
    self.0 & INV_BIT != 0
  }

  fn idx(&self) -> TypeIdx {
    TypeIdx(self.0 & !INV_BIT)
  }

  pub fn invert_if(&self, inv: bool) -> Type {
    Type(self.0 ^ (inv as usize * INV_BIT))
  }

  pub fn inverse(&self) -> Type {
    Type(self.0 ^ INV_BIT)
  }
}

impl From<TypeIdx> for Type {
  fn from(value: TypeIdx) -> Self {
    Type(value.0)
  }
}

impl Debug for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.inv() {
      f.write_char('~')?;
    }
    write!(f, "{:?}", self.idx())
  }
}

#[derive(Debug, Clone)]
pub enum TypeKind<'core> {
  Param(usize, Ident<'core>),
  Tuple(Vec<Type>),
  Object(BTreeMap<Ident<'core>, Type>),
  Fn(Vec<Type>, Type),
  Opaque(TypeDefId, Vec<Type>),
  Struct(StructId, Vec<Type>),
  Enum(EnumId, Vec<Type>),
  Ref(Type),
  Never,
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone)]
enum TypeNode<'core> {
  Root(Option<(bool, TypeKind<'core>)>, usize),
  Child(Type),
}

#[derive(Debug, Clone)]
pub enum ImplType {
  Trait(TraitDefId, Vec<Type>),
  Error(ErrorGuaranteed),
}

#[derive(Debug, Default, Clone)]
pub struct Types<'core> {
  types: IdxVec<TypeIdx, TypeNode<'core>>,
  error: Option<Type>,
  nil: Option<Type>,
}

impl<'core> Types<'core> {
  #[allow(clippy::new_ret_no_self)]
  pub fn new(&mut self, kind: TypeKind<'core>) -> Type {
    self.types.push(TypeNode::Root(Some((false, kind)), 1)).into()
  }

  pub fn new_var(&mut self, _span: Span) -> Type {
    self.types.push(TypeNode::Root(None, 1)).into()
  }

  pub fn error(&mut self, err: ErrorGuaranteed) -> Type {
    *self.error.get_or_insert_with(|| {
      self.types.push(TypeNode::Root(Some((false, TypeKind::Error(err))), 1)).into()
    })
  }

  pub fn nil(&mut self) -> Type {
    *self.nil.get_or_insert_with(|| {
      self.types.push(TypeNode::Root(Some((false, TypeKind::Tuple(vec![]))), 1)).into()
    })
  }

  pub fn unify(&mut self, a: Type, b: Type) -> UnifyResult {
    let a = self.find_mut(a);
    let b = self.find_mut(b);

    if a == b {
      return Success;
    }

    let (a_node, b_node) = self.types.get2_mut(a.idx(), b.idx()).unwrap();
    let TypeNode::Root(a_kind, _) = &mut *a_node else { unreachable!() };
    let TypeNode::Root(b_kind, _) = &mut *b_node else { unreachable!() };

    if a_kind.is_none() || b_kind.is_none() {
      if self.occurs(a, b) || self.occurs(b, a) {
        return Failure;
      }
      let (a_node, b_node) = self.types.get2_mut(a.idx(), b.idx()).unwrap();
      let TypeNode::Root(a_kind, a_size) = &mut *a_node else { unreachable!() };
      let TypeNode::Root(b_kind, b_size) = &mut *b_node else { unreachable!() };
      let kind = Option::or(
        a_kind.take().map(|(i, k)| (i ^ a.inv(), k)),
        b_kind.take().map(|(i, k)| (i ^ b.inv(), k)),
      );
      if a_size > b_size {
        *a_kind = kind.map(|(i, k)| (i ^ a.inv(), k));
        *a_size += *b_size;
        *b_node = TypeNode::Child(a.invert_if(b.inv()));
      } else {
        *b_kind = kind.map(|(i, k)| (i ^ b.inv(), k));
        *b_size += *a_size;
        *a_node = TypeNode::Child(b.invert_if(a.inv()));
      }
      return Success;
    }

    let a_kind = a_kind.take().unwrap();
    let b_kind = b_kind.take().unwrap();

    let inverted = a_kind.0 ^ a.inv() != b_kind.0 ^ b.inv();

    let result = match (&a_kind.1, &b_kind.1) {
      (TypeKind::Error(e), _) | (_, TypeKind::Error(e)) => Unknown(*e),
      (TypeKind::Tuple(a), TypeKind::Tuple(b)) => self.unify_types(a, b, inverted),
      (TypeKind::Object(a), TypeKind::Object(b)) => self.unify_objects(a, b, inverted),
      _ if inverted => Failure,
      (TypeKind::Param(i, _), TypeKind::Param(j, _)) if *i == *j => Success,
      (TypeKind::Fn(a, x), TypeKind::Fn(b, y)) => {
        self.unify_types(a, b, false).and(self.unify(*x, *y))
      }
      (TypeKind::Opaque(i, a), TypeKind::Opaque(j, b)) if *i == *j => self.unify_types(a, b, false),
      (TypeKind::Struct(i, a), TypeKind::Struct(j, b)) if *i == *j => self.unify_types(a, b, false),
      (TypeKind::Enum(i, a), TypeKind::Enum(j, b)) if *i == *j => self.unify_types(a, b, false),
      (TypeKind::Ref(a), TypeKind::Ref(b)) => self.unify(*a, *b),
      (TypeKind::Never, TypeKind::Never) => Success,
      _ => Failure,
    };

    let (a_node, b_node) = self.types.get2_mut(a.idx(), b.idx()).unwrap();
    let TypeNode::Root(a_kind_ref, a_size) = &mut *a_node else { unreachable!() };
    let TypeNode::Root(b_kind_ref, b_size) = &mut *b_node else { unreachable!() };

    *a_kind_ref = Some(a_kind);
    *b_kind_ref = Some(b_kind);

    if result.is_success() {
      if a_size > b_size {
        *a_size += *b_size;
        *b_node = TypeNode::Child(a.invert_if(b.inv()));
      } else {
        *b_size += *a_size;
        *a_node = TypeNode::Child(b.invert_if(a.inv()));
      }
    }

    result
  }

  pub fn unify_types(&mut self, a: &[Type], b: &[Type], inv: bool) -> UnifyResult {
    if a.len() == b.len() {
      UnifyResult::all(a.iter().zip(b).map(|(&a, &b)| self.unify(a, b.invert_if(inv))))
    } else {
      UnifyResult::Failure
    }
  }

  pub fn unify_objects(
    &mut self,
    a: &BTreeMap<Ident<'core>, Type>,
    b: &BTreeMap<Ident<'core>, Type>,
    inv: bool,
  ) -> UnifyResult {
    UnifyResult::from_bool(a.len() == b.len()).and(UnifyResult::all(a.iter().map(|(k, &a)| {
      if let Some(&b) = b.get(k) {
        self.unify(a, b.invert_if(inv))
      } else {
        Failure
      }
    })))
  }

  pub fn unify_impl_type(&mut self, a: &ImplType, b: &ImplType) -> UnifyResult {
    match (a, b) {
      (ImplType::Error(e), _) | (_, ImplType::Error(e)) => Unknown(*e),
      (ImplType::Trait(i, a), ImplType::Trait(j, b)) if i == j => self.unify_types(a, b, false),
      _ => Failure,
    }
  }

  fn occurs(&self, var: Type, ty: Type) -> bool {
    ty == var
      || self.kind(ty).as_ref().is_some_and(|kind| kind.1.children().any(|t| self.occurs(var, t)))
  }

  fn find(&self, ty: Type) -> Type {
    let mut inv = ty.inv();
    let mut cur = ty.idx();
    while let TypeNode::Child(parent) = self.types[cur] {
      inv ^= parent.inv();
      cur = parent.idx();
    }
    Type::new(inv, cur)
  }

  fn find_mut(&mut self, ty: Type) -> Type {
    let root = self.find(ty);
    let mut inv = ty.inv();
    let mut cur = ty.idx();
    while let TypeNode::Child(parent) = &mut self.types[cur] {
      let prev_parent = *parent;
      *parent = root.invert_if(inv);
      inv ^= prev_parent.inv();
      cur = prev_parent.idx();
    }
    root
  }

  pub(crate) fn kind(&self, ty: Type) -> Option<(bool, &TypeKind<'core>)> {
    let ty = self.find(ty);
    let TypeNode::Root(kind, _) = &self.types[ty.idx()] else { unreachable!() };
    kind.as_ref().map(|(inv, kind)| (inv ^ ty.inv(), kind))
  }

  pub(crate) fn force_kind(&self, ty: Type) -> (bool, &TypeKind<'core>) {
    // todo
    self.kind(ty).unwrap()
  }

  pub fn show(&self, chart: &Chart, ty: Type) -> String {
    let mut str = String::new();
    self._show(chart, ty, &mut str);
    str
  }

  fn _show(&self, chart: &Chart, mut ty: Type, str: &mut String) {
    loop {
      break match &self.types[ty.idx()] {
        TypeNode::Child(parent) => {
          ty = parent.invert_if(ty.inv());
          continue;
        }
        TypeNode::Root(None, _) => {
          write!(str, "{}?{}", if ty.inv() { "~" } else { "" }, ty.0).unwrap()
        }
        TypeNode::Root(Some((inv, kind)), _) => {
          if inv ^ ty.inv() {
            *str += "~";
          }
          match kind {
            TypeKind::Tuple(els) => {
              *str += "(";
              self._show_comma_separated(chart, els, str);
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
            TypeKind::Fn(params, ret) => {
              *str += "fn(";
              self._show_comma_separated(chart, params, str);
              *str += ")";
              if !matches!(self.kind(*ret), Some((false, TypeKind::Tuple(els))) if els.is_empty()) {
                *str += " -> ";
                self._show(chart, *ret, str);
              }
            }
            TypeKind::Ref(ty) => {
              *str += "&";
              self._show(chart, *ty, str)
            }
            TypeKind::Opaque(type_id, params) => {
              *str += chart.defs[chart.types[*type_id].def].name.0 .0;
              self._show_params(chart, params, str);
            }
            TypeKind::Struct(struct_id, params) => {
              *str += chart.structs[*struct_id].name.0 .0;
              self._show_params(chart, params, str);
            }
            TypeKind::Enum(enum_id, params) => {
              *str += chart.enums[*enum_id].name.0 .0;
              self._show_params(chart, params, str);
            }
            TypeKind::Param(_, name) => {
              *str += name.0 .0;
            }
            TypeKind::Never => *str += "!",
            TypeKind::Error(_) => *str += "??",
          }
        }
      };
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
      ImplType::Error(_) => str += "??",
    }
    str
  }

  pub fn import<T, U>(
    &mut self,
    source: &TypeCtx<'core, T>,
    params: Option<&[Type]>,
    f: impl FnOnce(&mut TypeTransfer<'core, '_>, &T) -> U,
  ) -> U {
    f(
      &mut TypeTransfer { mapping: HashMap::default(), source: &source.types, dest: self, params },
      &source.inner,
    )
  }

  pub fn export<'ctx, T>(
    &'ctx self,
    f: impl FnOnce(&mut TypeTransfer<'core, '_>) -> T,
  ) -> TypeCtx<'core, T> {
    let mut types = Types::default();
    let mut export =
      TypeTransfer { mapping: HashMap::default(), source: self, dest: &mut types, params: None };
    export.dest.error = export.source.error.map(|t| export.transfer(t));
    export.dest.nil = export.source.nil.map(|t| export.transfer(t));
    let inner = f(&mut export);
    TypeCtx { types, inner }
  }

  pub fn get_mod(&self, chart: &Chart, ty: Type) -> Option<DefId> {
    match self.kind(ty).as_ref()?.1 {
      TypeKind::Opaque(id, _) => Some(chart.types[*id].def),
      TypeKind::Struct(id, _) => Some(chart.structs[*id].def),
      TypeKind::Enum(id, _) => Some(chart.enums[*id].def),
      TypeKind::Ref(inner) => self.get_mod(chart, *inner),
      TypeKind::Fn(..)
      | TypeKind::Param(..)
      | TypeKind::Tuple(..)
      | TypeKind::Object(..)
      | TypeKind::Never
      | TypeKind::Error(_) => None,
    }
  }

  pub fn reset(&mut self) {
    self.types.clear();
    self.error = None;
    self.nil = None;
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

pub struct TypeTransfer<'core, 'ctx> {
  mapping: IntMap<TypeIdx, Type>,
  source: &'ctx Types<'core>,
  dest: &'ctx mut Types<'core>,
  params: Option<&'ctx [Type]>,
}

impl<'core, 'ctx> TypeTransfer<'core, 'ctx> {
  pub fn transfer(&mut self, ty: Type) -> Type {
    let old = self.source.find(ty);
    match self.mapping.entry(old.idx()) {
      Entry::Occupied(e) => (*e.get()).invert_if(old.inv()),
      Entry::Vacant(e) => {
        let old_kind = self.source.kind(old.idx().into());
        if let Some(params) = self.params {
          if let Some((inv, TypeKind::Param(p, _))) = old_kind {
            return params[*p].invert_if(inv ^ old.inv());
          }
        }
        let new = self.dest.new_var(Span::NONE);
        e.insert(new);
        if let Some((inv, old_kind)) = old_kind {
          let kind = old_kind.map(|t| self.transfer(t));
          let TypeNode::Root(new_kind, _) = &mut self.dest.types[new.idx()] else { unreachable!() };
          *new_kind = Some((inv, kind))
        }
        new.invert_if(old.inv())
      }
    }
  }

  pub fn transfer_impl_type(&mut self, ty: &ImplType) -> ImplType {
    ty.map(|t| self.transfer(t))
  }
}

impl<'core> TypeKind<'core> {
  fn map(&self, mut f: impl FnMut(Type) -> Type) -> Self {
    match self {
      TypeKind::Tuple(els) => TypeKind::Tuple(els.iter().copied().map(f).collect()),
      TypeKind::Object(els) => TypeKind::Object(els.iter().map(|(&k, &v)| (k, f(v))).collect()),
      TypeKind::Opaque(i, els) => TypeKind::Opaque(*i, els.iter().copied().map(f).collect()),
      TypeKind::Struct(i, els) => TypeKind::Struct(*i, els.iter().copied().map(f).collect()),
      TypeKind::Enum(i, els) => TypeKind::Enum(*i, els.iter().copied().map(f).collect()),
      TypeKind::Ref(t) => TypeKind::Ref(f(*t)),
      TypeKind::Fn(p, r) => TypeKind::Fn(p.iter().copied().map(&mut f).collect(), f(*r)),
      TypeKind::Param(i, n) => TypeKind::Param(*i, *n),
      TypeKind::Never => TypeKind::Never,
      TypeKind::Error(err) => TypeKind::Error(*err),
    }
  }

  fn children(&self) -> impl Iterator<Item = Type> + '_ {
    multi_iter! { Children { Zero, One, Vec, Object, Fn } }
    match self {
      TypeKind::Param(..) | TypeKind::Never | TypeKind::Error(_) => Children::Zero([]),
      TypeKind::Ref(t) => Children::One([*t]),
      TypeKind::Tuple(els)
      | TypeKind::Opaque(_, els)
      | TypeKind::Struct(_, els)
      | TypeKind::Enum(_, els) => Children::Vec(els.iter().copied()),
      TypeKind::Object(els) => Children::Object(els.values().copied()),
      TypeKind::Fn(p, r) => Children::Fn(p.iter().copied().chain([*r])),
    }
  }
}

impl ImplType {
  pub fn approx_eq(&self, other: &ImplType) -> bool {
    match (self, other) {
      (ImplType::Trait(i, _), ImplType::Trait(j, _)) => i == j,
      _ => false,
    }
  }
  fn map(&self, f: impl FnMut(Type) -> Type) -> Self {
    match self {
      ImplType::Trait(def, ts) => ImplType::Trait(*def, ts.iter().copied().map(f).collect()),
      ImplType::Error(e) => ImplType::Error(*e),
    }
  }
}

#[derive(Debug)]
pub struct TypeCtx<'core, T> {
  pub types: Types<'core>,
  pub inner: T,
}
