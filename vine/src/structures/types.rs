use std::{
  collections::{BTreeMap, HashMap, hash_map::Entry},
  fmt::{self, Debug, Write},
  mem::take,
  ops::{BitXor, BitXorAssign},
};

use vine_util::{
  idx::{Idx, IdxVec, IntMap, RangeExt},
  multi_iter, new_idx,
};

use crate::structures::{
  ast::{Flex, Ident, Span},
  chart::{Chart, ConcreteConstId, DefId, EnumId, FnId, OpaqueTypeId, StructId, TraitId},
  diag::{Diag, Diags, ErrorGuaranteed},
  signatures::FnSig,
  tir::ClosureId,
};

/// A type variable, or its inverse. The high bit of the usize denotes the
/// inverse. The remaining bits are a `TypeIdx`.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Type(usize);
new_idx!(pub TypeIdx; n => ["t{n}"]);

const INV_BIT: usize = isize::MIN as usize;
impl Type {
  fn new(inv: Inverted, idx: TypeIdx) -> Type {
    Type(idx.0).invert_if(inv)
  }

  fn inv(self) -> Inverted {
    Inverted(self.0 & INV_BIT != 0)
  }

  fn idx(self) -> TypeIdx {
    TypeIdx(self.0 & !INV_BIT)
  }

  pub fn invert_if(self, inv: Inverted) -> Type {
    if inv.0 { self.inverse() } else { self }
  }

  pub fn inverse(self) -> Type {
    Type(self.0 ^ INV_BIT)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Inverted(pub bool);

impl From<TypeIdx> for Type {
  fn from(value: TypeIdx) -> Self {
    Type(value.0)
  }
}

impl BitXor for Inverted {
  type Output = Self;
  fn bitxor(self, rhs: Self) -> Self::Output {
    Inverted(self.0 ^ rhs.0)
  }
}

impl BitXorAssign for Inverted {
  fn bitxor_assign(&mut self, rhs: Self) {
    self.0 ^= rhs.0;
  }
}

impl Debug for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.inv().0 {
      f.write_char('~')?;
    }
    write!(f, "{:?}", self.idx())
  }
}

#[derive(Debug, Clone)]
pub enum TypeKind {
  Param(usize, Ident),
  Tuple(Vec<Type>),
  Object(BTreeMap<Ident, Type>),
  Opaque(OpaqueTypeId, Vec<Type>),
  Struct(StructId, Vec<Type>),
  Enum(EnumId, Vec<Type>),
  Fn(FnId),
  Closure(ClosureId, Flex, FnSig),
  Ref(Type),
  Key(Ident),
  IfConst(ConcreteConstId, Type, Type),
  Never,
  Default,
  Error(ErrorGuaranteed),
}

/// A node in the union-find tree for the unification of type variables.
#[derive(Debug, Clone)]
enum TypeNode {
  Root {
    state: TypeState,
    // The size of the tree rooted at this node.
    size: usize,
  },
  Child(Type),
}

use TypeNode::*;

#[derive(Debug, Clone)]
/// The state of a type variable.
enum TypeState {
  Known(Inverted, TypeKind),
  /// Not yet inferred; if unable to infer, an error should be issued at the
  /// given `Span`.
  Unknown(Span),
  /// Not inferred, and an error has been issued reporting the inference
  /// failure.
  InferenceFailed(ErrorGuaranteed),
}

use TypeState::*;

impl TypeState {
  fn kind(&self) -> Option<(Inverted, &TypeKind)> {
    match self {
      Known(inverted, type_kind) => Some((*inverted, type_kind)),
      _ => None,
    }
  }

  fn take_kind(&mut self) -> Option<(Inverted, TypeKind)> {
    match self {
      Known(..) => {
        let Known(inverted, kind) = take(self) else { unreachable!() };
        Some((inverted, kind))
      }
      _ => None,
    }
  }
}

impl Default for TypeState {
  fn default() -> Self {
    TypeState::Unknown(Span::NONE)
  }
}

#[derive(Debug, Clone)]
pub enum ImplType {
  Trait(TraitId, Vec<Type>),
  Error(ErrorGuaranteed),
}

#[derive(Debug, Default, Clone)]
pub struct Types {
  types: IdxVec<TypeIdx, TypeNode>,
  error: Option<Type>,
  nil: Option<Type>,
}

impl Types {
  #[allow(clippy::new_ret_no_self)]
  pub fn new(&mut self, kind: TypeKind) -> Type {
    self.types.push(Root { state: Known(Inverted(false), kind), size: 1 }).into()
  }

  pub fn new_var(&mut self, span: Span) -> Type {
    self.types.push(Root { state: Unknown(span), size: 1 }).into()
  }

  pub fn new_vars(&mut self, span: Span, len: usize) -> Vec<Type> {
    (0..len).iter().map(|_| self.new_var(span)).collect()
  }

  pub fn error(&mut self, err: ErrorGuaranteed) -> Type {
    *self.error.get_or_insert_with(|| {
      self.types.push(Root { state: Known(Inverted(false), TypeKind::Error(err)), size: 1 }).into()
    })
  }

  pub fn nil(&mut self) -> Type {
    *self.nil.get_or_insert_with(|| {
      self
        .types
        .push(Root { state: Known(Inverted(false), TypeKind::Tuple(vec![])), size: 1 })
        .into()
    })
  }

  pub fn unify(&mut self, a: Type, b: Type) -> UnifyResult {
    let a = self.find_mut(a);
    let b = self.find_mut(b);

    if a.idx() == b.idx() {
      return UnifyResult::from_bool(a.inverse() == b.inverse() || self.self_dual(a));
    }

    let (a_node, b_node) = self.types.get2_mut(a.idx(), b.idx()).unwrap();
    let Root { state: a_state, .. } = &mut *a_node else { unreachable!() };
    let Root { state: b_state, .. } = &mut *b_node else { unreachable!() };

    if a_state.kind().is_none() || b_state.kind().is_none() {
      if self.occurs(a, b) || self.occurs(b, a) {
        return Failure;
      }
      let (a_node, b_node) = self.types.get2_mut(a.idx(), b.idx()).unwrap();
      let Root { state: a_state, size: a_size } = &mut *a_node else { unreachable!() };
      let Root { state: b_state, size: b_size } = &mut *b_node else { unreachable!() };
      let kind = Option::or(
        a_state.take_kind().map(|(i, k)| (i ^ a.inv(), k)),
        b_state.take_kind().map(|(i, k)| (i ^ b.inv(), k)),
      );
      if a_size > b_size {
        if let Some((inv, kind)) = kind {
          *a_state = Known(inv ^ a.inv(), kind);
        }
        *a_size += *b_size;
        *b_node = Child(a.invert_if(b.inv()));
      } else {
        if let Some((inv, kind)) = kind {
          *b_state = Known(inv ^ b.inv(), kind);
        }
        *b_size += *a_size;
        *a_node = Child(b.invert_if(a.inv()));
      }
      return Success;
    }

    let (a_inv, a_kind) = a_state.kind().unwrap();
    let (b_inv, b_kind) = b_state.kind().unwrap();
    let a_kind = a_kind.clone();
    let b_kind = b_kind.clone();

    let inverted = Inverted(a_inv ^ a.inv() != b_inv ^ b.inv());

    let result = match (&a_kind, &b_kind) {
      (TypeKind::Error(e), _) | (_, TypeKind::Error(e)) => Indeterminate(*e),
      (TypeKind::Tuple(a), TypeKind::Tuple(b)) => self.unify_types(a, b, inverted),
      (TypeKind::Object(a), TypeKind::Object(b)) => self.unify_objects(a, b, inverted),
      (TypeKind::IfConst(a, t, e), TypeKind::IfConst(b, u, f)) if a == b => {
        self.unify(*t, u.invert_if(inverted)).and(self.unify(*e, f.invert_if(inverted)))
      }
      _ if inverted.0 => Failure,
      (TypeKind::Param(i, _), TypeKind::Param(j, _)) if *i == *j => Success,
      (TypeKind::Opaque(OpaqueTypeId(i), a), TypeKind::Opaque(OpaqueTypeId(j), b))
      | (TypeKind::Struct(StructId(i), a), TypeKind::Struct(StructId(j), b))
      | (TypeKind::Enum(EnumId(i), a), TypeKind::Enum(EnumId(j), b))
        if *i == *j =>
      {
        self.unify_types(a, b, Inverted(false))
      }
      (TypeKind::Fn(i), TypeKind::Fn(j)) if *i == *j => Success,
      (TypeKind::Closure(i, _, x), TypeKind::Closure(j, _, y)) if *i == *j => self
        .unify_types(&x.param_tys, &y.param_tys, Inverted(false))
        .and(self.unify(x.ret_ty, y.ret_ty)),
      (TypeKind::Ref(a), TypeKind::Ref(b)) => self.unify(*a, *b),
      (TypeKind::Key(a), TypeKind::Key(b)) if a == b => Success,
      (TypeKind::Never, TypeKind::Never) => Success,
      (TypeKind::Default, TypeKind::Default) => Success,
      _ => Failure,
    };

    let (a_node, b_node) = self.types.get2_mut(a.idx(), b.idx()).unwrap();
    let Root { state: a_state, size: a_size } = &mut *a_node else { unreachable!() };
    let Root { state: b_state, size: b_size } = &mut *b_node else { unreachable!() };

    *a_state = Known(a_inv, a_kind);
    *b_state = Known(b_inv, b_kind);

    if result.is_success() {
      if a_size > b_size {
        *a_size += *b_size;
        *b_node = Child(a.invert_if(b.inv()));
      } else {
        *b_size += *a_size;
        *a_node = Child(b.invert_if(a.inv()));
      }
    }

    result
  }

  pub fn unify_types(&mut self, a: &[Type], b: &[Type], inv: Inverted) -> UnifyResult {
    if a.len() == b.len() {
      UnifyResult::all(a.iter().zip(b).map(|(&a, &b)| self.unify(a, b.invert_if(inv))))
    } else {
      UnifyResult::Failure
    }
  }

  pub fn unify_objects(
    &mut self,
    a: &BTreeMap<Ident, Type>,
    b: &BTreeMap<Ident, Type>,
    inv: Inverted,
  ) -> UnifyResult {
    UnifyResult::from_bool(a.len() == b.len()).and(UnifyResult::all(a.iter().map(|(k, &a)| {
      if let Some(&b) = b.get(k) { self.unify(a, b.invert_if(inv)) } else { Failure }
    })))
  }

  pub fn unify_impl_type(&mut self, a: &ImplType, b: &ImplType) -> UnifyResult {
    match (a, b) {
      (ImplType::Error(e), _) | (_, ImplType::Error(e)) => Indeterminate(*e),
      (ImplType::Trait(i, a), ImplType::Trait(j, b)) if i == j => {
        self.unify_types(a, b, Inverted(false))
      }
      _ => Failure,
    }
  }

  pub fn unify_fn_sig(&mut self, a: &FnSig, b: &FnSig) -> UnifyResult {
    self
      .unify_types(&a.param_tys, &b.param_tys, Inverted(false))
      .and(self.unify(a.ret_ty, b.ret_ty))
  }

  fn occurs(&self, var: Type, ty: Type) -> bool {
    debug_assert!(var == self.find(var) && ty == self.find(ty));
    ty.idx() == var.idx()
      || self
        .kind(ty)
        .as_ref()
        .is_some_and(|kind| kind.1.children().any(|t| self.occurs(var, self.find(t))))
  }

  fn find(&self, ty: Type) -> Type {
    let mut inv = ty.inv();
    let mut cur = ty.idx();
    while let Child(parent) = self.types[cur] {
      inv ^= parent.inv();
      cur = parent.idx();
    }
    Type::new(inv, cur)
  }

  fn find_mut(&mut self, ty: Type) -> Type {
    let root = self.find(ty);
    let mut inv = ty.inv();
    let mut cur = ty.idx();
    while let Child(parent) = &mut self.types[cur] {
      let prev_parent = *parent;
      *parent = root.invert_if(inv);
      inv ^= prev_parent.inv();
      cur = prev_parent.idx();
    }
    root
  }

  pub(crate) fn kind(&self, ty: Type) -> Option<(Inverted, &TypeKind)> {
    let ty = self.find(ty);
    let Root { state, .. } = &self.types[ty.idx()] else { unreachable!() };
    state.kind().map(|(inv, kind)| (inv ^ ty.inv(), kind))
  }

  pub(crate) fn self_dual(&self, ty: Type) -> bool {
    match self.kind(ty) {
      Some((_, TypeKind::Tuple(elements))) => elements.iter().all(|&x| self.self_dual(x)),
      Some((_, TypeKind::Object(entries))) => entries.values().all(|&x| self.self_dual(x)),
      Some((_, TypeKind::Default | TypeKind::Error(_))) => true,
      _ => false,
    }
  }

  pub(crate) fn force_kind(&mut self, diags: &mut Diags, ty: Type) -> (Inverted, &TypeKind) {
    let ty = self.find_mut(ty);
    let Root { state, .. } = &mut self.types[ty.idx()] else { unreachable!() };
    const ERROR: &TypeKind = &TypeKind::Error(ErrorGuaranteed::new_unchecked());
    match state {
      Known(inverted, kind) => (*inverted ^ ty.inv(), kind),
      Unknown(span) => {
        let err = diags.error(Diag::CannotInfer { span: *span });
        *state = InferenceFailed(err);
        (Inverted(false), ERROR)
      }
      InferenceFailed(_) => (Inverted(false), ERROR),
    }
  }

  pub fn show(&self, chart: &Chart, ty: Type) -> String {
    let mut str = String::new();
    self._show(chart, ty, &mut str);
    str
  }

  fn _show(&self, chart: &Chart, mut ty: Type, str: &mut String) {
    loop {
      break match &self.types[ty.idx()] {
        Child(parent) => {
          ty = parent.invert_if(ty.inv());
          continue;
        }
        Root { state: Unknown(..) | InferenceFailed(..), .. } => {
          write!(str, "{}?{}", if ty.inv().0 { "~" } else { "" }, ty.idx().0).unwrap()
        }
        Root { state: Known(inv, kind), .. } => {
          if (*inv ^ ty.inv()).0 {
            *str += "~";
          }
          match kind {
            TypeKind::Default => {
              *str += "_";
            }
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
                  *str += &key.0;
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
            TypeKind::Key(key) => {
              *str += ".";
              *str += &key.0;
            }
            TypeKind::Fn(fn_id) => {
              *str += "fn ";
              match fn_id {
                FnId::Concrete(fn_id) => {
                  *str += &chart.defs[chart.concrete_fns[*fn_id].def].path;
                }
                FnId::Abstract(trait_id, fn_id) => {
                  *str += &chart.defs[chart.traits[*trait_id].def].path;
                  *str += "::";
                  *str += &chart.traits[*trait_id].fns[*fn_id].name.0;
                }
              }
            }
            TypeKind::Closure(closure_id, flex, sig) => {
              write!(*str, "fn{} <{}>", flex.sigil(), closure_id.0).unwrap();
              self._show_fn_sig(chart, sig, str);
            }
            TypeKind::Opaque(type_id, params) => {
              *str += &chart.opaque_types[*type_id].name.0;
              self._show_params(chart, params, str);
            }
            TypeKind::Struct(struct_id, params) => {
              *str += &chart.structs[*struct_id].name.0;
              self._show_params(chart, params, str);
            }
            TypeKind::Enum(enum_id, params) => {
              *str += &chart.enums[*enum_id].name.0;
              self._show_params(chart, params, str);
            }
            TypeKind::IfConst(const_id, then, else_) => {
              *str += "if const ";
              *str += &chart.defs[chart.concrete_consts[*const_id].def].path;
              *str += " { ";
              self._show(chart, *then, str);
              *str += " }";
              if !matches!(self.kind(*else_), Some((_, TypeKind::Tuple(els))) if els.is_empty()) {
                *str += " else { ";
                self._show(chart, *else_, str);
                *str += " }";
              }
            }
            TypeKind::Param(_, name) => {
              *str += &name.0;
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

  fn _show_comma_separated(&self, chart: &Chart, tys: &[Type], str: &mut String) {
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
        str += &chart.traits[*trait_id].name.0;
        self._show_params(chart, params, &mut str);
      }
      ImplType::Error(_) => str += "??",
    }
    str
  }

  pub fn show_fn_sig(&self, chart: &Chart, sig: &FnSig) -> String {
    let mut str = String::new();
    self._show_fn_sig(chart, sig, &mut str);
    str
  }

  fn _show_fn_sig(&self, chart: &Chart, sig: &FnSig, str: &mut String) {
    let ret = sig.ret_ty;
    *str += "(";
    let mut first = true;
    for (name, &ty) in sig.names.iter().zip(&sig.param_tys) {
      if !first {
        *str += ", ";
      }
      *str += match name {
        Some(name) => &name.0,
        None => "...",
      };
      *str += ": ";
      self._show(chart, ty, str);
      first = false;
    }
    *str += ")";
    if !matches!(self.kind(ret), Some((_, TypeKind::Tuple(els))) if els.is_empty()) {
      *str += " -> ";
      self._show(chart, ret, str);
    };
  }

  pub fn import<T: TransferTypes>(&mut self, source: &TypeCtx<T>, params: Option<&[Type]>) -> T {
    self.import_with(source, params, |t, value| t.transfer(value))
  }

  pub fn import_with<T, U>(
    &mut self,
    source: &TypeCtx<T>,
    params: Option<&[Type]>,
    f: impl FnOnce(&mut TypeTransfer<'_>, &T) -> U,
  ) -> U {
    f(
      &mut TypeTransfer { mapping: HashMap::default(), source: &source.types, dest: self, params },
      &source.inner,
    )
  }

  pub fn export<T>(&self, f: impl FnOnce(&mut TypeTransfer<'_>) -> T) -> TypeCtx<T> {
    let mut types = Types::default();
    let mut export =
      TypeTransfer { mapping: HashMap::default(), source: self, dest: &mut types, params: None };
    export.dest.error = export.source.error.map(|t| export.transfer(&t));
    export.dest.nil = export.source.nil.map(|t| export.transfer(&t));
    let inner = f(&mut export);
    TypeCtx { types, inner }
  }

  pub fn get_mod(&self, chart: &Chart, ty: Type) -> Result<Option<DefId>, ErrorGuaranteed> {
    match self.kind(ty).as_ref() {
      Some((_, TypeKind::Opaque(id, _))) => Ok(Some(chart.opaque_types[*id].def)),
      Some((_, TypeKind::Struct(id, _))) => Ok(Some(chart.structs[*id].def)),
      Some((_, TypeKind::Enum(id, _))) => Ok(Some(chart.enums[*id].def)),
      Some((_, TypeKind::Ref(inner))) => self.get_mod(chart, *inner),
      Some((_, TypeKind::Key(..)))
      | Some((_, TypeKind::Fn(..)))
      | Some((_, TypeKind::Closure(..)))
      | Some((_, TypeKind::Param(..)))
      | Some((_, TypeKind::Tuple(..)))
      | Some((_, TypeKind::Object(..)))
      | Some((_, TypeKind::IfConst(..)))
      | Some((_, TypeKind::Never))
      | Some((_, TypeKind::Default))
      | None => Ok(None),
      Some((_, TypeKind::Error(err))) => Err(*err),
    }
  }

  pub fn reset(&mut self) {
    self.types.clear();
    self.error = None;
    self.nil = None;
  }

  pub fn finish_inference(&mut self) {
    for (_, node) in &mut self.types {
      if let TypeNode::Root { state, .. } = node
        && !matches!(state, TypeState::Known(..))
      {
        *state = TypeState::Known(Inverted(false), TypeKind::Default);
      }
    }
  }
}

#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnifyResult {
  Success,
  Failure,
  Indeterminate(ErrorGuaranteed),
}

use UnifyResult::*;

impl UnifyResult {
  pub fn and(self, other: Self) -> Self {
    match (self, other) {
      (Failure, _) | (_, Failure) => Failure,
      (Indeterminate(e), _) | (_, Indeterminate(e)) => Indeterminate(e),
      (Success, Success) => Success,
    }
  }

  pub fn all(i: impl IntoIterator<Item = Self>) -> Self {
    i.into_iter().fold(Success, Self::and)
  }

  pub fn from_bool(b: bool) -> Self {
    if b { Success } else { Failure }
  }

  pub fn is_success(self) -> bool {
    matches!(self, Success)
  }

  pub fn is_failure(self) -> bool {
    matches!(self, Failure)
  }
}

pub struct TypeTransfer<'ctx> {
  mapping: IntMap<TypeIdx, Type>,
  source: &'ctx Types,
  dest: &'ctx mut Types,
  params: Option<&'ctx [Type]>,
}

impl<'ctx> TypeTransfer<'ctx> {
  pub fn transfer<T: TransferTypes>(&mut self, t: &T) -> T {
    t.transfer(self)
  }

  fn transfer_type(&mut self, ty: Type) -> Type {
    let old = self.source.find(ty);
    match self.mapping.entry(old.idx()) {
      Entry::Occupied(e) => (*e.get()).invert_if(old.inv()),
      Entry::Vacant(e) => {
        let old_kind = self.source.kind(old.idx().into());
        if let Some(params) = self.params
          && let Some((inv, TypeKind::Param(p, _))) = old_kind
        {
          return params[*p].invert_if(inv ^ old.inv());
        }
        let new = self.dest.new_var(match self.source.types[old.idx()] {
          Root { state: Unknown(span), .. } => span,
          _ => Span::NONE,
        });
        e.insert(new);
        if let Some((inv, old_kind)) = old_kind {
          let kind = old_kind.map(|t| self.transfer(&t));
          let Root { state: new_kind, .. } = &mut self.dest.types[new.idx()] else {
            unreachable!()
          };
          *new_kind = Known(inv, kind)
        }
        new.invert_if(old.inv())
      }
    }
  }

  fn transfer_impl_type(&mut self, ty: &ImplType) -> ImplType {
    ty.map(|t| self.transfer(&t))
  }
}

impl TypeKind {
  fn map(&self, mut f: impl FnMut(Type) -> Type) -> Self {
    match self {
      TypeKind::Tuple(els) => TypeKind::Tuple(els.iter().copied().map(f).collect()),
      TypeKind::Object(els) => {
        TypeKind::Object(els.iter().map(|(k, &v)| (k.clone(), f(v))).collect())
      }
      TypeKind::Opaque(i, els) => TypeKind::Opaque(*i, els.iter().copied().map(f).collect()),
      TypeKind::Struct(i, els) => TypeKind::Struct(*i, els.iter().copied().map(f).collect()),
      TypeKind::Enum(i, els) => TypeKind::Enum(*i, els.iter().copied().map(f).collect()),
      TypeKind::Ref(t) => TypeKind::Ref(f(*t)),
      TypeKind::Key(k) => TypeKind::Key(k.clone()),
      TypeKind::Fn(i) => TypeKind::Fn(*i),
      TypeKind::Closure(i, x, s) => TypeKind::Closure(
        *i,
        *x,
        FnSig {
          names: s.names.clone(),
          param_tys: s.param_tys.iter().copied().map(&mut f).collect(),
          ret_ty: f(s.ret_ty),
        },
      ),
      TypeKind::Param(i, n) => TypeKind::Param(*i, n.clone()),
      TypeKind::IfConst(c, t, e) => TypeKind::IfConst(*c, f(*t), f(*e)),
      TypeKind::Never => TypeKind::Never,
      TypeKind::Default => TypeKind::Default,
      TypeKind::Error(err) => TypeKind::Error(*err),
    }
  }

  fn children(&self) -> impl Iterator<Item = Type> + '_ {
    multi_iter! { Children { Zero, One, Two, Vec, Object, Closure } }
    match self {
      TypeKind::Param(..)
      | TypeKind::Fn(_)
      | TypeKind::Never
      | TypeKind::Default
      | TypeKind::Error(_)
      | TypeKind::Key(_) => Children::Zero([]),
      TypeKind::Ref(t) => Children::One([*t]),
      TypeKind::IfConst(_, t, e) => Children::Two([*t, *e]),
      TypeKind::Tuple(els)
      | TypeKind::Opaque(_, els)
      | TypeKind::Struct(_, els)
      | TypeKind::Enum(_, els) => Children::Vec(els.iter().copied()),
      TypeKind::Object(els) => Children::Object(els.values().copied()),
      TypeKind::Closure(_, _, s) => {
        Children::Closure(s.param_tys.iter().copied().chain([s.ret_ty]))
      }
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

#[derive(Debug, Clone, Default)]
pub struct TypeCtx<T> {
  pub types: Types,
  pub inner: T,
}

pub trait TransferTypes {
  fn transfer(&self, t: &mut TypeTransfer<'_>) -> Self;
}

impl TransferTypes for Type {
  fn transfer(&self, t: &mut TypeTransfer<'_>) -> Self {
    t.transfer_type(*self)
  }
}

impl TransferTypes for ImplType {
  fn transfer(&self, t: &mut TypeTransfer<'_>) -> Self {
    t.transfer_impl_type(self)
  }
}

impl<T: TransferTypes> TransferTypes for Option<T> {
  fn transfer(&self, t: &mut TypeTransfer<'_>) -> Self {
    self.as_ref().map(|value| t.transfer(value))
  }
}

impl<T: TransferTypes> TransferTypes for Vec<T> {
  fn transfer(&self, t: &mut TypeTransfer<'_>) -> Self {
    self.iter().map(|value| t.transfer(value)).collect()
  }
}

impl<I: Idx, T: TransferTypes> TransferTypes for IdxVec<I, T> {
  fn transfer(&self, t: &mut TypeTransfer<'_>) -> Self {
    t.transfer(&self.vec).into()
  }
}
