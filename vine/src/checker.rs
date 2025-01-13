use std::{
  collections::BTreeMap,
  iter,
  mem::{replace, take},
  ops::Range,
};

use vine_util::{
  idx::{IdxVec, IntMap, RangeExt},
  new_idx,
};

use crate::{
  ast::{
    Block, Builtin, DynFnId, ExprKind, GenericPath, Ident, Key, LabelId, Local, Pat, PatKind, Span,
    StmtKind, Ty, TyKind,
  },
  core::Core,
  diag::{report, Diag, ErrorGuaranteed},
  resolver::{DefId, Resolver, TraitSubitem, TypeDef, ValueDefKind},
};

mod check_expr;
mod check_pat;
mod display_type;
mod typeof_def;
mod unify;

#[derive(Debug)]
pub struct Checker<'core, 'r> {
  core: &'core Core<'core>,
  resolver: &'r mut Resolver<'core>,
  state: CheckerState<'core>,
  generics: Vec<Ident<'core>>,
  return_ty: Option<Type<'core>>,
  labels: IdxVec<LabelId, Option<Type<'core>>>,
  impl_params: Vec<Type<'core>>,
  cur_def: DefId,

  bool: Option<DefId>,
  n32: Option<DefId>,
  f32: Option<DefId>,
  char: Option<DefId>,
  io: Option<DefId>,
  list: Option<DefId>,
  string: Option<DefId>,
  concat: Option<DefId>,
}

#[derive(Default, Debug, Clone)]
pub(crate) struct CheckerState<'core> {
  pub(crate) vars: IdxVec<Var, Result<Type<'core>, Span>>,
  pub(crate) locals: IntMap<Local, Var>,
  pub(crate) dyn_fns: IntMap<DynFnId, Type<'core>>,
}

impl<'core, 'r> Checker<'core, 'r> {
  pub fn new(core: &'core Core<'core>, resolver: &'r mut Resolver<'core>) -> Self {
    let bool = resolver.builtins.get(&Builtin::Bool).copied();
    let n32 = resolver.builtins.get(&Builtin::N32).copied();
    let f32 = resolver.builtins.get(&Builtin::F32).copied();
    let char = resolver.builtins.get(&Builtin::Char).copied();
    let io = resolver.builtins.get(&Builtin::IO).copied();
    define_primitive_type(resolver, bool, Type::Bool);
    define_primitive_type(resolver, n32, Type::N32);
    define_primitive_type(resolver, f32, Type::F32);
    define_primitive_type(resolver, char, Type::Char);
    define_primitive_type(resolver, io, Type::IO);
    let list = resolver.builtins.get(&Builtin::List).copied();
    let string = resolver.builtins.get(&Builtin::String).copied();
    let concat = resolver.builtins.get(&Builtin::Concat).copied();
    Checker {
      core,
      resolver,
      state: CheckerState::default(),
      generics: Vec::new(),
      impl_params: Vec::new(),
      return_ty: None,
      labels: Default::default(),
      cur_def: DefId::ROOT,
      bool,
      n32,
      f32,
      char,
      io,
      list,
      string,
      concat,
    }
  }

  pub fn check_defs(&mut self) {
    self._check_defs(self.resolver.defs.range());
  }

  pub(crate) fn _check_defs(&mut self, range: Range<DefId>) {
    for def in range.iter() {
      self.resolve_type_alias(def);
    }
    for def in range.iter() {
      self.resolve_trait_types(def);
    }
    for def in range.iter() {
      self.resolve_def_types(def);
    }
    for def in range.iter() {
      self.check_impl_def(def);
    }
    debug_assert!(self.state.vars.is_empty());
    for def in range.iter() {
      self.check_value_def(def);
    }
  }

  fn check_value_def(&mut self, def_id: DefId) {
    self.cur_def = def_id;
    debug_assert!(self.state.vars.is_empty() && self.state.locals.is_empty());
    let def = &mut self.resolver.defs[def_id];
    if let Some(value_def) = &mut def.value_def {
      if let ValueDefKind::Expr(expr) = &mut value_def.kind {
        self.impl_params.clone_from(value_def.impl_param_tys.as_ref().unwrap());
        self.generics.clone_from(&value_def.type_params);
        let mut ty = value_def.ty.clone().unwrap();
        let mut expr = take(expr);
        self.check_expr_form_type(&mut expr, Form::Value, &mut ty);
        let value_def = self.resolver.defs[def_id].value_def.as_mut().unwrap();
        value_def.kind = ValueDefKind::Expr(expr);
      }
    }
    self.state.vars.clear();
    self.state.locals.clear();
  }

  pub(crate) fn _check_custom(
    &mut self,
    def_id: DefId,
    state: CheckerState<'core>,
    block: &mut Block<'core>,
  ) -> (Type<'core>, CheckerState<'core>) {
    self.cur_def = def_id;
    self.state = state;
    let ty = self.check_block(block);
    (ty, take(&mut self.state))
  }

  fn new_var(&mut self, span: Span) -> Type<'core> {
    Type::Var(self.state.vars.push(Err(span)))
  }

  fn check_block(&mut self, block: &mut Block<'core>) -> Type<'core> {
    let mut ty = Type::UNIT;
    for stmt in block.stmts.iter_mut() {
      ty = Type::UNIT;
      match &mut stmt.kind {
        StmtKind::Let(l) => {
          let refutable = l.else_block.is_some();
          let mut let_ty = self.check_pat(&mut l.bind, Form::Value, refutable);
          if let Some(value) = &mut l.init {
            self.check_expr_form_type(value, Form::Value, &mut let_ty);
          }
          if let Some(block) = &mut l.else_block {
            self.check_block_type(block, &mut Type::Never);
          }
        }
        StmtKind::DynFn(d) => {
          let params = d.params.iter_mut().map(|p| self.check_pat(p, Form::Value, false)).collect();
          let mut ret = d
            .ret
            .as_mut()
            .map(|t| self.hydrate_type(t, true))
            .unwrap_or_else(|| self.new_var(d.body.span));
          let old = self.return_ty.replace(ret.clone());
          self.check_block_type(&mut d.body, &mut ret);
          self.return_ty = old;
          self.state.dyn_fns.insert(d.id.unwrap(), Type::Fn(params, Box::new(ret)));
        }
        StmtKind::Expr(e, semi) => {
          ty = self.check_expr_form(e, Form::Value);
          if *semi {
            ty = Type::UNIT;
          }
        }
        StmtKind::Item(_) | StmtKind::Empty => {}
      }
    }
    ty
  }

  fn check_block_type(&mut self, block: &mut Block<'core>, ty: &mut Type<'core>) {
    let mut found = self.check_block(block);
    if !self.unify(&mut found, ty) {
      self.core.report(Diag::ExpectedTypeFound {
        span: block.span,
        expected: self.display_type(ty),
        found: self.display_type(&found),
      });
    }
  }

  fn hydrate_type(&mut self, ty: &mut Ty<'core>, inference: bool) -> Type<'core> {
    let span = ty.span;
    match &mut ty.kind {
      TyKind::Hole if inference => self.new_var(span),
      TyKind::Paren(t) => self.hydrate_type(t, inference),
      TyKind::Hole => Type::Error(self.core.report(Diag::ItemTypeHole { span })),
      TyKind::Fn(args, ret) => Type::Fn(
        args.iter_mut().map(|arg| self.hydrate_type(arg, inference)).collect(),
        Box::new(ret.as_mut().map(|ret| self.hydrate_type(ret, inference)).unwrap_or(Type::UNIT)),
      ),
      TyKind::Tuple(tys) => {
        Type::Tuple(tys.iter_mut().map(|arg| self.hydrate_type(arg, inference)).collect())
      }
      TyKind::Object(entries) => {
        report!(self.core, ty.kind; self.build_object_type(entries, |self_, t| self_.hydrate_type(t, inference)))
      }
      TyKind::Ref(t) => Type::Ref(Box::new(self.hydrate_type(t, inference))),
      TyKind::Inverse(t) => Type::Inverse(Box::new(self.hydrate_type(t, inference))),
      TyKind::Path(path) => {
        report!(self.core, ty.kind; self.typeof_type_def(path, inference))
      }
      TyKind::Param(n) => Type::Opaque(*n),
      TyKind::Error(e) => Type::Error(*e),
    }
  }

  fn hydrate_param(&mut self, pat: &mut Pat<'core>) -> Type<'core> {
    let span = pat.span;
    match &mut pat.kind {
      PatKind::Paren(inner) => self.hydrate_param(inner),
      PatKind::Annotation(_, ty) => self.hydrate_type(ty, false),
      PatKind::Adt(path, _) => {
        report!(self.core, pat.kind; self.typeof_variant_def(path, false, false)).0
      }
      PatKind::Ref(p) => Type::Ref(Box::new(self.hydrate_param(p))),
      PatKind::Inverse(p) => Type::Inverse(Box::new(self.hydrate_param(p))),
      PatKind::Tuple(t) => Type::Tuple(t.iter_mut().map(|p| self.hydrate_param(p)).collect()),
      PatKind::Object(entries) => {
        report!(self.core, pat.kind; self.build_object_type(entries, Self::hydrate_param))
      }
      PatKind::Hole | PatKind::Local(_) | PatKind::Deref(_) => {
        Type::Error(self.core.report(Diag::ItemTypeHole { span }))
      }
      PatKind::Error(e) => Type::Error(*e),
    }
  }

  fn hydrate_generics(
    &mut self,
    path: &mut GenericPath<'core>,
    generic_count: usize,
    inference: bool,
  ) -> Vec<Type<'core>> {
    if let Some(generics) = &mut path.generics {
      if !generics.types.is_empty() {
        return generics
          .types
          .iter_mut()
          .map(|t| self.hydrate_type(t, inference))
          .collect::<Vec<_>>();
      }
    }
    iter::from_fn(|| Some(self.new_var(path.span))).take(generic_count).collect()
  }

  fn resolve_type_alias(&mut self, def_id: DefId) {
    let old = replace(&mut self.cur_def, def_id);
    if let Some(type_def) = &mut self.resolver.defs[def_id].type_def {
      if let Some(mut alias) = type_def.alias.take() {
        let ty = self.hydrate_type(&mut alias, false);
        self.resolver.defs[def_id].type_def.as_mut().unwrap().ty = Some(ty);
      }
    }
    self.cur_def = old;
  }

  fn resolve_trait_types(&mut self, def_id: DefId) {
    self.cur_def = def_id;
    if let Some(mut trait_def) = self.resolver.defs[def_id].trait_def.take() {
      for (_, sub_id, subitem, ty_out) in trait_def.subitems.iter_mut() {
        let ty = match subitem {
          TraitSubitem::Fn(pats, ret) => Type::Fn(
            pats.iter_mut().map(|pat| self.hydrate_param(pat)).collect(),
            Box::new(ret.as_mut().map(|r| self.hydrate_type(r, false)).unwrap_or(Type::UNIT)),
          ),
          TraitSubitem::Const(ty) => self.hydrate_type(ty, false),
        };
        *ty_out = Some(ty.clone());
        self.resolver.defs[*sub_id].value_def.as_mut().unwrap().ty = Some(ty);
      }
      self.resolver.defs[def_id].trait_def = Some(trait_def);
    }
  }

  fn resolve_def_types(&mut self, def_id: DefId) {
    self.cur_def = def_id;
    if let Some(mut variant) = self.resolver.defs[def_id].variant_def.take() {
      variant.field_types =
        Some(variant.fields.iter_mut().map(|ty| self.hydrate_type(ty, false)).collect());
      self.resolver.defs[def_id].variant_def = Some(variant);
    }
    if let Some(mut value) = self.resolver.defs[def_id].value_def.take() {
      if value.impl_param_tys.is_none() {
        value.impl_param_tys = Some(
          value
            .impl_params
            .iter_mut()
            .map(|(_, trait_)| {
              trait_
                .path
                .resolved
                .map(|trait_id| {
                  let trait_def = self.resolver.defs[trait_id].trait_def.take().unwrap();
                  let trait_args = self.hydrate_generics(trait_, trait_def.generics, false);
                  self.resolver.defs[trait_id].trait_def = Some(trait_def);
                  Type::Adt(trait_id, trait_args)
                })
                .unwrap_or(Type::Error(ErrorGuaranteed::new_unchecked()))
            })
            .collect(),
        );
      }
      let ty = if let Some(ty) = value.ty.take() {
        ty
      } else if let Some(ty) = &mut value.annotation {
        self.hydrate_type(ty, false)
      } else {
        match &mut value.kind {
          ValueDefKind::Expr(e) => {
            let ExprKind::Fn(args, Some(ret), _) = &mut e.kind else { unreachable!() };
            Type::Fn(
              args.iter_mut().map(|pat| self.hydrate_param(pat)).collect(),
              Box::new(ret.as_mut().map(|r| self.hydrate_type(r, false)).unwrap_or(Type::UNIT)),
            )
          }
          ValueDefKind::Ivy(_) => unreachable!(),
          ValueDefKind::AdtConstructor => {
            let variant = self.resolver.defs[def_id].variant_def.as_ref().unwrap();
            let params = variant.field_types.clone().unwrap();
            let adt =
              Type::Adt(variant.adt, (0..variant.type_params.len()).map(Type::Opaque).collect());
            if params.is_empty() {
              adt
            } else {
              Type::Fn(params, Box::new(adt))
            }
          }
          ValueDefKind::TraitSubitem(..) => unreachable!(),
        }
      };
      value.ty = Some(ty);
      self.resolver.defs[def_id].value_def = Some(value);
    }
    if let Some(mut impl_def) = self.resolver.defs[def_id].impl_def.take() {
      impl_def.impl_param_tys = Some(
        impl_def
          .impl_params
          .iter_mut()
          .map(|(_, trait_)| {
            trait_
              .path
              .resolved
              .map(|trait_id| {
                let trait_def = self.resolver.defs[trait_id].trait_def.take().unwrap();
                let trait_args = self.hydrate_generics(trait_, trait_def.generics, false);
                self.resolver.defs[trait_id].trait_def = Some(trait_def);
                Type::Adt(trait_id, trait_args)
              })
              .unwrap_or(Type::Error(ErrorGuaranteed::new_unchecked()))
          })
          .collect(),
      );
      if let Some(trait_id) = impl_def.trait_.path.resolved {
        let trait_def = self.resolver.defs[trait_id].trait_def.take().unwrap();
        let trait_args = self.hydrate_generics(&mut impl_def.trait_, trait_def.generics, false);
        impl_def.trait_ty = Some((trait_id, trait_args));
        self.resolver.defs[trait_id].trait_def = Some(trait_def);
      }
      self.resolver.defs[def_id].impl_def = Some(impl_def);
    }
  }

  fn check_impl_def(&mut self, def_id: DefId) {
    if let Some(impl_def) = self.resolver.defs[def_id].impl_def.take() {
      self.generics.clone_from(&impl_def.type_params);
      if let Some((trait_id, trait_args)) = &impl_def.trait_ty {
        let trait_def = self.resolver.defs[*trait_id].trait_def.take().unwrap();
        for &(span, name, sub_id) in &impl_def.subitems {
          if let Some(subitem) = trait_def.subitems.iter().find(|x| x.0 == name) {
            let mut value_def = self.resolver.defs[sub_id].value_def.take().unwrap();
            let ty = value_def.ty.as_mut().unwrap();
            let mut expected = subitem.3.as_ref().unwrap().instantiate(trait_args);
            if !self.unify(ty, &mut expected) {
              self.core.report(Diag::ExpectedTypeFound {
                span,
                expected: self.display_type(&expected),
                found: self.display_type(ty),
              });
            }
            self.resolver.defs[sub_id].value_def = Some(value_def);
          } else {
            self.core.report(Diag::ExtraneousImplItem { span, name });
          }
        }
        if trait_def.subitems.len() > impl_def.subitems.len() {
          self.core.report(Diag::IncompleteImpl { span: impl_def.span });
        }
        self.resolver.defs[*trait_id].trait_def = Some(trait_def);
      }
      self.resolver.defs[def_id].impl_def = Some(impl_def);
    }
  }

  fn build_object_type<T>(
    &mut self,
    entries: &mut Vec<(Key<'core>, T)>,
    mut f: impl FnMut(&mut Self, &mut T) -> Type<'core>,
  ) -> Result<Type<'core>, Diag<'core>> {
    let mut fields = BTreeMap::new();
    let mut duplicate = Ok(());
    for (key, value) in entries {
      let old = fields.insert(key.ident, f(self, value));
      if old.is_some() {
        duplicate = Err(self.core.report(Diag::DuplicateKey { span: key.span }));
      }
    }
    duplicate?;
    Ok(Type::Object(fields))
  }
}

fn define_primitive_type<'core>(
  resolver: &mut Resolver<'core>,
  def_id: Option<DefId>,
  ty: Type<'core>,
) {
  if let Some(def_id) = def_id {
    resolver.defs[def_id].type_def =
      Some(TypeDef { vis: DefId::ROOT, type_params: Vec::new(), alias: None, ty: Some(ty) });
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Form {
  Value,
  Place,
  Space,
  Error(ErrorGuaranteed),
}

impl Form {
  fn inverse(self) -> Self {
    match self {
      Form::Value => Form::Space,
      Form::Place => Form::Place,
      Form::Space => Form::Value,
      Form::Error(e) => Form::Error(e),
    }
  }
}

new_idx!(pub Var);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<'core> {
  Bool,
  N32,
  F32,
  Char,
  IO,
  Tuple(Vec<Type<'core>>),
  Object(BTreeMap<Ident<'core>, Type<'core>>),
  Fn(Vec<Type<'core>>, Box<Type<'core>>),
  Ref(Box<Type<'core>>),
  Inverse(Box<Type<'core>>),
  Adt(DefId, Vec<Type<'core>>),
  Opaque(usize),
  Var(Var),
  Never,
  Error(ErrorGuaranteed),
}

impl<'core> Type<'core> {
  const UNIT: Type<'static> = Type::Tuple(Vec::new());

  pub(crate) fn instantiate(&self, opaque: &[Type<'core>]) -> Type<'core> {
    match self {
      Type::Bool => Type::Bool,
      Type::N32 => Type::N32,
      Type::F32 => Type::F32,
      Type::Char => Type::Char,
      Type::IO => Type::IO,
      Type::Tuple(tys) => Type::Tuple(tys.iter().map(|t| t.instantiate(opaque)).collect()),
      Type::Object(entries) => {
        Type::Object(entries.iter().map(|(&k, t)| (k, t.instantiate(opaque))).collect())
      }
      Type::Fn(tys, ret) => Type::Fn(
        tys.iter().map(|t| t.instantiate(opaque)).collect(),
        Box::new(ret.instantiate(opaque)),
      ),
      Type::Ref(t) => Type::Ref(Box::new(t.instantiate(opaque))),
      Type::Inverse(t) => Type::Inverse(Box::new(t.instantiate(opaque))),
      Type::Adt(def, tys) => Type::Adt(*def, tys.iter().map(|t| t.instantiate(opaque)).collect()),
      Type::Opaque(n) => opaque[*n].clone(),
      Type::Error(e) => Type::Error(*e),
      Type::Never => Type::Never,
      Type::Var(_) => unreachable!(),
    }
  }

  fn inverse(self) -> Self {
    match self {
      Type::Inverse(t) => *t,
      _ => Type::Inverse(Box::new(self)),
    }
  }

  fn invert_if(self, invert: bool) -> Self {
    if invert {
      self.inverse()
    } else {
      self
    }
  }
}

impl Default for Type<'_> {
  fn default() -> Self {
    Self::UNIT
  }
}

impl From<ErrorGuaranteed> for Type<'_> {
  fn from(value: ErrorGuaranteed) -> Self {
    Type::Error(value)
  }
}
