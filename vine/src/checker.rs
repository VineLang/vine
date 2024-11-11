use std::{
  collections::HashMap,
  iter,
  mem::{replace, take},
  ops::Range,
};

use crate::{
  ast::{Block, Builtin, ExprKind, GenericPath, Ident, Span, StmtKind, Ty, TyKind},
  diag::{report, Diag, DiagGroup, ErrorGuaranteed},
  resolve::{DefId, Resolver, TypeDef, ValueDefKind},
};

mod check_expr;
mod check_pat;
mod display_type;
mod typeof_def;
mod unify;

#[derive(Debug)]
pub struct Checker<'r> {
  pub diags: DiagGroup,
  resolver: &'r mut Resolver,
  state: CheckerState,
  generics: Vec<Ident>,
  return_ty: Option<Type>,
  loop_ty: Option<Type>,
  cur_def: DefId,

  u32: Option<DefId>,
  f32: Option<DefId>,
  io: Option<DefId>,
  list: Option<DefId>,
  string: Option<Type>,
  concat: Option<DefId>,
}

#[derive(Default, Debug, Clone)]
pub(crate) struct CheckerState {
  pub(crate) vars: Vec<Result<Type, Span>>,
  pub(crate) locals: HashMap<usize, Var>,
  pub(crate) dyn_fns: HashMap<usize, Type>,
}

impl<'r> Checker<'r> {
  pub fn new(resolver: &'r mut Resolver) -> Self {
    let diags = DiagGroup::default();
    let u32 = resolver.builtins.get(&Builtin::U32).copied();
    let f32 = resolver.builtins.get(&Builtin::F32).copied();
    let io = resolver.builtins.get(&Builtin::IO).copied();
    define_primitive_type(resolver, u32, Type::U32);
    define_primitive_type(resolver, f32, Type::F32);
    define_primitive_type(resolver, io, Type::IO);
    let list = resolver.builtins.get(&Builtin::List).copied();
    let string = list.map(|x| Type::Adt(x, vec![Type::U32]));
    let concat = resolver.builtins.get(&Builtin::Concat).copied();
    Checker {
      diags,
      resolver,
      state: CheckerState::default(),
      generics: Vec::new(),
      return_ty: None,
      loop_ty: None,
      cur_def: 0,
      u32,
      f32,
      io,
      list,
      string,
      concat,
    }
  }

  pub fn check_defs(&mut self) {
    self._check_defs(0..self.resolver.defs.len());
  }

  pub(crate) fn _check_defs(&mut self, range: Range<DefId>) {
    for def in range.clone() {
      self.resolve_type_alias(def);
    }
    for def in range.clone() {
      self.resolve_def_types(def);
    }
    debug_assert!(self.state.vars.is_empty());
    for def in range.clone() {
      self.check_value_def(def);
    }
  }

  fn check_value_def(&mut self, def_id: DefId) {
    self.cur_def = def_id;
    debug_assert!(self.state.vars.is_empty() && self.state.locals.is_empty());
    let def = &mut self.resolver.defs[def_id];
    if let Some(value_def) = &mut def.value_def {
      if let ValueDefKind::Expr(expr) = &mut value_def.kind {
        self.generics.clone_from(&value_def.generics);
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

  pub(crate) fn _check_custom(&mut self, state: CheckerState, block: &mut Block) -> CheckerState {
    self.state = state;
    self.check_block(block);
    take(&mut self.state)
  }

  fn new_var(&mut self, span: Span) -> Type {
    let v = self.state.vars.len();
    self.state.vars.push(Err(span));
    Type::Var(v)
  }

  fn check_block(&mut self, block: &mut Block) -> Type {
    let mut ty = Type::UNIT;
    for stmt in block.stmts.iter_mut() {
      match &mut stmt.kind {
        StmtKind::Let(l) => {
          let mut ty = self.check_pat_annotation(&mut l.bind, l.ty.as_mut(), Form::Value, false);
          if let Some(value) = &mut l.init {
            self.check_expr_form_type(value, Form::Value, &mut ty);
          }
        }
        StmtKind::DynFn(d) => {
          let params = d
            .params
            .iter_mut()
            .map(|(p, a)| self.check_pat_annotation(p, a.as_mut(), Form::Value, false))
            .collect();
          let mut ret = d
            .ret
            .as_mut()
            .map(|t| self.hydrate_type(t, true))
            .unwrap_or_else(|| self.new_var(d.body.span));
          self.check_block_type(&mut d.body, &mut ret);
          self.state.dyn_fns.insert(d.dyn_id.unwrap(), Type::Fn(params, Box::new(ret)));
          ty = Type::UNIT;
        }
        StmtKind::Expr(e, semi) => {
          ty = self.check_expr_form(e, Form::Value);
          if *semi {
            ty = Type::UNIT;
          }
        }
        StmtKind::Item(_) | StmtKind::Empty => ty = Type::UNIT,
      }
    }
    ty
  }

  fn check_block_type(&mut self, block: &mut Block, ty: &mut Type) {
    let mut found = self.check_block(block);
    if !self.unify(&mut found, ty) {
      self.diags.add(Diag::ExpectedTypeFound {
        span: block.span,
        expected: self.display_type(ty),
        found: self.display_type(&found),
      });
    }
  }

  fn hydrate_type(&mut self, ty: &mut Ty, inference: bool) -> Type {
    let span = ty.span;
    match &mut ty.kind {
      TyKind::Hole if inference => self.new_var(span),
      TyKind::Paren(t) => self.hydrate_type(t, inference),
      TyKind::Hole => Type::Error(self.diags.add(Diag::ItemTypeHole { span })),
      TyKind::Fn(args, ret) => Type::Fn(
        args.iter_mut().map(|arg| self.hydrate_type(arg, inference)).collect(),
        Box::new(ret.as_mut().map(|ret| self.hydrate_type(ret, inference)).unwrap_or(Type::UNIT)),
      ),
      TyKind::Tuple(tys) => {
        Type::Tuple(tys.iter_mut().map(|arg| self.hydrate_type(arg, inference)).collect())
      }
      TyKind::Ref(t) => Type::Ref(Box::new(self.hydrate_type(t, inference))),
      TyKind::Inverse(t) => Type::Inverse(Box::new(self.hydrate_type(t, inference))),
      TyKind::Path(path) => {
        report!(self.diags, ty.kind; self.typeof_type_def(path, inference))
      }
      TyKind::Generic(n) => Type::Opaque(*n),
      TyKind::Error(e) => Type::Error(*e),
    }
  }

  fn hydrate_generics(&mut self, path: &mut GenericPath, generic_count: usize) -> Vec<Type> {
    if let Some(generics) = &mut path.generics {
      generics.iter_mut().map(|t| self.hydrate_type(t, true)).collect::<Vec<_>>()
    } else {
      iter::from_fn(|| Some(self.new_var(path.span))).take(generic_count).collect()
    }
  }

  fn resolve_type_alias(&mut self, def_id: usize) {
    let old = replace(&mut self.cur_def, def_id);
    if let Some(type_def) = &mut self.resolver.defs[def_id].type_def {
      if let Some(mut alias) = type_def.alias.take() {
        let ty = self.hydrate_type(&mut alias, false);
        self.resolver.defs[def_id].type_def.as_mut().unwrap().ty = Some(ty);
      }
    }
    self.cur_def = old;
  }

  fn resolve_def_types(&mut self, def_id: usize) {
    self.cur_def = def_id;
    if let Some(mut variant) = self.resolver.defs[def_id].variant_def.take() {
      variant.field_types =
        Some(variant.fields.iter_mut().map(|ty| self.hydrate_type(ty, false)).collect());
      self.resolver.defs[def_id].variant_def = Some(variant);
    }
    if let Some(mut value) = self.resolver.defs[def_id].value_def.take() {
      let ty = if let Some(ty) = &mut value.annotation {
        self.hydrate_type(ty, false)
      } else {
        match &mut value.kind {
          ValueDefKind::Expr(e) => {
            let ExprKind::Fn(args, Some(ret), _) = &mut e.kind else { unreachable!() };
            Type::Fn(
              args
                .iter_mut()
                .map(|(pat, ty)| match ty {
                  Some(ty) => self.hydrate_type(ty, false),
                  None => Type::Error(self.diags.add(Diag::FnItemUntypedParam { span: pat.span })),
                })
                .collect(),
              Box::new(ret.as_mut().map(|r| self.hydrate_type(r, false)).unwrap_or(Type::UNIT)),
            )
          }
          ValueDefKind::Ivy(_) => unreachable!(),
          ValueDefKind::AdtConstructor => {
            let variant = self.resolver.defs[def_id].variant_def.as_ref().unwrap();
            let params = variant.field_types.clone().unwrap();
            let adt =
              Type::Adt(variant.adt, (0..variant.generics.len()).map(Type::Opaque).collect());
            if params.is_empty() {
              adt
            } else {
              Type::Fn(params, Box::new(adt))
            }
          }
        }
      };
      value.ty = Some(ty);
      self.resolver.defs[def_id].value_def = Some(value);
    }
  }
}

fn define_primitive_type(resolver: &mut Resolver, def_id: Option<DefId>, ty: Type) {
  if let Some(def_id) = def_id {
    resolver.defs[def_id].type_def =
      Some(TypeDef { vis: 0, generics: Vec::new(), alias: None, ty: Some(ty) });
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

type Var = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
  U32,
  F32,
  IO,
  Tuple(Vec<Type>),
  Fn(Vec<Type>, Box<Type>),
  Ref(Box<Type>),
  Inverse(Box<Type>),
  Adt(DefId, Vec<Type>),
  Opaque(usize),
  Var(Var),
  Error(ErrorGuaranteed),
}

impl Type {
  const UNIT: Type = Type::Tuple(Vec::new());

  fn instantiate(&self, opaque: &[Type]) -> Type {
    match self {
      Type::U32 => Type::U32,
      Type::F32 => Type::F32,
      Type::IO => Type::IO,
      Type::Tuple(tys) => Type::Tuple(tys.iter().map(|t| t.instantiate(opaque)).collect()),
      Type::Fn(tys, ret) => Type::Fn(
        tys.iter().map(|t| t.instantiate(opaque)).collect(),
        Box::new(ret.instantiate(opaque)),
      ),
      Type::Ref(t) => Type::Ref(Box::new(t.instantiate(opaque))),
      Type::Inverse(t) => Type::Inverse(Box::new(t.instantiate(opaque))),
      Type::Adt(def, tys) => Type::Adt(*def, tys.iter().map(|t| t.instantiate(opaque)).collect()),
      Type::Opaque(n) => opaque[*n].clone(),
      Type::Error(e) => Type::Error(*e),
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

impl Default for Type {
  fn default() -> Self {
    Self::UNIT
  }
}

impl From<ErrorGuaranteed> for Type {
  fn from(value: ErrorGuaranteed) -> Self {
    Type::Error(value)
  }
}
