use std::{collections::HashMap, iter, mem::take, ops::Range};

use vine_util::interner::StringInterner;

use crate::{
  ast::{
    BinaryOp, Block, Expr, ExprKind, GenericPath, Ident, Pat, PatKind, Path, Span, StmtKind, Ty,
    TyKind,
  },
  diag::{Diag, DiagGroup, ErrorGuaranteed},
  resolve::{Def, DefId, Resolver, ValueDefKind},
};

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

impl Type {
  const UNIT: Type = Type::Tuple(Vec::new());
}

#[derive(Debug)]
pub struct Checker<'d> {
  pub diags: DiagGroup,
  defs: &'d mut [Def],
  state: CheckerState,
  list: Option<DefId>,
  string: Option<Type>,
  generics: Vec<Ident>,
  return_ty: Option<Type>,
  loop_ty: Option<Type>,
}

#[derive(Default, Debug, Clone)]
pub(crate) struct CheckerState {
  pub(crate) vars: Vec<Result<Type, Span>>,
  pub(crate) locals: HashMap<usize, Var>,
}

impl<'d> Checker<'d> {
  pub fn new(resolver: &'d mut Resolver, interner: &StringInterner<'static>) -> Self {
    let diags = DiagGroup::default();
    let list = resolver
      .resolve_path(
        0,
        &Path {
          span: Span::NONE,
          segments: vec![
            Ident(interner.intern("std")),
            Ident(interner.intern("list")),
            Ident(interner.intern("List")),
          ],
          absolute: true,
          resolved: None,
        },
      )
      .ok();
    let string = list.map(|x| Type::Adt(x, vec![Type::U32]));
    Checker {
      diags,
      defs: &mut resolver.defs,
      state: CheckerState::default(),
      list,
      string,
      generics: Vec::new(),
      return_ty: None,
      loop_ty: None,
    }
  }

  pub fn check_items(&mut self) {
    self._check_items(0..self.defs.len());
  }

  pub(crate) fn _check_items(&mut self, range: Range<DefId>) {
    for def in range.clone() {
      self.resolve_type_alias(def);
    }
    for def in range.clone() {
      self.populate_def_tys(def);
    }
    debug_assert!(self.state.vars.is_empty());
    for def in range.clone() {
      self.check_def(def);
    }
  }

  fn check_def(&mut self, def_id: DefId) {
    debug_assert!(self.state.vars.is_empty() && self.state.locals.is_empty());
    let def = &mut self.defs[def_id];
    if let Some(value_def) = &mut def.value_def {
      if let ValueDefKind::Expr(expr) = &mut value_def.kind {
        self.generics = take(&mut value_def.generics);
        let mut ty = value_def.ty.clone().unwrap();
        let mut expr = take(expr);
        self.expect_expr_form_ty(&mut expr, Form::Value, &mut ty);
        let value_def = self.defs[def_id].value_def.as_mut().unwrap();
        value_def.kind = ValueDefKind::Expr(expr);
        value_def.generics = take(&mut self.generics);
      }
    }
    self.state.vars.clear();
    self.state.locals.clear();
  }

  pub(crate) fn _check_block(&mut self, state: CheckerState, block: &mut Block) -> CheckerState {
    self.state = state;
    self.infer_block(block);
    take(&mut self.state)
  }

  fn new_var(&mut self, span: Span) -> Type {
    let v = self.state.vars.len();
    self.state.vars.push(Err(span));
    Type::Var(v)
  }

  fn unify(&mut self, a: &mut Type, b: &mut Type) -> bool {
    self._unify(a, b, false, false)
  }

  fn _unify(&mut self, a: &mut Type, b: &mut Type, i: bool, j: bool) -> bool {
    match (&mut *a, &mut *b) {
      (Type::Error(_), _) | (_, Type::Error(_)) => true,
      (Type::Inverse(a), Type::Inverse(b)) => self._unify(a, b, !i, !j),
      (a, Type::Inverse(b)) => self._unify(a, b, i, !j),
      (Type::Inverse(a), b) => self._unify(a, b, !i, j),
      (Type::Var(v), Type::Var(u)) if *v == *u => i == j,
      (Type::Var(v), Type::Var(u)) => {
        let (v, u) = get2_mut(&mut self.state.vars, *v, *u);
        match (&mut *v, &mut *u) {
          (Ok(v), Ok(u)) => {
            *a = v.clone();
            *b = u.clone();
            self._unify(a, b, i, j)
          }
          (Ok(v), Err(_)) => {
            *a = v.clone();
            self._unify(a, b, i, j)
          }
          (Err(_), Ok(u)) => {
            *b = u.clone();
            self._unify(a, b, i, j)
          }
          (Err(_), Err(_)) => {
            *v = Ok(b.clone().invert_if(i != j));
            *a = b.clone().invert_if(i != j);
            true
          }
        }
      }
      (&mut ref mut o @ Type::Var(v), t) | (t, &mut ref mut o @ Type::Var(v)) => {
        let v = &mut self.state.vars[v];
        if let Ok(u) = v {
          *o = u.clone();
          self._unify(a, b, i, j)
        } else {
          *o = t.clone();
          *v = Ok(t.clone().invert_if(i != j));
          true
        }
      }
      (Type::U32, Type::U32) | (Type::F32, Type::F32) | (Type::IO, Type::IO) if i == j => true,
      (Type::Opaque(a), Type::Opaque(b)) if a == b && i == j => true,
      (Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => {
        let mut success = true;
        for (a, b) in a.iter_mut().zip(b) {
          success &= self._unify(a, b, i, j);
        }
        success
      }
      (Type::Ref(a), Type::Ref(b)) if i == j => self._unify(a, b, i, j),
      (Type::Fn(x, a), Type::Fn(y, b)) if i == j => {
        let mut success = true;
        for (x, y) in x.iter_mut().zip(y) {
          success &= self._unify(x, y, i, j);
        }
        success &= self._unify(a, b, i, j);
        success
      }
      (Type::Adt(n, a), Type::Adt(m, b)) if n == m && i == j => {
        let mut success = true;
        for (a, b) in a.iter_mut().zip(b) {
          success &= self._unify(a, b, i, j);
        }
        success
      }
      _ => false,
    }
  }

  fn infer_expr(&mut self, expr: &mut Expr) -> (Form, Type) {
    let span = expr.span;
    match &mut expr.kind {
      ExprKind![synthetic] => unreachable!(),
      ExprKind![!error && !space && !place && !synthetic] => {
        (Form::Value, self._infer_expr_value(expr))
      }
      ExprKind::Error(e) => (Form::Error(*e), Type::Error(*e)),
      ExprKind::Hole => (Form::Space, self.new_var(span)),
      ExprKind::Local(l) => (
        Form::Place,
        Type::Var(*self.state.locals.entry(*l).or_insert_with(|| {
          let v = self.state.vars.len();
          self.state.vars.push(Err(span));
          v
        })),
      ),
      ExprKind::Deref(_) => todo!(),
      ExprKind::Inverse(expr) => {
        let (form, ty) = self.infer_expr(expr);
        (form.inverse(), ty.inverse())
      }
      ExprKind::Tuple(v) => {
        if v.is_empty() {
          (Form::Place, Type::UNIT)
        } else {
          let (forms, types) =
            v.iter_mut().map(|x| self.infer_expr(x)).collect::<(Vec<_>, Vec<_>)>();
          let form = if let Some(&one_form) = forms.iter().find(|x| !matches!(x, Form::Error(_))) {
            if forms.iter().all(|&x| matches!(x, Form::Error(_)) || x == one_form) {
              one_form
            } else {
              for (form, e) in forms.into_iter().zip(v) {
                self.coerce_expr(e, form, Form::Place);
              }
              Form::Place
            }
          } else {
            forms[0]
          };
          (form, Type::Tuple(types))
        }
      }
      ExprKind::Field(..) => todo!(),
    }
  }

  fn _infer_expr_value(&mut self, expr: &mut Expr) -> Type {
    let span = expr.span;
    match &mut expr.kind {
      ExprKind![error || place || space || synthetic] => unreachable!(),
      ExprKind::Path(path) => self.infer_path_value(path),
      ExprKind::Block(block) => self.infer_block(block),
      ExprKind::Assign(space, value) => {
        let mut ty = self.expect_expr_form(space, Form::Space);
        self.expect_expr_form_ty(value, Form::Value, &mut ty);
        Type::UNIT
      }
      ExprKind::Match(scrutinee, arms) => {
        let mut scrutinee = self.expect_expr_form(scrutinee, Form::Value);
        let mut result = self.new_var(span);
        for (pat, expr) in arms {
          self.expect_pat_form_ty(pat, Form::Value, true, &mut scrutinee);
          self.expect_expr_form_ty(expr, Form::Value, &mut result);
        }
        result
      }
      ExprKind::If(cond, then, els) => {
        self.expect_expr_form_ty(cond, Form::Value, &mut Type::U32);
        let mut then = self.infer_block(then);
        let mut els = self.expect_expr_form(els, Form::Value);
        if !self.unify(&mut then, &mut els) {
          Type::Error(self.diags.add(Diag::MismatchedThenElseTypes {
            span,
            then: self.print_ty(&then),
            els: self.print_ty(&els),
          }))
        } else {
          then
        }
      }
      ExprKind::While(cond, block) => {
        let old = self.loop_ty.replace(Type::UNIT);
        self.expect_expr_form_ty(cond, Form::Value, &mut Type::U32);
        self.infer_block(block);
        self.loop_ty = old;
        Type::UNIT
      }
      ExprKind::Loop(block) => {
        let result = self.new_var(span);
        let old = self.loop_ty.replace(result.clone());
        self.infer_block(block);
        self.loop_ty = old;
        result
      }
      ExprKind::Fn(args, ret, body) => Type::Fn(
        args
          .iter_mut()
          .map(|(pat, ty)| self.do_pat(pat, ty.as_mut(), Form::Value, false))
          .collect(),
        Box::new({
          let mut ret = match ret {
            Some(Some(t)) => self.hydrate_type(t, true),
            Some(None) => Type::UNIT,
            None => self.new_var(span),
          };
          let old = self.return_ty.replace(ret.clone());
          self.expect_expr_form_ty(body, Form::Value, &mut ret);
          self.return_ty = old;
          ret
        }),
      ),
      ExprKind::Break(e) => {
        if let Some(ty) = &self.loop_ty {
          let mut ty = ty.clone();
          if let Some(e) = e {
            self.expect_expr_form_ty(e, Form::Value, &mut ty);
          } else if !self.unify(&mut ty, &mut { Type::UNIT }) {
            self.diags.add(Diag::MissingBreakExpr { span, ty: self.print_ty(&ty) });
          }
        } else {
          self.diags.add(Diag::NoLoopBreak { span });
        }
        self.new_var(span)
      }
      ExprKind::Return(e) => {
        if let Some(ty) = &self.return_ty {
          let mut ty = ty.clone();
          if let Some(e) = e {
            self.expect_expr_form_ty(e, Form::Value, &mut ty);
          } else if !self.unify(&mut ty, &mut { Type::UNIT }) {
            self.diags.add(Diag::MissingReturnExpr { span, ty: self.print_ty(&ty) });
          }
        } else {
          self.diags.add(Diag::NoReturn { span });
        }
        self.new_var(span)
      }
      ExprKind::Continue => {
        if self.loop_ty.is_none() {
          self.diags.add(Diag::NoLoopContinue { span });
        }
        self.new_var(span)
      }
      ExprKind::Ref(expr) => {
        let ty = self.expect_expr_form(expr, Form::Place);
        Type::Ref(Box::new(ty))
      }
      ExprKind::Move(expr) => self.expect_expr_form(expr, Form::Place),
      ExprKind::List(els) => {
        let mut item = self.new_var(span);
        for el in els {
          self.expect_expr_form_ty(el, Form::Value, &mut item);
        }
        if let Some(list) = self.list {
          Type::Adt(list, vec![item])
        } else {
          Type::Error(self.diags.add(Diag::NoList { span }))
        }
      }
      ExprKind::Method(receiver, path, args) => {
        let mut ty = self.infer_path_value(path);
        match &mut ty {
          Type::Fn(params, ret) => match params.first_mut() {
            Some(Type::Ref(rec)) => {
              self.expect_expr_form_ty(receiver, Form::Place, rec);
              let params = &mut params[1..];
              if params.len() != args.len() {
                Type::Error(self.diags.add(Diag::BadArgCount {
                  span,
                  expected: params.len(),
                  got: args.len(),
                  ty: self.print_ty(&ty),
                }))
              } else {
                for (ty, arg) in params.iter_mut().zip(args) {
                  self.expect_expr_form_ty(arg, Form::Value, ty);
                }
                take(&mut **ret)
              }
            }
            Some(Type::Error(e)) => Type::Error(*e),
            _ => {
              Type::Error(self.diags.add(Diag::NonMethodFunction { span, ty: self.print_ty(&ty) }))
            }
          },
          Type::Error(e) => Type::Error(*e),
          ty => Type::Error(self.diags.add(Diag::NonFunctionCall { span, ty: self.print_ty(ty) })),
        }
      }
      ExprKind::Call(func, args) => {
        let mut ty = self.expect_expr_form(func, Form::Value);
        self.concretize(&mut ty);
        match &mut ty {
          Type::Fn(params, ret) => {
            if params.len() != args.len() {
              Type::Error(self.diags.add(Diag::BadArgCount {
                span,
                expected: params.len(),
                got: args.len(),
                ty: self.print_ty(&ty),
              }))
            } else {
              for (ty, arg) in params.iter_mut().zip(args) {
                self.expect_expr_form_ty(arg, Form::Value, ty);
              }
              take(&mut **ret)
            }
          }
          Type::Error(e) => Type::Error(*e),
          ty => Type::Error(self.diags.add(Diag::NonFunctionCall { span, ty: self.print_ty(ty) })),
        }
      }

      ExprKind::Not(expr) => {
        self.expect_expr_form_ty(expr, Form::Value, &mut Type::U32);
        Type::U32
      }
      ExprKind::Is(expr, pat) => {
        let mut ty = self.expect_expr_form(expr, Form::Value);
        self.expect_pat_form_ty(pat, Form::Value, true, &mut ty);
        Type::U32
      }
      ExprKind::LogicalOp(_, a, b) => {
        self.expect_expr_form_ty(a, Form::Value, &mut Type::U32);
        self.expect_expr_form_ty(b, Form::Value, &mut Type::U32);
        Type::U32
      }
      ExprKind::Neg(expr) => {
        let ty = self.expect_expr_form(expr, Form::Value);
        self.bin_op(span, BinaryOp::Sub, Type::U32, ty)
      }
      ExprKind::BinaryOp(op, a, b) => {
        let a = self.expect_expr_form(a, Form::Value);
        let b = self.expect_expr_form(b, Form::Value);
        self.bin_op(span, *op, a, b)
      }
      ExprKind::BinaryOpAssign(op, a, b) => {
        let mut a = self.expect_expr_form(a, Form::Place);
        let b = self.expect_expr_form(b, Form::Value);
        let mut o = self.bin_op(span, *op, a.clone(), b);
        self.unify(&mut a, &mut o);
        Type::UNIT
      }
      ExprKind::ComparisonOp(init, cmps) => {
        let mut lhs = self.expect_expr_form(init, Form::Value);
        self.concretize(&mut lhs);
        for (_, next) in cmps {
          let mut rhs = self.expect_expr_form(next, Form::Value);
          self.concretize(&mut rhs);
          if !matches!(
            (&lhs, &rhs),
            (Type::U32, Type::U32)
              | (Type::F32, Type::F32)
              | (Type::Error(_), _)
              | (_, Type::Error(_))
          ) {
            self.diags.add(Diag::CannotCompare {
              span,
              lhs: self.print_ty(&lhs),
              rhs: self.print_ty(&rhs),
            });
          }
          lhs = rhs;
        }
        Type::U32
      }
      ExprKind::U32(_) => Type::U32,
      ExprKind::F32(_) => Type::F32,
      ExprKind::String(_) => {
        self.string.clone().unwrap_or_else(|| Type::Error(self.diags.add(Diag::NoList { span })))
      }
      ExprKind::For(..) => todo!(),
    }
  }

  fn bin_op(&mut self, span: Span, op: BinaryOp, mut a: Type, mut b: Type) -> Type {
    match op {
      BinaryOp::Range | BinaryOp::RangeTo => todo!(),
      BinaryOp::Concat => {
        self.concretize(&mut a);
        self.concretize(&mut b);
        if self.unify(&mut a, &mut b) {
          if let Type::Adt(n, _) = a {
            if Some(n) == self.list {
              return a;
            }
          }
        }
      }
      _ => {
        if let Ok(ty) = self._bin_op(op, &mut a, &mut b, false) {
          return ty;
        }
      }
    }
    let diag = Diag::BadBinOp { span, op, lhs: self.print_ty(&a), rhs: self.print_ty(&b) };
    Type::Error(self.diags.add(diag))
  }

  fn _bin_op(
    &mut self,
    op: BinaryOp,
    mut a: &mut Type,
    mut b: &mut Type,
    i: bool,
  ) -> Result<Type, ()> {
    self.concretize(a);
    self.concretize(b);
    match (op, &mut a, &mut b) {
      (_, Type::Error(e), _) | (_, _, Type::Error(e)) => Ok(Type::Error(*e)),
      (_, Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => a
        .iter_mut()
        .zip(b)
        .map(|(a, b)| self._bin_op(op, a, b, i))
        .collect::<Result<_, _>>()
        .map(Type::Tuple),
      (_, Type::Tuple(a), b @ (Type::U32 | Type::F32)) => {
        a.iter_mut().map(|a| self._bin_op(op, a, b, i)).collect::<Result<_, _>>().map(Type::Tuple)
      }
      (_, a @ (Type::U32 | Type::F32), Type::Tuple(b)) => {
        b.iter_mut().map(|b| self._bin_op(op, a, b, i)).collect::<Result<_, _>>().map(Type::Tuple)
      }
      (_, Type::U32, Type::U32) if !i => Ok(Type::U32),
      (
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem,
        Type::U32 | Type::F32,
        Type::U32 | Type::F32,
      ) if !i => Ok(Type::F32),
      (_, Type::Inverse(a), b) => self._bin_op(op, a, b, !i),
      (_, a, Type::Inverse(b)) => self._bin_op(op, a, b, !i),
      _ => Err(()),
    }
  }

  fn print_ty(&self, ty: &Type) -> String {
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
          string += &self.print_ty(t);
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
          string += &self.print_ty(t);
          first = false;
        }
        string += ")";
        if **ret != Type::UNIT {
          string += " -> ";
          string += &self.print_ty(ret);
        }
        string
      }
      Type::Ref(ty) => format!("&{}", self.print_ty(ty)),
      Type::Inverse(ty) => format!("~{}", self.print_ty(ty)),
      Type::Adt(n, gens) => {
        let mut string = self.defs[*n].canonical.to_string();
        if !gens.is_empty() {
          string += "[";
          let mut first = true;
          for t in gens {
            if !first {
              string += ", ";
            }
            string += &self.print_ty(t);
            first = false;
          }
          string += "]";
        }
        string
      }
      Type::Opaque(n) => self.generics[*n].0 .0.into(),
      Type::Var(v) => match &self.state.vars[*v] {
        Ok(t) => self.print_ty(t),
        _ => format!("?{v}"),
      },
      Type::Error(_) => "??".to_string(),
    }
  }

  fn expect_expr_form_ty(&mut self, expr: &mut Expr, form: Form, ty: &mut Type) {
    let mut found = self.expect_expr_form(expr, form);
    if !self.unify(&mut found, ty) {
      self.diags.add(Diag::ExpectedTypeFound {
        span: expr.span,
        expected: self.print_ty(ty),
        found: self.print_ty(&found),
      });
    }
  }

  fn do_pat(&mut self, pat: &mut Pat, ty: Option<&mut Ty>, form: Form, refutable: bool) -> Type {
    match ty {
      Some(ty) => {
        let mut ty = self.hydrate_type(ty, true);
        self.expect_pat_form_ty(pat, form, refutable, &mut ty);
        ty
      }
      None => self.expect_pat_form(pat, form, refutable),
    }
  }

  fn expect_expr_form(&mut self, expr: &mut Expr, form: Form) -> Type {
    let (found, ty) = self.infer_expr(expr);
    self.coerce_expr(expr, found, form);
    ty
  }

  fn expect_pat_form_ty(&mut self, pat: &mut Pat, form: Form, refutable: bool, ty: &mut Type) {
    let mut found = self.expect_pat_form(pat, form, refutable);
    if !self.unify(&mut found, ty) {
      self.diags.add(Diag::ExpectedTypeFound {
        span: pat.span,
        expected: self.print_ty(ty),
        found: self.print_ty(&found),
      });
    }
  }

  fn expect_pat_form(&mut self, pat: &mut Pat, form: Form, refutable: bool) -> Type {
    let span = pat.span;
    match (&mut pat.kind, form) {
      (_, Form::Error(_)) => unreachable!(),
      (PatKind::Error(e), _) => Type::Error(*e),

      (PatKind::Adt(path, fields), _) => {
        let variant_id = path.path.resolved.unwrap();
        let variant = &self.defs[variant_id];
        let Some(variant_def) = &variant.variant_def else {
          return Type::Error(
            self.diags.add(Diag::PathNoPat { span, path: variant.canonical.clone() }),
          );
        };
        let adt_id = variant_def.adt;
        let adt = &self.defs[adt_id];
        let adt_def = adt.adt_def.as_ref().unwrap();
        if !refutable && adt_def.variants.len() > 1 {
          self.diags.add(Diag::ExpectedIrrefutablePat { span });
        }
        let generic_count = adt_def.generics.len();
        if let Err(diag) = Self::check_generic_count(span, variant, path, generic_count) {
          return Type::Error(self.diags.add(diag));
        }
        let generics = self.hydrate_generics(path, generic_count);
        let variant = &self.defs[variant_id];
        let variant_def = variant.variant_def.as_ref().unwrap();
        let field_tys = variant_def.field_types.as_ref().unwrap();
        let fields = fields.get_or_insert(Vec::new());
        if fields.len() != field_tys.len() {
          return Type::Error(self.diags.add(Diag::BadFieldCount {
            span,
            path: variant.canonical.clone(),
            expected: field_tys.len(),
            got: fields.len(),
          }));
        }
        let field_tys = field_tys.iter().map(|t| t.instantiate(&generics)).collect::<Vec<_>>();
        for (field, mut ty) in fields.iter_mut().zip(field_tys) {
          self.expect_pat_form_ty(field, form, refutable, &mut ty);
        }
        Type::Adt(adt_id, generics)
      }

      (PatKind::Hole, _) => self.new_var(span),
      (PatKind::Local(l), _) => {
        let old = self.state.locals.insert(*l, self.state.vars.len());
        debug_assert!(old.is_none());
        self.new_var(span)
      }
      (PatKind::Inverse(p), _) => self.expect_pat_form(p, form.inverse(), refutable).inverse(),
      (PatKind::Tuple(t), _) => {
        Type::Tuple(t.iter_mut().map(|p| self.expect_pat_form(p, form, refutable)).collect())
      }
      (PatKind::Move(p), Form::Place) => self.expect_pat_form(p, Form::Value, refutable),
      (PatKind::Deref(p), Form::Place) => {
        let ty = self.new_var(span);
        self.expect_pat_form_ty(p, Form::Value, refutable, &mut Type::Ref(Box::new(ty.clone())));
        ty
      }

      (PatKind::Ref(p), Form::Value | Form::Place) => {
        Type::Ref(Box::new(self.expect_pat_form(p, Form::Place, refutable)))
      }

      (PatKind::Ref(pat), Form::Space) => {
        let err = self.diags.add(Diag::RefSpacePat { span });
        pat.kind = PatKind::Error(err);
        Type::Error(err)
      }
      (PatKind::Deref(pat), _) => {
        let err = self.diags.add(Diag::DerefNonPlacePat { span });
        pat.kind = PatKind::Error(err);
        Type::Error(err)
      }
      (PatKind::Move(_), _) => {
        let err = self.diags.add(Diag::MoveNonPlacePat { span });
        pat.kind = PatKind::Error(err);
        Type::Error(err)
      }
    }
  }

  fn infer_block(&mut self, block: &mut Block) -> Type {
    let mut ty = Type::UNIT;
    for stmt in block.stmts.iter_mut() {
      match &mut stmt.kind {
        StmtKind::Let(l) => {
          let mut ty = self.do_pat(&mut l.bind, l.ty.as_mut(), Form::Value, false);
          if let Some(value) = &mut l.init {
            self.expect_expr_form_ty(value, Form::Value, &mut ty);
          }
        }
        StmtKind::Expr(e, semi) => {
          ty = self.expect_expr_form(e, Form::Value);
          if *semi {
            ty = Type::UNIT;
          }
        }
        StmtKind::Item(_) | StmtKind::Empty => ty = Type::UNIT,
      }
    }
    ty
  }

  fn concretize(&mut self, ty: &mut Type) {
    while let Type::Var(v) = *ty {
      match self.state.vars[v].clone() {
        Ok(t) => *ty = t,
        Err(span) => {
          let err = self.diags.add(Diag::CannotInfer { span });
          *ty = Type::Error(err);
        }
      }
    }
  }

  fn coerce_expr(&mut self, expr: &mut Expr, from: Form, to: Form) {
    let span = expr.span;
    match (from, to) {
      (_, Form::Error(_)) => unreachable!(),
      (Form::Error(_), _) => {}
      (Form::Value, Form::Value) | (Form::Place, Form::Place) | (Form::Space, Form::Space) => {}
      (Form::Value, Form::Place) => expr.wrap(ExprKind::Temp),
      (Form::Place, Form::Value) => Self::copy_expr(expr),
      (Form::Place, Form::Space) => Self::set_expr(expr),

      (Form::Space, Form::Value) => {
        expr.kind = ExprKind::Error(self.diags.add(Diag::ExpectedValueFoundSpaceExpr { span }));
      }
      (Form::Space, Form::Place) => {
        expr.kind = ExprKind::Error(self.diags.add(Diag::ExpectedPlaceFoundSpaceExpr { span }))
      }
      (Form::Value, Form::Space) => {
        expr.kind = ExprKind::Error(self.diags.add(Diag::ExpectedSpaceFoundValueExpr { span }));
      }
    }
  }

  fn copy_expr(expr: &mut Expr) {
    match &mut expr.kind {
      ExprKind::Local(l) => expr.kind = ExprKind::CopyLocal(*l),
      ExprKind::Tuple(t) => {
        for e in t {
          Self::copy_expr(e);
        }
      }
      ExprKind::Temp(t) => {
        expr.kind = take(&mut t.kind);
      }
      _ => expr.wrap(ExprKind::Copy),
    }
  }

  fn set_expr(expr: &mut Expr) {
    match &mut expr.kind {
      ExprKind::Local(l) => expr.kind = ExprKind::SetLocal(*l),
      ExprKind::Inverse(e) => Self::move_expr(e),
      ExprKind::Tuple(t) => {
        for e in t {
          Self::set_expr(e);
        }
      }
      _ => expr.wrap(ExprKind::Set),
    }
  }

  fn move_expr(expr: &mut Expr) {
    match &mut expr.kind {
      ExprKind::Local(l) => expr.kind = ExprKind::MoveLocal(*l),
      ExprKind::Inverse(e) => Self::set_expr(e),
      ExprKind::Tuple(t) => {
        for e in t {
          Self::move_expr(e);
        }
      }
      _ => expr.wrap(ExprKind::Move),
    }
  }

  fn infer_path_value(&mut self, path: &mut GenericPath) -> Type {
    let span = path.path.span;
    let def_id = path.path.resolved.unwrap();
    let def = &self.defs[def_id];
    let Some(value_def) = &def.value_def else {
      return Type::Error(self.diags.add(Diag::PathNoValue { span, path: def.canonical.clone() }));
    };
    let generic_count = value_def.generics.len();
    if let Err(diag) = Self::check_generic_count(span, def, path, generic_count) {
      return Type::Error(self.diags.add(diag));
    }
    let generics = self.hydrate_generics(path, generic_count);
    let def = &self.defs[def_id];
    def.value_def.as_ref().unwrap().ty.as_ref().unwrap().instantiate(&generics)
  }

  fn hydrate_type(&mut self, ty: &mut Ty, inference: bool) -> Type {
    let span = ty.span;
    match &mut ty.kind {
      TyKind::Hole if inference => self.new_var(span),
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
        let def_id = path.path.resolved.unwrap();
        let def = &self.defs[def_id];
        let Some(type_def) = &def.type_def else {
          let err = self.diags.add(Diag::PathNoType { span, path: def.canonical.clone() });
          ty.kind = TyKind::Error(err);
          return Type::Error(err);
        };
        let generic_count = type_def.generics.len();
        if let Err(diag) = Self::check_generic_count(span, def, path, generic_count) {
          let err = self.diags.add(diag);
          ty.kind = TyKind::Error(err);
          return Type::Error(err);
        }
        if !inference && path.generics.is_none() && generic_count != 0 {
          return Type::Error(self.diags.add(Diag::ItemTypeHole { span }));
        }
        let generics = self.hydrate_generics(path, generic_count);
        let def = &self.defs[def_id];
        if def.adt_def.is_some() {
          Type::Adt(def_id, generics)
        } else {
          self.resolve_type_alias(def.id);
          let def = &self.defs[def_id];
          if let Some(ty) = &def.type_def.as_ref().unwrap().ty {
            ty.instantiate(&generics)
          } else {
            dbg!(&def);
            Type::Error(self.diags.add(Diag::RecursiveTypeAlias { span }))
          }
        }
      }
      TyKind::Generic(n) => Type::Opaque(*n),
      TyKind::Error(e) => Type::Error(*e),
    }
  }

  fn hydrate_generics(&mut self, path: &mut GenericPath, generic_count: usize) -> Vec<Type> {
    if let Some(generics) = &mut path.generics {
      generics.iter_mut().map(|t| self.hydrate_type(t, true)).collect::<Vec<_>>()
    } else {
      iter::from_fn(|| Some(self.new_var(path.path.span))).take(generic_count).collect()
    }
  }

  fn check_generic_count(
    span: Span,
    def: &Def,
    path: &GenericPath,
    expected: usize,
  ) -> Result<(), Diag> {
    if let Some(generics) = &path.generics {
      if generics.len() != expected {
        Err(Diag::BadGenericCount {
          span,
          path: def.canonical.clone(),
          expected,
          got: path.generics.as_ref().unwrap().len(),
        })?
      }
    }
    Ok(())
  }

  fn resolve_type_alias(&mut self, def_id: usize) {
    if let Some(type_def) = &mut self.defs[def_id].type_def {
      if let Some(mut alias) = type_def.alias.take() {
        let ty = self.hydrate_type(&mut alias, false);
        self.defs[def_id].type_def.as_mut().unwrap().ty = Some(ty);
      }
    }
  }

  fn populate_def_tys(&mut self, def_id: usize) {
    if let Some(mut variant) = self.defs[def_id].variant_def.take() {
      variant.field_types =
        Some(variant.fields.iter_mut().map(|ty| self.hydrate_type(ty, false)).collect());
      self.defs[def_id].variant_def = Some(variant);
    }
    if let Some(mut value) = self.defs[def_id].value_def.take() {
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
            let variant = self.defs[def_id].variant_def.as_ref().unwrap();
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
      self.defs[def_id].value_def = Some(value);
    }
  }
}

fn get2_mut<T>(slice: &mut [T], a: usize, b: usize) -> (&mut T, &mut T) {
  assert!(a != b);
  if a < b {
    let (l, r) = slice.split_at_mut(b);
    (&mut l[a], &mut r[0])
  } else {
    let (l, r) = slice.split_at_mut(a);
    (&mut r[0], &mut l[b])
  }
}
