#![allow(unused)]

use std::mem::take;

use slab::Slab;

use crate::{
  ast::{BinaryOp, Block, Expr, ExprKind, GenericPath, Pat, PatKind, Span, StmtKind},
  diag::{Diag, DiagGroup, ErrorGuaranteed},
  resolve::{Node, NodeId},
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

type TyVar = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Ty {
  U32,
  F32,
  IO,
  Tuple(Vec<Ty>),
  Fn(Vec<Ty>, Box<Ty>),
  Ref(Box<Ty>),
  Inverse(Box<Ty>),
  Adt(NodeId, Vec<Ty>),
  Opaque(usize),
  Var(TyVar),
  Error(ErrorGuaranteed),
}

impl Ty {
  fn inverse(self) -> Self {
    match self {
      Ty::Inverse(t) => *t,
      _ => Ty::Inverse(Box::new(self)),
    }
  }
}

impl Default for Ty {
  fn default() -> Self {
    Self::UNIT
  }
}

impl Ty {
  const UNIT: Ty = Ty::Tuple(Vec::new());
}

struct Checker<'n> {
  pub diags: DiagGroup,
  nodes: &'n mut [Node],
  vars: Vec<Result<Ty, Span>>,
  string: Ty,
  list: NodeId,
}

impl<'n> Checker<'n> {
  fn new_var(&mut self, span: Span) -> Ty {
    let v = self.vars.len();
    self.vars.push(Err(span));
    Ty::Var(v)
  }

  fn unify(&mut self, a: &mut Ty, b: &mut Ty) -> bool {
    self._unify(a, b, false, false)
  }

  fn _unify(&mut self, a: &mut Ty, b: &mut Ty, i: bool, j: bool) -> bool {
    match (&mut *a, &mut *b) {
      (Ty::Error(_), _) | (_, Ty::Error(_)) => true,
      (Ty::Inverse(a), Ty::Inverse(b)) => self._unify(a, b, !i, !j),
      (a, Ty::Inverse(b)) => self._unify(a, b, i, !j),
      (Ty::Inverse(a), b) => self._unify(a, b, !i, j),
      (&mut ref mut o @ Ty::Var(v), t) | (t, &mut ref mut o @ Ty::Var(v)) => {
        let v = &mut self.vars[v];
        if let Ok(u) = v {
          *o = u.clone();
          self._unify(a, b, i, j)
        } else {
          *o = t.clone();
          *v = Ok(t.clone());
          true
        }
      }
      (Ty::U32, Ty::U32) | (Ty::F32, Ty::F32) | (Ty::IO, Ty::IO) if i == j => true,
      (Ty::Opaque(a), Ty::Opaque(b)) if a == b && i == j => true,
      (Ty::Tuple(a), Ty::Tuple(b)) if a.len() == b.len() => {
        let mut success = true;
        for (a, b) in a.iter_mut().zip(b) {
          success &= self._unify(a, b, i, j);
        }
        success
      }
      (Ty::Ref(a), Ty::Ref(b)) if i == j => self._unify(a, b, i, j),
      (Ty::Fn(x, a), Ty::Fn(y, b)) if i == j => {
        let mut success = true;
        for (x, y) in x.iter_mut().zip(y) {
          success &= self._unify(x, y, i, j);
        }
        success &= self._unify(a, b, i, j);
        success
      }
      (Ty::Adt(n, a), Ty::Adt(m, b)) if n == m && i == j => {
        let mut success = true;
        for (a, b) in a.iter_mut().zip(b) {
          success &= self._unify(a, b, i, j);
        }
        success
      }
      _ => false,
    }
  }

  fn infer_expr(&mut self, expr: &mut Expr) -> (Form, Ty) {
    let span = expr.span;
    match &mut expr.kind {
      ExprKind![synthetic] => unreachable!(),
      ExprKind![!error && !space && !place && !synthetic] => {
        (Form::Value, self._infer_expr_value(expr))
      }
      ExprKind::Error(e) => (Form::Error(*e), Ty::Error(*e)),
      ExprKind::Hole => (Form::Space, self.new_var(span)),
      ExprKind::Local(l) => (Form::Place, Ty::Var(*l)),
      ExprKind::Deref(_) => todo!(),
      ExprKind::Inverse(expr) => {
        let (form, ty) = self.infer_expr(expr);
        (form.inverse(), ty.inverse())
      }
      ExprKind::Tuple(v) => {
        if v.is_empty() {
          (Form::Place, Ty::UNIT)
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
          (form, Ty::Tuple(types))
        }
      }
      ExprKind::Field(..) => todo!(),
    }
  }

  fn infer_path(&mut self, path: &mut GenericPath) -> Ty {
    todo!()
  }

  fn _infer_expr_value(&mut self, expr: &mut Expr) -> Ty {
    let span = expr.span;
    match &mut expr.kind {
      ExprKind![error || place || space || synthetic] => unreachable!(),
      ExprKind::Path(path) => self.infer_path(path),
      ExprKind::Block(block) => self.infer_block(block),
      ExprKind::Assign(space, value) => {
        let mut ty = self.expect_expr_form(space, Form::Space);
        self.expect_expr_form_ty(value, Form::Value, &mut ty);
        Ty::UNIT
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
        self.expect_expr_form_ty(cond, Form::Value, &mut Ty::U32);
        let mut then = self.infer_block(then);
        let mut els = self.expect_expr_form(els, Form::Value);
        if !self.unify(&mut then, &mut els) {
          Ty::Error(self.diags.add(Diag::MismatchedThenElseTypes {
            span,
            then: self.print_ty(&then),
            els: self.print_ty(&els),
          }))
        } else {
          then
        }
      }
      ExprKind::While(cond, block) => {
        self.expect_expr_form_ty(cond, Form::Value, &mut Ty::U32);
        self.infer_block(block);
        Ty::UNIT
      }
      ExprKind::Loop(block) => {
        self.infer_block(block);
        self.new_var(span)
      }
      ExprKind::For(..) => todo!(),
      ExprKind::Fn(args, ret) => Ty::Fn(
        args.iter_mut().map(|arg| self.expect_pat_form(arg, Form::Value, false)).collect(),
        Box::new(self.expect_expr_form(ret, Form::Value)),
      ),
      ExprKind::Return(e) | ExprKind::Break(e) => {
        // todo: check
        if let Some(e) = e {
          self.expect_expr_form(e, Form::Value);
        }
        self.new_var(span)
      }
      ExprKind::Continue => {
        // todo: check
        self.new_var(span)
      }
      ExprKind::Ref(expr) => {
        let ty = self.expect_expr_form(expr, Form::Place);
        Ty::Ref(Box::new(ty))
      }
      ExprKind::Move(expr) => self.expect_expr_form(expr, Form::Place),
      ExprKind::List(els) => {
        let mut item = self.new_var(span);
        for el in els {
          self.expect_expr_form_ty(el, Form::Value, &mut item);
        }
        Ty::Adt(self.list, vec![item])
      }
      ExprKind::Method(receiver, path, args) => {
        let mut ty = self.infer_path(path);
        match &mut ty {
          Ty::Fn(params, ret) => {
            if let Some(Ty::Ref(rec)) = params.first_mut() {
              self.expect_expr_form_ty(receiver, Form::Place, rec);
              let params = &mut params[1..];
              if params.len() != args.len() {
                Ty::Error(self.diags.add(Diag::BadArgCount {
                  span,
                  expected: params.len(),
                  got: args.len(),
                  ty: self.print_ty(&ty),
                }))
              } else {
                for (ty, arg) in params.into_iter().zip(args) {
                  self.expect_expr_form_ty(arg, Form::Value, ty);
                }
                take(&mut **ret)
              }
            } else {
              Ty::Error(self.diags.add(Diag::NonMethodFunction { span, ty: self.print_ty(&ty) }))
            }
          }
          Ty::Error(e) => Ty::Error(*e),
          ty => Ty::Error(self.diags.add(Diag::NonFunctionCall { span, ty: self.print_ty(&ty) })),
        }
      }
      ExprKind::Call(func, args) => {
        let mut ty = self.expect_expr_form(func, Form::Value);
        self.concretize(&mut ty);
        match &mut ty {
          Ty::Fn(params, ret) => {
            if params.len() != args.len() {
              Ty::Error(self.diags.add(Diag::BadArgCount {
                span,
                expected: params.len(),
                got: args.len(),
                ty: self.print_ty(&ty),
              }))
            } else {
              for (mut ty, arg) in params.into_iter().zip(args) {
                self.expect_expr_form_ty(arg, Form::Value, &mut ty);
              }
              take(&mut **ret)
            }
          }
          Ty::Error(e) => Ty::Error(*e),
          ty => Ty::Error(self.diags.add(Diag::NonFunctionCall { span, ty: self.print_ty(&ty) })),
        }
      }
      ExprKind::Neg(expr) | ExprKind::Not(expr) => {
        self.expect_expr_form_ty(expr, Form::Value, &mut Ty::U32);
        Ty::U32
      }
      ExprKind::Is(expr, pat) => {
        let mut ty = self.expect_expr_form(expr, Form::Value);
        self.expect_pat_form_ty(pat, Form::Value, true, &mut ty);
        Ty::U32
      }
      ExprKind::LogicalOp(_, a, b) => {
        self.expect_expr_form_ty(a, Form::Value, &mut Ty::U32);
        self.expect_expr_form_ty(b, Form::Value, &mut Ty::U32);
        Ty::U32
      }
      ExprKind::BinaryOp(op, a, b) => {
        let a = self.expect_expr_form(a, Form::Value);
        let b = self.expect_expr_form(b, Form::Value);
        self.bin_op(span, *op, a, b)
      }
      ExprKind::BinaryOpAssign(op, a, b) => {
        let a = self.expect_expr_form(a, Form::Place);
        let b = self.expect_expr_form(b, Form::Value);
        self.bin_op(span, *op, a, b)
      }
      ExprKind::ComparisonOp(init, cmps) => {
        let mut lhs = self.expect_expr_form(init, Form::Value);
        self.concretize(&mut lhs);
        for (_, next) in cmps {
          let mut rhs = self.expect_expr_form(next, Form::Value);
          self.concretize(&mut rhs);
          if !matches!(
            (&lhs, &rhs),
            (Ty::U32, Ty::U32) | (Ty::F32, Ty::F32) | (Ty::Error(_), _) | (_, Ty::Error(_))
          ) {
            self.diags.add(Diag::CannotCompare {
              span,
              lhs: self.print_ty(&lhs),
              rhs: self.print_ty(&rhs),
            });
          }
          lhs = rhs;
        }
        Ty::U32
      }
      ExprKind::U32(_) => Ty::U32,
      ExprKind::F32(_) => Ty::F32,
      ExprKind::String(_) => self.string.clone(),
    }
  }

  fn bin_op(&mut self, span: Span, op: BinaryOp, mut a: Ty, mut b: Ty) -> Ty {
    match op {
      BinaryOp::Range | BinaryOp::RangeTo => todo!(),
      BinaryOp::Concat => {
        self.concretize(&mut a);
        self.concretize(&mut b);
        if self.unify(&mut a, &mut b) {
          if let Ty::Adt(n, _) = a {
            if n == self.list {
              return a;
            }
          }
        }
      }
      _ => {
        if let Ok(ty) = self._bin_op(op, &mut a, &mut b) {
          return ty;
        }
      }
    }
    let diag = Diag::BadBinOp { span, op, lhs: self.print_ty(&a), rhs: self.print_ty(&b) };
    Ty::Error(self.diags.add(diag))
  }

  fn _bin_op(&mut self, op: BinaryOp, mut a: &mut Ty, mut b: &mut Ty) -> Result<Ty, ()> {
    self.concretize(a);
    self.concretize(b);
    match (op, &mut a, &mut b) {
      (_, Ty::Error(e), _) | (_, _, Ty::Error(e)) => Ok(Ty::Error(*e)),
      (_, Ty::Tuple(a), Ty::Tuple(b)) if a.len() == b.len() => a
        .iter_mut()
        .zip(b)
        .map(|(a, b)| self._bin_op(op, a, b))
        .collect::<Result<_, _>>()
        .map(Ty::Tuple),
      (_, Ty::Tuple(a), b @ (Ty::U32 | Ty::F32)) => {
        a.iter_mut().map(|a| self._bin_op(op, a, b)).collect::<Result<_, _>>().map(Ty::Tuple)
      }
      (_, a @ (Ty::U32 | Ty::F32), Ty::Tuple(b)) => {
        b.iter_mut().map(|b| self._bin_op(op, a, b)).collect::<Result<_, _>>().map(Ty::Tuple)
      }
      (_, Ty::U32, Ty::U32) => Ok(Ty::U32),
      (
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem,
        Ty::F32,
        Ty::F32,
      ) => Ok(Ty::F32),
      _ => Err(()),
    }
  }

  fn print_ty(&self, ty: &Ty) -> String {
    match ty {
      Ty::U32 => "u32".into(),
      Ty::F32 => "f32".into(),
      Ty::IO => "IO".into(),
      Ty::Tuple(t) => {
        let mut string = "(".to_owned();
        let mut first = false;
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
      Ty::Fn(args, ret) => {
        let mut string = "fn(".to_owned();
        let mut first = false;
        for t in args {
          if !first {
            string += ", ";
          }
          string += &self.print_ty(t);
          first = false;
        }
        string += ")";
        if **ret != Ty::UNIT {
          string += " -> ";
          string += &self.print_ty(ty);
        }
        string
      }
      Ty::Ref(ty) => format!("&{}", self.print_ty(ty)),
      Ty::Inverse(ty) => format!("~{}", self.print_ty(ty)),
      Ty::Adt(_, vec) => todo!(),
      Ty::Opaque(n) => format!("T{n}"),
      Ty::Var(v) => format!("?{v}"),
      Ty::Error(_) => format!("??"),
    }
  }

  fn expect_expr_form_ty(&mut self, expr: &mut Expr, form: Form, ty: &mut Ty) {
    let mut found = self.expect_expr_form(expr, form);
    if !self.unify(&mut found, ty) {
      self.diags.add(Diag::ExpectedTypeFound {
        span: expr.span,
        expected: self.print_ty(ty),
        found: self.print_ty(&found),
      });
    }
  }

  fn expect_expr_form(&mut self, expr: &mut Expr, form: Form) -> Ty {
    let (found, ty) = self.infer_expr(expr);
    self.coerce_expr(expr, found, form);
    ty
  }

  fn expect_pat_form_ty(&mut self, pat: &mut Pat, form: Form, refutable: bool, ty: &mut Ty) {
    let mut found = self.expect_pat_form(pat, form, refutable);
    if !self.unify(&mut found, ty) {
      self.diags.add(Diag::ExpectedTypeFound {
        span: pat.span,
        expected: self.print_ty(ty),
        found: self.print_ty(&found),
      });
    }
  }

  fn expect_pat_form(&mut self, pat: &mut Pat, form: Form, refutable: bool) -> Ty {
    let span = pat.span;
    match (&mut pat.kind, form) {
      (_, Form::Error(_)) => unreachable!(),
      (PatKind::Error(e), _) => Ty::Error(*e),

      (PatKind::Adt(_, _), _) => todo!(),

      (PatKind::Hole, _) => self.new_var(span),
      (PatKind::Local(l), _) => Ty::Var(*l),
      (PatKind::Inverse(p), _) => self.expect_pat_form(p, form.inverse(), refutable).inverse(),
      (PatKind::Tuple(t), _) => {
        Ty::Tuple(t.iter_mut().map(|p| self.expect_pat_form(p, form, refutable)).collect())
      }
      (PatKind::Move(p), Form::Place) => self.expect_pat_form(p, Form::Value, refutable),
      (PatKind::Deref(p), Form::Place) => {
        let ty = self.new_var(span);
        self.expect_pat_form_ty(p, Form::Value, refutable, &mut Ty::Ref(Box::new(ty.clone())));
        ty
      }
      (PatKind::Type(p, _), _) => todo!(),

      (PatKind::Ref(p), Form::Value | Form::Place) => {
        Ty::Ref(Box::new(self.expect_pat_form(p, Form::Place, refutable)))
      }

      (PatKind::Ref(pat), Form::Space) => {
        let err = self.diags.add(Diag::RefSpacePat { span });
        pat.kind = PatKind::Error(err);
        Ty::Error(err)
      }
      (PatKind::Deref(pat), _) => {
        let err = self.diags.add(Diag::DerefNonPlacePat { span });
        pat.kind = PatKind::Error(err);
        Ty::Error(err)
      }
      (PatKind::Move(_), _) => {
        let err = self.diags.add(Diag::MoveNonPlacePat { span });
        pat.kind = PatKind::Error(err);
        Ty::Error(err)
      }
    }
  }

  fn infer_block(&mut self, block: &mut Block) -> Ty {
    let mut ty = Ty::UNIT;
    for stmt in block.stmts.iter_mut() {
      match &mut stmt.kind {
        StmtKind::Let(l) => {
          self.expect_pat_form(&mut l.bind, Form::Value, true);
          if let Some(value) = &mut l.init {
            self.expect_expr_form_ty(value, Form::Value, &mut ty);
          }
        }
        StmtKind::Expr(e, _) => {
          ty = self.expect_expr_form(e, Form::Value);
        }
        StmtKind::Item(_) | StmtKind::Empty => ty = Ty::UNIT,
      }
    }
    ty
  }

  fn concretize(&mut self, ty: &mut Ty) {
    if let Ty::Var(v) = *ty {
      match self.vars[v].clone() {
        Ok(t) => *ty = t,
        Err(span) => {
          let err = self.diags.add(Diag::CannotInfer { span });
          *ty = Ty::Error(err);
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
}
