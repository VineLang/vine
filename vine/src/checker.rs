use std::{iter, mem::take, ops::Range};

use vine_util::interner::StringInterner;

use crate::{
  ast::{
    BinaryOp, Block, Expr, ExprKind, GenericPath, Ident, Pat, PatKind, Path, Span, StmtKind, Type,
    TypeKind,
  },
  diag::{Diag, DiagGroup, ErrorGuaranteed},
  resolve::{Node, NodeId, NodeValueKind, Resolver},
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
pub enum Ty {
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
  fn instantiate(&self, opaque: &[Ty]) -> Ty {
    match self {
      Ty::U32 => Ty::U32,
      Ty::F32 => Ty::F32,
      Ty::IO => Ty::IO,
      Ty::Tuple(tys) => Ty::Tuple(tys.iter().map(|t| t.instantiate(opaque)).collect()),
      Ty::Fn(tys, ret) => Ty::Fn(
        tys.iter().map(|t| t.instantiate(opaque)).collect(),
        Box::new(ret.instantiate(opaque)),
      ),
      Ty::Ref(t) => Ty::Ref(Box::new(t.instantiate(opaque))),
      Ty::Inverse(t) => Ty::Inverse(Box::new(t.instantiate(opaque))),
      Ty::Adt(node, tys) => Ty::Adt(*node, tys.iter().map(|t| t.instantiate(opaque)).collect()),
      Ty::Opaque(n) => opaque[*n].clone(),
      Ty::Error(e) => Ty::Error(*e),
      Ty::Var(_) => unreachable!(),
    }
  }

  fn inverse(self) -> Self {
    match self {
      Ty::Inverse(t) => *t,
      _ => Ty::Inverse(Box::new(self)),
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

impl Default for Ty {
  fn default() -> Self {
    Self::UNIT
  }
}

impl Ty {
  const UNIT: Ty = Ty::Tuple(Vec::new());
}

pub struct Checker<'n> {
  pub diags: DiagGroup,
  nodes: &'n mut [Node],
  vars: Vec<Result<Ty, Span>>,
  list: Option<NodeId>,
  string: Option<Ty>,
  generics: Vec<Ident>,
  return_ty: Option<Ty>,
  loop_ty: Option<Ty>,
}

impl<'n> Checker<'n> {
  pub fn new(resolver: &'n mut Resolver, interner: &StringInterner<'static>) -> Self {
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
    let string = list.map(|x| Ty::Adt(x, vec![Ty::U32]));
    Checker {
      diags,
      nodes: &mut resolver.nodes,
      vars: Vec::new(),
      list,
      string,
      generics: Vec::new(),
      return_ty: None,
      loop_ty: None,
    }
  }

  pub fn check_items(&mut self) {
    self._check_items(0..self.nodes.len());
  }

  pub(crate) fn _check_items(&mut self, range: Range<NodeId>) {
    for node in range.clone() {
      self.resolve_type_alias(node);
    }
    for node in range.clone() {
      self.populate_node_tys(node);
    }
    debug_assert!(self.vars.is_empty());
    for node in range.clone() {
      self.check_node(node);
    }
  }

  fn check_node(&mut self, node_id: NodeId) {
    self.vars.clear();
    let node = &mut self.nodes[node_id];
    if let Some(value) = &mut node.value {
      if let NodeValueKind::Expr(expr) = &mut value.kind {
        self.generics = take(&mut value.generics);
        let mut ty = value.ty.clone().unwrap();
        let mut expr = take(expr);
        self.vars.resize_with(node.locals, || Err(Span { file: 0, start: 0, end: 0 }));
        self.expect_expr_form_ty(&mut expr, Form::Value, &mut ty);
        let value = self.nodes[node_id].value.as_mut().unwrap();
        value.kind = NodeValueKind::Expr(expr);
        value.generics = take(&mut self.generics);
      }
    }
  }

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
      (Ty::Var(v), Ty::Var(u)) if *v == *u => i == j,
      (Ty::Var(v), Ty::Var(u)) => {
        let (v, u) = get2_mut(&mut self.vars, *v, *u);
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
      (&mut ref mut o @ Ty::Var(v), t) | (t, &mut ref mut o @ Ty::Var(v)) => {
        let v = &mut self.vars[v];
        if let Ok(u) = v {
          *o = u.clone();
          self._unify(a, b, i, j)
        } else {
          *o = t.clone();
          *v = Ok(t.clone().invert_if(i != j));
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

  fn _infer_expr_value(&mut self, expr: &mut Expr) -> Ty {
    let span = expr.span;
    match &mut expr.kind {
      ExprKind![error || place || space || synthetic] => unreachable!(),
      ExprKind::Path(path) => self.infer_path_value(path),
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
        let old = self.loop_ty.replace(Ty::UNIT);
        self.expect_expr_form_ty(cond, Form::Value, &mut Ty::U32);
        self.infer_block(block);
        self.loop_ty = old;
        Ty::UNIT
      }
      ExprKind::Loop(block) => {
        let result = self.new_var(span);
        let old = self.loop_ty.replace(result.clone());
        self.infer_block(block);
        self.loop_ty = old;
        result
      }
      ExprKind::Fn(args, ret, body) => Ty::Fn(
        args
          .iter_mut()
          .map(|(pat, ty)| self.do_pat(pat, ty.as_mut(), Form::Value, false))
          .collect(),
        Box::new({
          let mut ret = match ret {
            Some(Some(t)) => self.hydrate_type(t, true),
            Some(None) => Ty::UNIT,
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
          } else if !self.unify(&mut ty, &mut { Ty::UNIT }) {
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
          } else if !self.unify(&mut ty, &mut { Ty::UNIT }) {
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
        Ty::Ref(Box::new(ty))
      }
      ExprKind::Move(expr) => self.expect_expr_form(expr, Form::Place),
      ExprKind::List(els) => {
        let mut item = self.new_var(span);
        for el in els {
          self.expect_expr_form_ty(el, Form::Value, &mut item);
        }
        if let Some(list) = self.list {
          Ty::Adt(list, vec![item])
        } else {
          Ty::Error(self.diags.add(Diag::NoList { span }))
        }
      }
      ExprKind::Method(receiver, path, args) => {
        let mut ty = self.infer_path_value(path);
        match &mut ty {
          Ty::Fn(params, ret) => match params.first_mut() {
            Some(Ty::Ref(rec)) => {
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
                for (ty, arg) in params.iter_mut().zip(args) {
                  self.expect_expr_form_ty(arg, Form::Value, ty);
                }
                take(&mut **ret)
              }
            }
            Some(Ty::Error(e)) => Ty::Error(*e),
            _ => {
              Ty::Error(self.diags.add(Diag::NonMethodFunction { span, ty: self.print_ty(&ty) }))
            }
          },
          Ty::Error(e) => Ty::Error(*e),
          ty => Ty::Error(self.diags.add(Diag::NonFunctionCall { span, ty: self.print_ty(ty) })),
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
              for (ty, arg) in params.iter_mut().zip(args) {
                self.expect_expr_form_ty(arg, Form::Value, ty);
              }
              take(&mut **ret)
            }
          }
          Ty::Error(e) => Ty::Error(*e),
          ty => Ty::Error(self.diags.add(Diag::NonFunctionCall { span, ty: self.print_ty(ty) })),
        }
      }

      ExprKind::Not(expr) => {
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
      ExprKind::Neg(expr) => {
        let ty = self.expect_expr_form(expr, Form::Value);
        self.bin_op(span, BinaryOp::Sub, Ty::U32, ty)
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
        Ty::UNIT
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
      ExprKind::String(_) => {
        self.string.clone().unwrap_or_else(|| Ty::Error(self.diags.add(Diag::NoList { span })))
      }
      ExprKind::For(..) => todo!(),
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
    Ty::Error(self.diags.add(diag))
  }

  fn _bin_op(&mut self, op: BinaryOp, mut a: &mut Ty, mut b: &mut Ty, i: bool) -> Result<Ty, ()> {
    self.concretize(a);
    self.concretize(b);
    match (op, &mut a, &mut b) {
      (_, Ty::Error(e), _) | (_, _, Ty::Error(e)) => Ok(Ty::Error(*e)),
      (_, Ty::Tuple(a), Ty::Tuple(b)) if a.len() == b.len() => a
        .iter_mut()
        .zip(b)
        .map(|(a, b)| self._bin_op(op, a, b, i))
        .collect::<Result<_, _>>()
        .map(Ty::Tuple),
      (_, Ty::Tuple(a), b @ (Ty::U32 | Ty::F32)) => {
        a.iter_mut().map(|a| self._bin_op(op, a, b, i)).collect::<Result<_, _>>().map(Ty::Tuple)
      }
      (_, a @ (Ty::U32 | Ty::F32), Ty::Tuple(b)) => {
        b.iter_mut().map(|b| self._bin_op(op, a, b, i)).collect::<Result<_, _>>().map(Ty::Tuple)
      }
      (_, Ty::U32, Ty::U32) if !i => Ok(Ty::U32),
      (
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem,
        Ty::U32 | Ty::F32,
        Ty::U32 | Ty::F32,
      ) if !i => Ok(Ty::F32),
      (_, Ty::Inverse(a), b) => self._bin_op(op, a, b, !i),
      (_, a, Ty::Inverse(b)) => self._bin_op(op, a, b, !i),
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
      Ty::Fn(args, ret) => {
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
        if **ret != Ty::UNIT {
          string += " -> ";
          string += &self.print_ty(ret);
        }
        string
      }
      Ty::Ref(ty) => format!("&{}", self.print_ty(ty)),
      Ty::Inverse(ty) => format!("~{}", self.print_ty(ty)),
      Ty::Adt(n, gens) => {
        let mut string = self.nodes[*n].canonical.to_string();
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
      Ty::Opaque(n) => self.generics[*n].0 .0.into(),
      Ty::Var(v) => match &self.vars[*v] {
        Ok(t) => self.print_ty(t),
        _ => format!("?{v}"),
      },
      Ty::Error(_) => "??".to_string(),
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

  fn do_pat(&mut self, pat: &mut Pat, ty: Option<&mut Type>, form: Form, refutable: bool) -> Ty {
    match ty {
      Some(ty) => {
        let mut ty = self.hydrate_type(ty, true);
        self.expect_pat_form_ty(pat, form, refutable, &mut ty);
        ty
      }
      None => self.expect_pat_form(pat, form, refutable),
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

      (PatKind::Adt(path, fields), _) => {
        let variant_id = path.path.resolved.unwrap();
        let variant_node = &self.nodes[variant_id];
        let Some(variant) = &variant_node.variant else {
          return Ty::Error(
            self.diags.add(Diag::PathNoPat { span, path: variant_node.canonical.clone() }),
          );
        };
        let adt_id = variant.adt;
        let adt_node = &self.nodes[adt_id];
        let adt = adt_node.adt.as_ref().unwrap();
        if !refutable && adt.variants.len() > 1 {
          self.diags.add(Diag::ExpectedIrrefutablePat { span });
        }
        let generic_count = adt.generics.len();
        if let Err(diag) = Self::check_generic_count(span, variant_node, path, generic_count) {
          return Ty::Error(self.diags.add(diag));
        }
        let generics = self.hydrate_generics(path, generic_count);
        let variant_node = &self.nodes[variant_id];
        let variant = variant_node.variant.as_ref().unwrap();
        let field_tys = variant.field_tys.as_ref().unwrap();
        let fields = fields.get_or_insert(Vec::new());
        if fields.len() != field_tys.len() {
          return Ty::Error(self.diags.add(Diag::BadFieldCount {
            span,
            path: variant_node.canonical.clone(),
            expected: field_tys.len(),
            got: fields.len(),
          }));
        }
        let field_tys = field_tys.iter().map(|t| t.instantiate(&generics)).collect::<Vec<_>>();
        for (field, mut ty) in fields.iter_mut().zip(field_tys) {
          self.expect_pat_form_ty(field, form, refutable, &mut ty);
        }
        Ty::Adt(adt_id, generics)
      }

      (PatKind::Hole, _) => self.new_var(span),
      (PatKind::Local(l), _) => {
        let var = &mut self.vars[*l];
        debug_assert!(var.is_err());
        *var = Err(span);
        Ty::Var(*l)
      }
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
          let mut ty = self.do_pat(&mut l.bind, l.ty.as_mut(), Form::Value, false);
          if let Some(value) = &mut l.init {
            self.expect_expr_form_ty(value, Form::Value, &mut ty);
          }
        }
        StmtKind::Expr(e, semi) => {
          ty = self.expect_expr_form(e, Form::Value);
          if *semi {
            ty = Ty::UNIT;
          }
        }
        StmtKind::Item(_) | StmtKind::Empty => ty = Ty::UNIT,
      }
    }
    ty
  }

  fn concretize(&mut self, ty: &mut Ty) {
    while let Ty::Var(v) = *ty {
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

  fn infer_path_value(&mut self, path: &mut GenericPath) -> Ty {
    let span = path.path.span;
    let node_id = path.path.resolved.unwrap();
    let node = &self.nodes[node_id];
    let Some(value) = &node.value else {
      return Ty::Error(self.diags.add(Diag::PathNoValue { span, path: node.canonical.clone() }));
    };
    let generic_count = value.generics.len();
    if let Err(diag) = Self::check_generic_count(span, node, path, generic_count) {
      return Ty::Error(self.diags.add(diag));
    }
    let generics = self.hydrate_generics(path, generic_count);
    let node = &self.nodes[node_id];
    node.value.as_ref().unwrap().ty.as_ref().unwrap().instantiate(&generics)
  }

  fn hydrate_type(&mut self, ty: &mut Type, inference: bool) -> Ty {
    let span = ty.span;
    match &mut ty.kind {
      TypeKind::Hole if inference => self.new_var(span),
      TypeKind::Hole => Ty::Error(self.diags.add(Diag::ItemTypeHole { span })),
      TypeKind::Fn(args, ret) => Ty::Fn(
        args.iter_mut().map(|arg| self.hydrate_type(arg, inference)).collect(),
        Box::new(ret.as_mut().map(|ret| self.hydrate_type(ret, inference)).unwrap_or(Ty::UNIT)),
      ),
      TypeKind::Tuple(tys) => {
        Ty::Tuple(tys.iter_mut().map(|arg| self.hydrate_type(arg, inference)).collect())
      }
      TypeKind::Ref(t) => Ty::Ref(Box::new(self.hydrate_type(t, inference))),
      TypeKind::Inverse(t) => Ty::Inverse(Box::new(self.hydrate_type(t, inference))),
      TypeKind::Path(path) => {
        let node_id = path.path.resolved.unwrap();
        let node = &self.nodes[node_id];
        let Some(typ) = &node.typ else {
          let err = self.diags.add(Diag::PathNoType { span, path: node.canonical.clone() });
          ty.kind = TypeKind::Error(err);
          return Ty::Error(err);
        };
        let generic_count = typ.generics.len();
        if let Err(diag) = Self::check_generic_count(span, node, path, generic_count) {
          let err = self.diags.add(diag);
          ty.kind = TypeKind::Error(err);
          return Ty::Error(err);
        }
        if !inference && path.generics.is_none() && generic_count != 0 {
          return Ty::Error(self.diags.add(Diag::ItemTypeHole { span }));
        }
        let generics = self.hydrate_generics(path, generic_count);
        let node = &self.nodes[node_id];
        if node.adt.is_some() {
          Ty::Adt(node_id, generics)
        } else {
          self.resolve_type_alias(node.id);
          let node = &self.nodes[node_id];
          if let Some(ty) = &node.typ.as_ref().unwrap().ty {
            ty.instantiate(&generics)
          } else {
            dbg!(&node);
            Ty::Error(self.diags.add(Diag::RecursiveTypeAlias { span }))
          }
        }
      }
      TypeKind::Generic(n) => Ty::Opaque(*n),
      TypeKind::Error(e) => Ty::Error(*e),
    }
  }

  fn hydrate_generics(&mut self, path: &mut GenericPath, generic_count: usize) -> Vec<Ty> {
    if let Some(generics) = &mut path.generics {
      generics.iter_mut().map(|t| self.hydrate_type(t, true)).collect::<Vec<_>>()
    } else {
      iter::from_fn(|| Some(self.new_var(path.path.span))).take(generic_count).collect()
    }
  }

  fn check_generic_count(
    span: Span,
    node: &Node,
    path: &GenericPath,
    expected: usize,
  ) -> Result<(), Diag> {
    if let Some(generics) = &path.generics {
      if generics.len() != expected {
        Err(Diag::BadGenericCount {
          span,
          path: node.canonical.clone(),
          expected,
          got: path.generics.as_ref().unwrap().len(),
        })?
      }
    }
    Ok(())
  }

  fn resolve_type_alias(&mut self, node_id: usize) {
    if let Some(typ) = &mut self.nodes[node_id].typ {
      if let Some(mut alias) = typ.alias.take() {
        let ty = self.hydrate_type(&mut alias, false);
        self.nodes[node_id].typ.as_mut().unwrap().ty = Some(ty);
      }
    }
  }

  fn populate_node_tys(&mut self, node_id: usize) {
    if let Some(mut variant) = self.nodes[node_id].variant.take() {
      variant.field_tys =
        Some(variant.fields.iter_mut().map(|ty| self.hydrate_type(ty, false)).collect());
      self.nodes[node_id].variant = Some(variant);
    }
    if let Some(mut value) = self.nodes[node_id].value.take() {
      let ty = if let Some(ty) = &mut value.annotation {
        self.hydrate_type(ty, false)
      } else {
        match &mut value.kind {
          NodeValueKind::Expr(e) => {
            let ExprKind::Fn(args, Some(ret), _) = &mut e.kind else { unreachable!() };
            Ty::Fn(
              args
                .iter_mut()
                .map(|(pat, ty)| match ty {
                  Some(ty) => self.hydrate_type(ty, false),
                  None => Ty::Error(self.diags.add(Diag::FnItemUntypedParam { span: pat.span })),
                })
                .collect(),
              Box::new(ret.as_mut().map(|r| self.hydrate_type(r, false)).unwrap_or(Ty::UNIT)),
            )
          }
          NodeValueKind::Ivy(_) => unreachable!(),
          NodeValueKind::AdtConstructor => {
            let variant = self.nodes[node_id].variant.as_ref().unwrap();
            let params = variant.field_tys.clone().unwrap();
            let adt = Ty::Adt(variant.adt, (0..variant.generics.len()).map(Ty::Opaque).collect());
            if params.is_empty() {
              adt
            } else {
              Ty::Fn(params, Box::new(adt))
            }
          }
        }
      };
      value.ty = Some(ty);
      self.nodes[node_id].value = Some(value);
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
