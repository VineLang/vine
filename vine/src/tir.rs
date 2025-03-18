use std::fmt::Debug;

use class::Classes;
use ivy::ast::Net;
use vine_util::{
  idx::{Counter, IdxVec},
  new_idx,
};

use crate::{
  ast::{Flex, Ident, LogicalOp, Span},
  chart::{AdtId, ImplDefId, ValueDefId, VariantId},
  diag::ErrorGuaranteed,
  types::Type,
};

new_idx!(pub LabelId);
new_idx!(pub Local; n => ["l{n}"]);
new_idx!(pub ClosureId; n => ["c{n}"]);

#[derive(Default, Debug, Clone)]
pub struct Tir<'core> {
  pub locals: Counter<Local>,
  pub value: TirExpr<'core>,
  pub closures: IdxVec<ClosureId, TirClosure<'core>>,
}

#[derive(Default, Debug, Clone)]
pub struct TirBlock<'core> {
  pub span: Span,
  pub ty: Type,
  pub stmts: Vec<TirStmt<'core>>,
}

#[derive(Clone)]
pub struct TirStmt<'core> {
  pub span: Span,
  pub kind: TirStmtKind<'core>,
}

#[derive(Debug, Clone)]
pub enum TirStmtKind<'core> {
  Let(TirLetStmt<'core>),
  Expr(TirExpr<'core>, bool),
  Empty,
}

#[derive(Debug, Clone)]
pub struct TirLetStmt<'core> {
  pub bind: TirPat,
  pub init: Option<TirExpr<'core>>,
  pub else_block: Option<TirBlock<'core>>,
}

#[derive(Debug, Clone)]
pub struct TirClosure<'core> {
  pub flex: Flex,
  pub params: Vec<TirPat>,
  pub body: TirBlock<'core>,
}

#[derive(Default, Clone)]
pub struct TirExpr<'core> {
  pub span: Span,
  pub ty: Type,
  pub kind: TirExprKind<'core>,
}

#[derive(Default, Debug, Clone, Classes)]
pub enum TirExprKind<'core> {
  #[default]
  #[class(space)]
  Hole,
  #[class(value)]
  Def(ValueDefId, Vec<TirImpl>),
  #[class(value, place, space)]
  Local(Local),
  #[class(value)]
  Closure(ClosureId),
  #[class(value)]
  Do(LabelId, TirBlock<'core>),
  #[class(value)]
  Assign(bool, B<TirExpr<'core>>, B<TirExpr<'core>>),
  #[class(value)]
  Match(B<TirExpr<'core>>, Vec<(TirPat, TirBlock<'core>)>),
  #[class(value)]
  If(Vec<(TirExpr<'core>, TirBlock<'core>)>, Option<TirBlock<'core>>),
  #[class(value)]
  While(LabelId, B<TirExpr<'core>>, TirBlock<'core>),
  #[class(value)]
  Loop(LabelId, TirBlock<'core>),
  #[class(value)]
  Return(Option<B<TirExpr<'core>>>),
  #[class(value)]
  Break(LabelId, Option<B<TirExpr<'core>>>),
  #[class(value)]
  Continue(LabelId),
  #[class(value)]
  Ref(B<TirExpr<'core>>),
  #[class(place)]
  Deref(B<TirExpr<'core>>),
  #[class(value, place, space)]
  Inverse(B<TirExpr<'core>>),
  #[class(place)]
  Place(B<TirExpr<'core>>, B<TirExpr<'core>>),
  #[class(value)]
  List(Vec<TirExpr<'core>>),
  #[class(value, place)]
  Field(B<TirExpr<'core>>, usize, usize),
  #[class(value)]
  Call(B<TirExpr<'core>>, Vec<TirExpr<'core>>),
  #[class(value, place, space)]
  Adt(Option<(AdtId, VariantId)>, Vec<TirExpr<'core>>),
  #[class(value, cond)]
  Bool(bool),
  #[class(value, cond)]
  Not(B<TirExpr<'core>>),
  #[class(value, cond)]
  Is(B<TirExpr<'core>>, B<TirPat>),
  #[class(value, cond)]
  LogicalOp(LogicalOp, B<TirExpr<'core>>, B<TirExpr<'core>>),
  #[class(value)]
  N32(u32),
  #[class(value)]
  F32(f32),
  #[class(value)]
  Char(char),
  #[class(value)]
  String(String, Vec<(TirExpr<'core>, String)>),
  #[class(value)]
  InlineIvy(Vec<(Ident<'core>, bool, TirExpr<'core>)>, Net),
  #[class(value)]
  CallAssign(B<TirExpr<'core>>, B<TirExpr<'core>>, B<TirExpr<'core>>),
  #[class(value)]
  CallCompare(B<TirExpr<'core>>, Vec<(TirExpr<'core>, TirExpr<'core>)>),
  #[class(error)]
  Error(ErrorGuaranteed),
}

#[derive(Default, Clone)]
pub struct TirPat {
  pub span: Span,
  pub ty: Type,
  pub kind: TirPatKind,
}

#[derive(Default, Debug, Clone, Classes)]
pub enum TirPatKind {
  #[default]
  #[class(value, place, space)]
  Hole,
  #[class(value, place, space)]
  Adt(Option<(AdtId, VariantId)>, Vec<TirPat>),
  #[class(value, place, space)]
  Local(Local),
  #[class(value, place)]
  Ref(B<TirPat>),
  #[class(place)]
  Deref(B<TirPat>),
  #[class(value, place, space)]
  Inverse(B<TirPat>),
  #[class(error)]
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone)]
pub enum TirImpl {
  Param(usize),
  Def(ImplDefId, Vec<TirImpl>),
  ClosureFn(ValueDefId, ClosureId),
  ClosureFork(ValueDefId, ClosureId),
  ClosureDrop(ValueDefId, ClosureId),
  Error(ErrorGuaranteed),
}

pub type B<T> = Box<T>;

macro_rules! debug_kind {
  ($($Ty:ty),*) => {$(
    impl Debug for $Ty {
      fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.kind, f)
      }
    }
  )*};
}

debug_kind!(TirStmt<'_>, TirExpr<'_>, TirPat);

impl From<ErrorGuaranteed> for TirExprKind<'_> {
  fn from(err: ErrorGuaranteed) -> Self {
    TirExprKind::Error(err)
  }
}

impl From<ErrorGuaranteed> for TirPatKind {
  fn from(err: ErrorGuaranteed) -> Self {
    TirPatKind::Error(err)
  }
}
