use std::fmt::Debug;

use class::Classes;
use ivy::ast::Net;
use vine_util::{
  idx::{Counter, IdxVec},
  new_idx,
};

use crate::{
  ast::{Flex, Ident, LogicalOp, Span},
  chart::{EnumId, ImplDefId, StructId, ValueDefId, VariantId},
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

#[derive(Default, Clone)]
pub struct TirBlock<'core> {
  pub span: Span,
  pub ty: Type,
  pub kind: TirBlockKind<'core>,
}

#[derive(Default, Debug, Clone)]
pub enum TirBlockKind<'core> {
  #[default]
  Nil,
  Error(ErrorGuaranteed),
  Let(TirPat, Option<B<TirExpr<'core>>>, B<TirBlock<'core>>),
  LetElse(TirPat, B<TirExpr<'core>>, B<TirBlock<'core>>, B<TirBlock<'core>>),
  Seq(B<TirExpr<'core>>, B<TirBlock<'core>>),
  Expr(B<TirExpr<'core>>),
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
  #[class(value, place, space)]
  Unwrap(B<TirExpr<'core>>),
  #[class(value)]
  Call(TirImpl, Option<B<TirExpr<'core>>>, Vec<TirExpr<'core>>),
  #[class(value, place, space)]
  Struct(StructId, B<TirExpr<'core>>),
  #[class(value)]
  Enum(EnumId, VariantId, Option<B<TirExpr<'core>>>),
  #[class(value, place, space)]
  Composite(Vec<TirExpr<'core>>),
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
  CallAssign(TirImpl, B<TirExpr<'core>>, B<TirExpr<'core>>),
  #[class(value)]
  CallCompare(B<TirExpr<'core>>, Vec<(TirImpl, TirExpr<'core>)>),
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
  Hole,
  Composite(Vec<TirPat>),
  Struct(StructId, B<TirPat>),
  Enum(EnumId, VariantId, Option<B<TirPat>>),
  Local(Local),
  Ref(B<TirPat>),
  Deref(B<TirPat>),
  Inverse(B<TirPat>),
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TirImpl {
  Param(usize),
  Def(ImplDefId, Vec<TirImpl>),
  ClosureFn(ValueDefId, ClosureId, Vec<TirImpl>),
  ClosureFork(ValueDefId, ClosureId, Vec<TirImpl>),
  ClosureDrop(ValueDefId, ClosureId, Vec<TirImpl>),
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

debug_kind!(TirBlock<'_>, TirExpr<'_>, TirPat);

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
