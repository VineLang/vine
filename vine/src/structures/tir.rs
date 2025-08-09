use class::Classes;
use ivy::ast::Net;
use vine_util::{idx::IdxVec, new_idx};

use crate::{
  components::synthesizer::SyntheticImpl,
  structures::{
    ast::{Flex, LogicalOp, Span},
    chart::{EnumId, FnId, ImplId, StructId, VariantId},
    diag::ErrorGuaranteed,
    resolutions::{ConstRelId, FnRelId, Rels},
    types::{Type, Types},
  },
};

new_idx!(pub TargetId);
new_idx!(pub Local; n => ["l{n}"]);
new_idx!(pub ClosureId; n => ["c{n}"]);

#[derive(Debug, Clone)]
pub struct Tir<'core> {
  pub span: Span,
  pub types: Types<'core>,
  pub locals: IdxVec<Local, TirLocal>,
  pub rels: Rels<'core>,
  pub closures: IdxVec<ClosureId, TirClosure>,
  pub root: TirExpr,
}

#[derive(Debug, Clone)]
pub struct TirClosure {
  pub span: Span,
  pub ty: Type,
  pub flex: Flex,
  pub params: Vec<TirPat>,
  pub body: TirExpr,
}

#[derive(Debug, Clone)]
pub struct TirLocal {
  pub span: Span,
  pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct TirExpr {
  pub span: Span,
  pub ty: Type,
  pub kind: Box<TirExprKind>,
}

#[derive(Debug, Clone, Classes)]
pub enum TirExprKind {
  #[class(space)]
  Hole,
  #[class(value)]
  Const(ConstRelId),
  #[class(value)]
  Fn,
  #[class(poly, value, place, space)]
  Local(Local),
  #[class(value)]
  Closure(ClosureId),
  #[class(value)]
  Do(TargetId, TirExpr),
  #[class(nil, value)]
  Assign(bool, TirExpr, TirExpr),
  #[class(value)]
  Match(TirExpr, Vec<(TirPat, TirExpr)>),
  #[class(value)]
  If(TirExpr, TirExpr, Option<TirExpr>),
  #[class(value)]
  When(TargetId, Vec<(TirExpr, TirExpr)>, Option<TirExpr>),
  #[class(value)]
  While(TargetId, TirExpr, TirExpr, Option<TirExpr>),
  #[class(value)]
  Loop(TargetId, TirExpr),
  #[class(value)]
  For(TargetId, FnRelId, TirPat, TirExpr, TirExpr, Option<TirExpr>),
  #[class(nil, value)]
  Return(Option<TirExpr>),
  #[class(nil, value)]
  Break(TargetId, Option<TirExpr>),
  #[class(nil, value)]
  Continue(TargetId),
  #[class(value)]
  Ref(TirExpr),
  #[class(place)]
  Deref(TirExpr),
  #[class(poly, value, place, space)]
  Inverse(TirExpr),
  #[class(place)]
  Place(TirExpr, TirExpr),
  #[class(poly, value, place, space)]
  Composite(Vec<TirExpr>),
  #[class(value)]
  List(Vec<TirExpr>),
  #[class(poly)]
  Field(TirExpr, usize, Vec<Type>),
  #[class(value)]
  Call(FnRelId, Option<TirExpr>, Vec<TirExpr>),
  #[class(poly, value, place, space)]
  Struct(StructId, TirExpr),
  #[class(value)]
  Enum(EnumId, VariantId, Option<TirExpr>),
  #[class(value, cond)]
  Bool(bool),
  #[class(value, cond)]
  Not(TirExpr),
  #[class(value, cond)]
  Is(TirExpr, TirPat),
  #[class(value, cond)]
  LogicalOp(LogicalOp, TirExpr, TirExpr),
  #[class(poly, value, place, space)]
  Unwrap(StructId, TirExpr),
  #[class(value)]
  Try(Type, Type, TirExpr),
  #[class(value)]
  N32(u32),
  #[class(value)]
  I32(i32),
  #[class(value)]
  F32(f32),
  #[class(value)]
  Char(char),
  #[class(value)]
  String(String, Vec<(TirExpr, String)>),
  #[class(value)]
  InlineIvy(Vec<(String, bool, TirExpr)>, Net),
  #[class(nil, value)]
  CallAssign(FnRelId, TirExpr, TirExpr),
  #[class(value)]
  CallCompare(TirExpr, Vec<(FnRelId, TirExpr)>),
  #[class(value)]
  Let(TirPat, Option<TirExpr>, TirExpr),
  #[class(value)]
  Seq(TirExpr, TirExpr),
  #[class(poly, value, place, space)]
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone)]
pub struct TirPat {
  pub span: Span,
  pub ty: Type,
  pub kind: Box<TirPatKind>,
}

#[derive(Debug, Clone, Classes)]
pub enum TirPatKind {
  #[class(value, place, space, complete)]
  Hole,
  #[class(value, place, space, complete, incomplete)]
  Struct(StructId, TirPat),
  #[class(value, place, space, incomplete)]
  Enum(EnumId, VariantId, Option<TirPat>),
  #[class(value, place, space, complete)]
  Local(Local),
  #[class(value, place, complete, incomplete)]
  Ref(TirPat),
  #[class(place, complete)]
  Deref(TirPat),
  #[class(value, place, space, complete)]
  Inverse(TirPat),
  #[class(value, place, space, complete, incomplete)]
  Composite(Vec<TirPat>),
  #[class(value, place, space, complete)]
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TirImpl<'core> {
  Error(ErrorGuaranteed),
  Param(usize),
  Def(ImplId, Vec<TirImpl<'core>>),
  Fn(FnId, Vec<TirImpl<'core>>, usize),
  Closure(ClosureId, usize),
  ForkClosure(ClosureId),
  DropClosure(ClosureId),
  Synthetic(SyntheticImpl<'core>),
}

impl TirExpr {
  pub fn new(span: Span, ty: Type, kind: TirExprKind) -> Self {
    TirExpr { span, ty, kind: Box::new(kind) }
  }
}

impl TirPat {
  pub fn new(span: Span, ty: Type, kind: TirPatKind) -> Self {
    TirPat { span, ty, kind: Box::new(kind) }
  }
}
