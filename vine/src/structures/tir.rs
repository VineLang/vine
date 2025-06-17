use class::Classes;
use ivy::ast::Net;
use vine_util::{
  idx::{Counter, IdxVec},
  new_idx,
};

use crate::structures::{
  ast::{LogicalOp, Span},
  chart::{ConstId, EnumId, FnId, ImplId, StructId, VariantId},
  diag::ErrorGuaranteed,
  types::Type,
};

new_idx!(pub LabelId);
new_idx!(pub Local; n => ["l{n}"]);
new_idx!(pub ClosureId; n => ["c{n}"]);
new_idx!(pub FnRelId);
new_idx!(pub ConstRelId);

#[derive(Debug, Clone)]
pub struct Tir {
  pub locals: Counter<Local>,
  pub closures: IdxVec<ClosureId, TirClosure>,
  pub const_rels: IdxVec<ConstRelId, (ConstId, Vec<TirImpl>)>,
  pub fn_rels: IdxVec<FnRelId, (FnId, Vec<TirImpl>)>,
  pub root: TirExpr,
}

#[derive(Debug, Clone)]
pub struct TirClosure {
  pub params: Vec<TirPat>,
  pub body: TirExpr,
}

#[derive(Debug, Clone)]
pub struct TirExpr {
  pub span: Span,
  pub ty: Type,
  pub form: Form,
  pub kind: Box<TirExprKind>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Form {
  Value,
  Place,
  Space,
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone, Classes)]
pub enum TirExprKind {
  #[class(space)]
  Hole,
  #[class(value)]
  Const(ConstRelId),
  #[class(value)]
  Fn(FnRelId),
  #[class(place)]
  Local(Local),
  #[class(value)]
  Closure(ClosureId),
  #[class(value)]
  Do(LabelId, TirExpr),
  #[class(value)]
  Assign(bool, TirExpr, TirExpr),
  #[class(value)]
  Match(TirExpr, Vec<(TirPat, TirExpr)>),
  #[class(value)]
  If(Vec<(TirExpr, TirExpr)>, Option<TirExpr>),
  #[class(value)]
  While(LabelId, TirExpr, TirExpr),
  #[class(value)]
  Loop(LabelId, TirExpr),
  #[class(value)]
  Return(Option<TirExpr>),
  #[class(value)]
  Break(LabelId, Option<TirExpr>),
  #[class(value)]
  Continue(LabelId),
  #[class(value)]
  Ref(TirExpr),
  #[class(place)]
  Deref(TirExpr),
  #[class(value)]
  Move(TirExpr),
  #[class(value, place, space)]
  Inverse(TirExpr),
  #[class(place)]
  Place(TirExpr, TirExpr),
  #[class(value, place, space)]
  Composite(Vec<TirExpr>),
  #[class(value)]
  List(Vec<TirExpr>),
  #[class(value, place)]
  Field(TirExpr, usize, usize),
  #[class(value)]
  Call(TirExpr, Vec<TirExpr>),
  #[class(value)]
  CallFn(FnRelId, Vec<TirExpr>),
  #[class(value, place, space)]
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
  #[class(value, space, place)]
  Unwrap(TirExpr),
  #[class(value)]
  Try(TirExpr),
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
  #[class(space)]
  Set(TirExpr),
  #[class(value)]
  Copy(TirExpr),
  #[class(space)]
  Hedge(TirExpr),
  #[class(value)]
  CopyLocal(Local),
  #[class(space)]
  HedgeLocal(Local),
  #[class(value)]
  MoveLocal(Local),
  #[class(space)]
  SetLocal(Local),
  #[class(value)]
  InlineIvy(Vec<(String, bool, TirExpr)>, Net),
  #[class(value)]
  CallAssign(FnRelId, TirExpr, TirExpr),
  #[class(value)]
  CallCompare(TirExpr, Vec<(FnRelId, TirExpr)>),
  #[class(value)]
  Let(TirPat, Option<TirExpr>, TirExpr),
  #[class(value)]
  LetElse(TirPat, TirExpr, TirExpr, TirExpr),
  #[class(value)]
  Seq(TirExpr, TirExpr),
  #[class(value, place, space)]
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone)]
pub struct TirPat {
  pub span: Span,
  pub ty: Type,
  pub form: Form,
  pub kind: Box<TirPatKind>,
}

#[derive(Debug, Clone, Classes)]
pub enum TirPatKind {
  #[class(value, place, space)]
  Hole,
  #[class(value, place, space)]
  Struct(StructId, TirPat),
  #[class(value, place, space)]
  Enum(EnumId, VariantId, Option<TirPat>),
  #[class(value, place, space)]
  Local(Local),
  #[class(value, place)]
  Ref(TirPat),
  #[class(place)]
  Deref(TirPat),
  #[class(value, place, space)]
  Inverse(TirPat),
  #[class(value, place, space)]
  Composite(Vec<TirPat>),
  #[class(value, place, space)]
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone)]
pub enum TirImpl {
  Error(ErrorGuaranteed),
  Param(usize),
  Def(ImplId, Vec<TirImpl>),
}

impl Form {
  pub fn inverse(self) -> Self {
    match self {
      Form::Value => Form::Space,
      Form::Place => Form::Place,
      Form::Space => Form::Value,
      Form::Error(e) => Form::Error(e),
    }
  }
}
