use std::{
  fmt::{self, Debug, Display, Write},
  mem::take,
  path::PathBuf,
};

use class::Classes;
use ivy::ast::Net;
use vine_util::interner::Interned;

use crate::{diag::ErrorGuaranteed, resolve::DefId};

#[derive(Clone)]
pub struct Item {
  pub span: Span,
  pub kind: ItemKind,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
  Fn(FnItem),
  Const(ConstItem),
  Struct(StructItem),
  Enum(Enum),
  Pattern(PatternItem),
  Type(TypeItem),
  Mod(ModItem),
  Use(UseTree),
  Ivy(InlineIvy),
  Taken,
}

#[derive(Debug, Clone)]
pub struct FnItem {
  pub name: Ident,
  pub generics: Vec<Ident>,
  pub params: Vec<(Pat, Option<Ty>)>,
  pub ret: Option<Ty>,
  pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct ConstItem {
  pub name: Ident,
  pub generics: Vec<Ident>,
  pub ty: Ty,
  pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct TypeItem {
  pub name: Ident,
  pub generics: Vec<Ident>,
  pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct PatternItem {}

#[derive(Debug, Clone)]
pub struct StructItem {
  pub name: Ident,
  pub generics: Vec<Ident>,
  pub fields: Vec<Ty>,
}

#[derive(Debug, Clone)]
pub struct Enum {
  pub name: Ident,
  pub generics: Vec<Ident>,
  pub variants: Vec<Variant>,
}

#[derive(Debug, Clone)]
pub struct Variant {
  pub name: Ident,
  pub fields: Vec<Ty>,
}

#[derive(Debug, Clone)]
pub struct ModItem {
  pub name: Ident,
  pub kind: ModKind,
}

#[derive(Debug, Clone)]
pub enum ModKind {
  Loaded(Vec<Item>),
  Unloaded(PathBuf),
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone)]
pub struct UseTree {
  pub span: Span,
  pub path: Path,
  pub children: Option<Vec<UseTree>>,
}

#[derive(Debug, Clone)]
pub struct InlineIvy {
  pub name: Ident,
  pub generics: Vec<Ident>,
  pub ty: Ty,
  pub net: Net,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Path {
  pub segments: Vec<Ident>,
  pub absolute: bool,
  pub resolved: Option<DefId>,
}

impl Path {
  pub const ROOT: Self = Self { segments: Vec::new(), absolute: true, resolved: Some(0) };
}

#[derive(Default, Debug, Clone)]
pub struct Block {
  pub span: Span,
  pub stmts: Vec<Stmt>,
}

#[derive(Clone)]
pub struct Stmt {
  pub span: Span,
  pub kind: StmtKind,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
  Let(LetStmt),
  Expr(Expr, bool),
  Item(Item),
  Empty,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
  pub bind: Pat,
  pub ty: Option<Ty>,
  pub init: Option<Expr>,
}

#[derive(Default, Clone)]
pub struct Expr {
  pub span: Span,
  pub kind: ExprKind,
}

#[derive(Default, Debug, Clone, Classes)]
pub enum ExprKind {
  #[default]
  #[class(space)]
  Hole,
  #[class(value)]
  Path(GenericPath),
  #[class(place)]
  Local(usize),
  #[class(value)]
  Block(Block),
  #[class(value)]
  Assign(B<Expr>, B<Expr>),
  #[class(value)]
  Match(B<Expr>, Vec<(Pat, Expr)>),
  #[class(value)]
  If(B<Expr>, Block, B<Expr>),
  #[class(value)]
  While(B<Expr>, Block),
  #[class(value)]
  Loop(Block),
  #[class(value)]
  For(B<Pat>, B<Expr>, Block),
  #[class(value)]
  Fn(Vec<(Pat, Option<Ty>)>, Option<Option<Ty>>, B<Expr>),
  #[class(value)]
  Return(Option<B<Expr>>),
  #[class(value)]
  Break(Option<B<Expr>>),
  #[class(value)]
  Continue,
  #[class(value)]
  Ref(B<Expr>),
  #[class(place)]
  Deref(B<Expr>),
  #[class(value)]
  Move(B<Expr>),
  #[class(value, place, space)]
  Inverse(B<Expr>),
  #[class(value, place, space)]
  Tuple(Vec<Expr>),
  #[class(value)]
  List(Vec<Expr>),
  #[class(place, sugar)]
  Field(B<Expr>, GenericPath),
  #[class(value, sugar)]
  Method(B<Expr>, GenericPath, Vec<Expr>),
  #[class(value)]
  Call(B<Expr>, Vec<Expr>),
  #[class(value)]
  Neg(B<Expr>),
  #[class(value)]
  BinaryOp(BinaryOp, B<Expr>, B<Expr>),
  #[class(value, cond)]
  Not(B<Expr>),
  #[class(value, cond)]
  Is(B<Expr>, B<Pat>),
  #[class(value, cond)]
  LogicalOp(LogicalOp, B<Expr>, B<Expr>),
  #[class(value)]
  ComparisonOp(B<Expr>, Vec<(ComparisonOp, Expr)>),
  #[class(value)]
  BinaryOpAssign(BinaryOp, B<Expr>, B<Expr>),
  #[class(value)]
  U32(u32),
  #[class(value)]
  F32(f32),
  #[class(value)]
  String(String),
  #[class(place, synthetic)]
  Temp(B<Expr>),
  #[class(space, synthetic)]
  Set(B<Expr>),
  #[class(value, synthetic)]
  Copy(B<Expr>),
  #[class(value, synthetic)]
  CopyLocal(usize),
  #[class(value, synthetic)]
  MoveLocal(usize),
  #[class(space, synthetic)]
  SetLocal(usize),
  #[class(error)]
  Error(ErrorGuaranteed),
}

#[derive(Default, Clone)]
pub struct Pat {
  pub span: Span,
  pub kind: PatKind,
}

#[derive(Default, Debug, Clone, Classes)]
pub enum PatKind {
  #[default]
  #[class(value, place, space)]
  Hole,
  #[class(value, place, space)]
  Adt(GenericPath, Option<Vec<Pat>>),
  #[class(value, place, space)]
  Local(usize),
  #[class(value, place)]
  Ref(B<Pat>),
  #[class(place)]
  Deref(B<Pat>),
  #[class(place)]
  Move(B<Pat>),
  #[class(value, place, space)]
  Inverse(B<Pat>),
  #[class(value, place, space)]
  Tuple(Vec<Pat>),
  #[class(error)]
  Error(ErrorGuaranteed),
}

#[derive(Default, Debug, Clone)]
pub struct GenericPath {
  pub span: Span,
  pub path: Path,
  pub generics: Option<Vec<Ty>>,
}

#[derive(Clone)]
pub struct Ty {
  pub span: Span,
  pub kind: TyKind,
}

#[derive(Debug, Clone)]
pub enum TyKind {
  Hole,
  Fn(Vec<Ty>, Option<B<Ty>>),
  Tuple(Vec<Ty>),
  Ref(B<Ty>),
  Inverse(B<Ty>),
  Path(GenericPath),
  Generic(usize),
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
  Range,
  RangeTo,
  BitOr,
  BitXor,
  BitAnd,
  Shl,
  Shr,
  Add,
  Sub,
  Concat,
  Mul,
  Div,
  Rem,
}

impl Display for BinaryOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      BinaryOp::Range => "..",
      BinaryOp::RangeTo => "..=",
      BinaryOp::BitOr => "|",
      BinaryOp::BitXor => "^",
      BinaryOp::BitAnd => "&",
      BinaryOp::Shl => "<<",
      BinaryOp::Shr => ">>",
      BinaryOp::Add => "+",
      BinaryOp::Sub => "-",
      BinaryOp::Concat => "++",
      BinaryOp::Mul => "*",
      BinaryOp::Div => "/",
      BinaryOp::Rem => "%",
    })
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOp {
  And,
  Or,
  Implies,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonOp {
  Eq,
  Ne,
  Lt,
  Gt,
  Le,
  Ge,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident(pub Interned<'static, str>);

impl Display for Ident {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0 .0)
  }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
  pub file: usize,
  pub start: usize,
  pub end: usize,
}

impl Span {
  pub const NONE: Span = Span { file: usize::MAX, start: 0, end: 0 };
}

pub type B<T> = Box<T>;

impl Expr {
  pub fn wrap(&mut self, f: impl FnOnce(B<Self>) -> ExprKind) {
    let span = self.span;
    let kind = f(Box::new(take(self)));
    *self = Expr { span, kind };
  }
}

impl Path {
  pub fn extend(&self, ext: &[Ident]) -> Self {
    Path {
      segments: self.segments.iter().chain(ext).copied().collect(),
      absolute: self.absolute,
      resolved: None,
    }
  }

  pub fn as_ident(&self) -> Option<Ident> {
    if let [ident] = self.segments[..] {
      Some(ident)
    } else {
      None
    }
  }
}

impl Display for Path {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.absolute {
      f.write_str("::")?;
    }
    for (i, seg) in self.segments.iter().enumerate() {
      if i != 0 {
        f.write_str("::")?;
      }
      f.write_str(&seg.0)?;
    }
    Ok(())
  }
}

// impl Debug for Path {
//   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//     write!(f, "`{self}`")
//   }
// }

impl Debug for Ident {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_char('`')?;
    f.write_str(&self.0)?;
    f.write_char('`')?;
    Ok(())
  }
}

macro_rules! debug_kind {
  ($($Ty:ident),*) => {$(
    impl Debug for $Ty {
      fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.kind, f)
      }
    }
  )*};
}

debug_kind!(Item, Stmt, Expr, Pat, Ty);

impl From<ErrorGuaranteed> for ExprKind {
  fn from(value: ErrorGuaranteed) -> Self {
    ExprKind::Error(value)
  }
}

impl From<ErrorGuaranteed> for PatKind {
  fn from(value: ErrorGuaranteed) -> Self {
    PatKind::Error(value)
  }
}

impl From<ErrorGuaranteed> for TyKind {
  fn from(value: ErrorGuaranteed) -> Self {
    TyKind::Error(value)
  }
}
