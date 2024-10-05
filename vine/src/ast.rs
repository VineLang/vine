use std::{
  fmt::{self, Debug, Display, Write},
  path::PathBuf,
};

use ivy::ast::Net;
use vine_util::interner::Interned;

use crate::{diag::ErrorGuaranteed, resolve::NodeId};

#[derive(Clone)]
pub struct Item {
  pub span: Span,
  pub kind: ItemKind,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
  Fn(FnItem),
  Const(ConstItem),
  Struct(Struct),
  Enum(Enum),
  Pattern(PatternItem),
  Mod(ModItem),
  Use(UseTree),
  Ivy(InlineIvy),
  Taken,
}

#[derive(Debug, Clone)]
pub struct FnItem {
  pub name: Ident,
  pub params: Vec<Pat>,
  pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct ConstItem {
  pub name: Ident,
  pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct PatternItem {}

#[derive(Debug, Clone)]
pub struct Struct {
  pub name: Ident,
  pub fields: Vec<Ident>,
}

#[derive(Debug, Clone)]
pub struct Enum {
  pub name: Ident,
  pub variants: Vec<Struct>,
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
  pub path: Path,
  pub children: Option<Vec<UseTree>>,
}

#[derive(Debug, Clone)]
pub struct InlineIvy {
  pub name: Ident,
  pub net: Net,
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Path {
  pub span: Span,
  pub segments: Vec<Ident>,
  pub absolute: bool,
  pub resolved: Option<NodeId>,
}

impl Path {
  pub const ROOT: Self =
    Self { span: Span::NONE, segments: Vec::new(), absolute: true, resolved: Some(0) };
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
  pub init: Option<Expr>,
}

#[derive(Default, Clone)]
pub struct Expr {
  pub span: Span,
  pub kind: ExprKind,
}

#[derive(Default, Debug, Clone)]
pub enum ExprKind {
  #[default]
  Hole,
  Path(Path),
  Local(usize),
  Block(Block),
  Assign(B<Expr>, B<Expr>),
  Match(B<Expr>, Vec<(Pat, Expr)>),
  If(B<Expr>, Block, B<Expr>),
  While(B<Expr>, Block),
  WhileLet(B<Pat>, B<Expr>, Block),
  Loop(Block),
  For(B<Pat>, B<Expr>, Block),
  Fn(Vec<Pat>, B<Expr>),
  Return(B<Expr>),
  Break,
  Ref(B<Expr>),
  Deref(B<Expr>),
  Move(B<Expr>),
  Inverse(B<Expr>),
  Tuple(Vec<Expr>),
  List(Vec<Expr>),
  Field(B<Expr>, Path),
  Method(B<Expr>, Path, Vec<Expr>),
  Call(B<Expr>, Vec<Expr>),
  UnaryOp(UnaryOp, B<Expr>),
  BinaryOp(BinaryOp, B<Expr>, B<Expr>),
  LogicalOp(LogicalOp, B<Expr>, B<Expr>),
  ComparisonOp(B<Expr>, Vec<(ComparisonOp, Expr)>),
  BinaryOpAssign(BinaryOp, B<Expr>, B<Expr>),
  U32(u32),
  F32(f32),
  String(String),
  Error(ErrorGuaranteed),
}

#[derive(Default, Clone)]
pub struct Pat {
  pub span: Span,
  pub kind: PatKind,
}

#[derive(Default, Debug, Clone)]
pub enum PatKind {
  #[default]
  Hole,
  Adt(Path, Option<Vec<Pat>>),
  Local(usize),
  Ref(B<Pat>),
  Deref(B<Pat>),
  Move(B<Pat>),
  Inverse(B<Pat>),
  Tuple(Vec<Pat>),
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
  Neg,
  Not,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOp {
  LogicalAnd,
  LogicalOr,
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

impl Path {
  pub fn extend(&self, ext: &[Ident]) -> Self {
    Path {
      span: Span::NONE,
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

impl Debug for Path {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "`{self}`")
  }
}

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

debug_kind!(Item, Stmt, Expr, Pat);
