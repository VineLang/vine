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
  pub params: Vec<Term>,
  pub body: Term,
}

#[derive(Debug, Clone)]
pub struct ConstItem {
  pub name: Ident,
  pub value: Term,
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
  Term(Term, bool),
  Item(Item),
  Empty,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
  pub bind: Term,
  pub init: Option<Term>,
}

#[derive(Default, Clone)]
pub struct Term {
  pub span: Span,
  pub kind: TermKind,
}

#[derive(Default, Debug, Clone)]
pub enum TermKind {
  #[default]
  Hole,
  Path(Path),
  Local(usize),
  Block(Block),
  Assign(B<Term>, B<Term>),
  Match(B<Term>, Vec<(Term, Term)>),
  If(B<Term>, Block, B<Term>),
  While(B<Term>, Block),
  WhileLet(B<Term>, B<Term>, Block),
  Loop(Block),
  For(B<Term>, B<Term>, Block),
  Fn(Vec<Term>, B<Term>),
  Return(B<Term>),
  Break,
  Ref(B<Term>),
  Deref(B<Term>),
  Move(B<Term>),
  Inverse(B<Term>),
  Tuple(Vec<Term>),
  List(Vec<Term>),
  Field(B<Term>, Path),
  Method(B<Term>, Path, Vec<Term>),
  Call(B<Term>, Vec<Term>),
  UnaryOp(UnaryOp, B<Term>),
  BinaryOp(BinaryOp, B<Term>, B<Term>),
  LogicalOp(LogicalOp, B<Term>, B<Term>),
  ComparisonOp(B<Term>, Vec<(ComparisonOp, Term)>),
  BinaryOpAssign(BinaryOp, B<Term>, B<Term>),
  U32(u32),
  F32(f32),
  String(String),
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

debug_kind!(Item, Stmt, Term);
