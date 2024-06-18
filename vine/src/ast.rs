use std::fmt::{self, Debug, Display, Write};

use vine_util::interner::Interned;

#[derive(Clone)]
pub struct Item {
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
  pub inner: Mod,
}

#[derive(Debug, Clone)]
pub struct Mod {
  pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub struct UseTree {
  pub path: Path,
  pub children: Option<Vec<UseTree>>,
}

#[derive(Clone)]
pub struct Path {
  pub segments: Vec<Ident>,
  pub absolute: bool,
}

impl Path {
  pub const ROOT: Self = Self { segments: Vec::new(), absolute: true };
}

#[derive(Default, Debug, Clone)]
pub struct Block {
  pub stmts: Vec<Stmt>,
}

#[derive(Clone)]
pub struct Stmt {
  pub kind: StmtKind,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
  Let(LetStmt),
  Term(Term, bool),
  Empty,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
  pub bind: Term,
  pub init: Option<Term>,
}

#[derive(Clone)]
pub struct Term {
  pub kind: TermKind,
}

#[derive(Debug, Clone)]
pub enum TermKind {
  Hole,
  Path(Path),
  Local(usize),
  Block(Block),
  Assign(B<Term>, B<Term>),
  Match(B<Term>, Vec<(Term, Term)>),
  If(B<Term>, Block, B<Term>),
  While(B<Term>, Block),
  For(B<Term>, B<Term>, Block),
  Fn(Vec<Term>, B<Term>),
  Ref(B<Term>),
  Deref(B<Term>),
  Move(B<Term>),
  Tuple(Vec<Term>),
  List(Vec<Term>),
  Field(B<Term>, Path),
  Method(B<Term>, Path, Vec<Term>),
  Call(B<Term>, Vec<Term>),
  UnaryOp(UnaryOp, B<Term>),
  BinaryOp(BinaryOp, B<Term>, B<Term>),
  LogicalOp(LogicalOp, B<Term>, B<Term>),
  ComparisonOp(B<Term>, Vec<(ComparisonOp, Term)>),
  Num(u32),
  String(String),
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

pub type B<T> = Box<T>;

impl Term {
  pub fn new_ref(term: Term) -> Term {
    Term { kind: TermKind::Ref(Box::new(term)) }
  }

  pub fn new_deref(term: Term) -> Term {
    Term { kind: TermKind::Deref(Box::new(term)) }
  }

  pub fn new_num(num: u32) -> Term {
    Term { kind: TermKind::Num(num) }
  }

  pub fn new_path(path: Path) -> Term {
    Term { kind: TermKind::Path(path) }
  }

  pub fn new_move(term: Term) -> Term {
    Term { kind: TermKind::Move(Box::new(term)) }
  }

  pub fn new_unary_op(op: UnaryOp, term: Term) -> Term {
    Term { kind: TermKind::UnaryOp(op, Box::new(term)) }
  }

  pub fn new_binary_op(op: BinaryOp, lhs: Term, rhs: Term) -> Term {
    Term { kind: TermKind::BinaryOp(op, Box::new(lhs), Box::new(rhs)) }
  }
}

impl Path {
  pub fn extend(&self, ext: &[Ident]) -> Self {
    Path { segments: self.segments.iter().chain(ext).copied().collect(), absolute: self.absolute }
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
