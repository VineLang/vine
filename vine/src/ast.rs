use std::{
  fmt::{self, Debug, Display, Write},
  mem::take,
  path::PathBuf,
};

use class::Classes;
use ivy::ast::Net;
use vine_util::{interner::Interned, new_idx};

use crate::{diag::ErrorGuaranteed, resolver::DefId};

new_idx!(pub Local; n => ["l{n}"]);
new_idx!(pub DynFnId; n => ["f{n}"]);

#[derive(Clone, Default)]
pub struct Item<'core> {
  pub span: Span,
  pub vis: Vis<'core>,
  pub attrs: Vec<Attr>,
  pub kind: ItemKind<'core>,
}

#[derive(Default, Debug, Clone)]
pub enum ItemKind<'core> {
  Fn(FnItem<'core>),
  Const(ConstItem<'core>),
  Struct(StructItem<'core>),
  Enum(Enum<'core>),
  Type(TypeItem<'core>),
  Mod(ModItem<'core>),
  Use(UseItem<'core>),
  Ivy(InlineIvy<'core>),
  #[default]
  Taken,
}

#[derive(Debug, Clone)]
pub struct FnItem<'core> {
  pub name: Ident<'core>,
  pub generics: Vec<Ident<'core>>,
  pub params: Vec<(Pat<'core>, Option<Ty<'core>>)>,
  pub ret: Option<Ty<'core>>,
  pub body: Block<'core>,
}

#[derive(Debug, Clone)]
pub struct ConstItem<'core> {
  pub name: Ident<'core>,
  pub generics: Vec<Ident<'core>>,
  pub ty: Ty<'core>,
  pub value: Expr<'core>,
}

#[derive(Debug, Clone)]
pub struct TypeItem<'core> {
  pub name: Ident<'core>,
  pub generics: Vec<Ident<'core>>,
  pub ty: Ty<'core>,
}

#[derive(Debug, Clone)]
pub struct StructItem<'core> {
  pub name: Ident<'core>,
  pub generics: Vec<Ident<'core>>,
  pub fields: Vec<Ty<'core>>,
}

#[derive(Debug, Clone)]
pub struct Enum<'core> {
  pub name: Ident<'core>,
  pub generics: Vec<Ident<'core>>,
  pub variants: Vec<Variant<'core>>,
}

#[derive(Debug, Clone)]
pub struct Variant<'core> {
  pub name: Ident<'core>,
  pub fields: Vec<Ty<'core>>,
}

#[derive(Debug, Clone)]
pub struct ModItem<'core> {
  pub name: Ident<'core>,
  pub kind: ModKind<'core>,
}

#[derive(Debug, Clone)]
pub enum ModKind<'core> {
  Loaded(Span, Vec<Item<'core>>),
  Unloaded(Span, PathBuf),
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone)]
pub struct UseItem<'core> {
  pub absolute: bool,
  pub tree: UseTree<'core>,
}

#[derive(Debug, Clone)]
pub struct UseTree<'core> {
  pub span: Span,
  pub path: Path<'core>,
  pub children: Option<Vec<UseTree<'core>>>,
}

#[derive(Debug, Clone)]
pub struct InlineIvy<'core> {
  pub name: Ident<'core>,
  pub generics: Vec<Ident<'core>>,
  pub ty: Ty<'core>,
  pub net: Net,
}

#[derive(Default, Debug, Clone)]
pub enum Vis<'core> {
  #[default]
  Private,
  Public,
  PublicTo(Span, Ident<'core>),
}

#[derive(Clone)]
pub struct Attr {
  pub span: Span,
  pub kind: AttrKind,
}

#[derive(Debug, Clone)]
pub enum AttrKind {
  Builtin(Builtin),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Builtin {
  Bool,
  N32,
  F32,
  Char,
  IO,
  Prelude,
  List,
  Concat,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Path<'core> {
  pub segments: Vec<Ident<'core>>,
  pub absolute: bool,
  pub resolved: Option<DefId>,
}

impl Path<'_> {
  pub const ROOT: Self = Self { segments: Vec::new(), absolute: true, resolved: Some(DefId::ROOT) };
}

#[derive(Default, Debug, Clone)]
pub struct Block<'core> {
  pub span: Span,
  pub stmts: Vec<Stmt<'core>>,
}

#[derive(Clone)]
pub struct Stmt<'core> {
  pub span: Span,
  pub kind: StmtKind<'core>,
}

#[derive(Debug, Clone)]
pub enum StmtKind<'core> {
  Let(LetStmt<'core>),
  DynFn(DynFnStmt<'core>),
  Expr(Expr<'core>, bool),
  Item(Item<'core>),
  Empty,
}

#[derive(Debug, Clone)]
pub struct LetStmt<'core> {
  pub bind: Pat<'core>,
  pub ty: Option<Ty<'core>>,
  pub init: Option<Expr<'core>>,
  pub else_block: Option<Block<'core>>,
}

#[derive(Debug, Clone)]
pub struct DynFnStmt<'core> {
  pub name: Ident<'core>,
  pub id: Option<DynFnId>,
  pub params: Vec<(Pat<'core>, Option<Ty<'core>>)>,
  pub ret: Option<Ty<'core>>,
  pub body: Block<'core>,
}

#[derive(Default, Clone)]
pub struct Expr<'core> {
  pub span: Span,
  pub kind: ExprKind<'core>,
}

#[derive(Default, Debug, Clone, Classes)]
pub enum ExprKind<'core> {
  #[default]
  #[class(space)]
  Hole,
  #[class(value, place, space)]
  Paren(B<Expr<'core>>),
  #[class(value)]
  Path(GenericPath<'core>),
  #[class(place, resolved)]
  Local(Local),
  #[class(value, resolved)]
  DynFn(DynFnId),
  #[class(value)]
  Do(Label<'core>, Block<'core>),
  #[class(value)]
  Block(Block<'core>),
  #[class(value)]
  Assign(bool, B<Expr<'core>>, B<Expr<'core>>),
  #[class(value)]
  Match(B<Expr<'core>>, Vec<(Pat<'core>, Expr<'core>)>),
  #[class(value)]
  If(B<Expr<'core>>, Block<'core>, B<Expr<'core>>),
  #[class(value)]
  While(Label<'core>, B<Expr<'core>>, Block<'core>),
  #[class(value)]
  Loop(Label<'core>, Block<'core>),
  #[class(value)]
  Fn(Vec<(Pat<'core>, Option<Ty<'core>>)>, Option<Option<Ty<'core>>>, B<Expr<'core>>),
  #[class(value)]
  Return(Option<B<Expr<'core>>>),
  #[class(value)]
  Break(Label<'core>, Option<B<Expr<'core>>>),
  #[class(value)]
  Continue(Label<'core>),
  #[class(value)]
  Ref(B<Expr<'core>>, bool),
  #[class(place)]
  Deref(B<Expr<'core>>, bool),
  #[class(value)]
  Move(B<Expr<'core>>, bool),
  #[class(value, place, space)]
  Inverse(B<Expr<'core>>, bool),
  #[class(place)]
  Place(B<Expr<'core>>, B<Expr<'core>>),
  #[class(value, place, space)]
  Tuple(Vec<Expr<'core>>),
  #[class(value)]
  List(Vec<Expr<'core>>),
  #[class(value, place)]
  TupleField(B<Expr<'core>>, usize, Option<usize>),
  #[class(value, sugar)]
  Method(B<Expr<'core>>, GenericPath<'core>, Vec<Expr<'core>>),
  #[class(value)]
  Call(B<Expr<'core>>, Vec<Expr<'core>>),
  #[class(value, place, space, resolved)]
  Adt(GenericPath<'core>, Vec<Expr<'core>>),
  #[class(value)]
  Neg(B<Expr<'core>>),
  #[class(value)]
  BinaryOp(BinaryOp, B<Expr<'core>>, B<Expr<'core>>),
  #[class(value, cond)]
  Bool(bool),
  #[class(value, cond)]
  Not(B<Expr<'core>>),
  #[class(value, cond)]
  Is(B<Expr<'core>>, B<Pat<'core>>),
  #[class(value, cond)]
  LogicalOp(LogicalOp, B<Expr<'core>>, B<Expr<'core>>),
  #[class(value)]
  ComparisonOp(B<Expr<'core>>, Vec<(ComparisonOp, Expr<'core>)>),
  #[class(value)]
  BinaryOpAssign(BinaryOp, B<Expr<'core>>, B<Expr<'core>>),
  #[class(value)]
  N32(u32),
  #[class(value)]
  F32(f32),
  #[class(value)]
  Char(char),
  #[class(value)]
  String(String),
  #[class(space, synthetic)]
  Set(B<Expr<'core>>),
  #[class(value, synthetic)]
  Copy(B<Expr<'core>>),
  #[class(space, synthetic)]
  Hedge(B<Expr<'core>>),
  #[class(value, synthetic)]
  CopyLocal(Local),
  #[class(space, synthetic)]
  HedgeLocal(Local),
  #[class(value, synthetic)]
  MoveLocal(Local),
  #[class(space, synthetic)]
  SetLocal(Local),
  #[class(error)]
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone, Copy)]
pub enum Label<'core> {
  Ident(Option<Ident<'core>>),
  Resolved(LabelId),
  Error(ErrorGuaranteed),
}

impl<'core> Label<'core> {
  pub fn as_id(self) -> LabelId {
    match self {
      Label::Resolved(id) => id,
      _ => unreachable!(),
    }
  }
}

new_idx!(pub LabelId);

#[derive(Default, Clone)]
pub struct Pat<'core> {
  pub span: Span,
  pub kind: PatKind<'core>,
}

#[derive(Default, Debug, Clone, Classes)]
pub enum PatKind<'core> {
  #[default]
  #[class(value, place, space)]
  Hole,
  #[class(value, place, space)]
  Paren(B<Pat<'core>>),
  #[class(value, place, space)]
  Adt(GenericPath<'core>, Option<Vec<Pat<'core>>>),
  #[class(value, place, space)]
  Local(Local),
  #[class(value, place)]
  Ref(B<Pat<'core>>),
  #[class(place)]
  Deref(B<Pat<'core>>),
  #[class(value, place, space)]
  Inverse(B<Pat<'core>>),
  #[class(value, place, space)]
  Tuple(Vec<Pat<'core>>),
  #[class(error)]
  Error(ErrorGuaranteed),
}

#[derive(Default, Debug, Clone)]
pub struct GenericPath<'core> {
  pub span: Span,
  pub path: Path<'core>,
  pub generics: Option<Vec<Ty<'core>>>,
}

#[derive(Clone)]
pub struct Ty<'core> {
  pub span: Span,
  pub kind: TyKind<'core>,
}

#[derive(Debug, Clone)]
pub enum TyKind<'core> {
  Hole,
  Paren(B<Ty<'core>>),
  Fn(Vec<Ty<'core>>, Option<B<Ty<'core>>>),
  Tuple(Vec<Ty<'core>>),
  Ref(B<Ty<'core>>),
  Inverse(B<Ty<'core>>),
  Path(GenericPath<'core>),
  Generic(usize),
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
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
    f.write_str(self.as_str())
  }
}

impl BinaryOp {
  pub fn as_str(&self) -> &'static str {
    match self {
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
    }
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
pub struct Ident<'core>(pub Interned<'core, str>);

impl Display for Ident<'_> {
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

impl<'core> Expr<'core> {
  pub fn wrap(&mut self, f: impl FnOnce(B<Self>) -> ExprKind<'core>) {
    let span = self.span;
    let kind = f(Box::new(take(self)));
    *self = Expr { span, kind };
  }
}

impl<'core> Pat<'core> {
  pub const HOLE: Self = Pat { span: Span::NONE, kind: PatKind::Hole };
}

impl<'core> Path<'core> {
  pub fn extend(&self, ext: &[Ident<'core>]) -> Self {
    Path {
      segments: self.segments.iter().chain(ext).copied().collect(),
      absolute: self.absolute,
      resolved: None,
    }
  }

  pub fn as_ident(&self) -> Option<Ident<'core>> {
    if let [ident] = self.segments[..] {
      Some(ident)
    } else {
      None
    }
  }
}

impl Display for Path<'_> {
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

impl Debug for Ident<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_char('`')?;
    f.write_str(&self.0)?;
    f.write_char('`')?;
    Ok(())
  }
}

macro_rules! debug_kind {
  ($($Ty:ty),*) => {$(
    impl Debug for $Ty {
      fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.kind, f)
      }
    }
  )*};
}

debug_kind!(Item<'_>, Attr, Stmt<'_>, Expr<'_>, Pat<'_>, Ty<'_>);

impl From<ErrorGuaranteed> for ExprKind<'_> {
  fn from(value: ErrorGuaranteed) -> Self {
    ExprKind::Error(value)
  }
}

impl From<ErrorGuaranteed> for PatKind<'_> {
  fn from(value: ErrorGuaranteed) -> Self {
    PatKind::Error(value)
  }
}

impl From<ErrorGuaranteed> for TyKind<'_> {
  fn from(value: ErrorGuaranteed) -> Self {
    TyKind::Error(value)
  }
}
