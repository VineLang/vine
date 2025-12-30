use std::{
  cmp::{Ordering, Reverse},
  collections::BTreeMap,
  fmt::{self, Debug, Display, Write},
};

use ivy::ast::Net;
use vine_util::{idx, nat::Nat};

use crate::{
  components::loader::FileId, features::builtin::Builtin, structures::diag::ErrorGuaranteed,
};

pub mod visit;

#[derive(Clone)]
pub struct Item {
  pub span: Span,
  pub name_span: Span,
  pub docs: Vec<String>,
  pub vis: Vis,
  pub attrs: Vec<Attr>,
  pub kind: ItemKind,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
  Fn(FnItem),
  Const(ConstItem),
  Struct(StructItem),
  Enum(EnumItem),
  Type(TypeItem),
  Mod(ModItem),
  Trait(TraitItem),
  Impl(ImplItem),
  Use(UseItem),
  OuterMod,
  Taken,
}

#[derive(Debug, Clone)]
pub struct FnItem {
  pub method: bool,
  pub name: Ident,
  pub generics: GenericParams,
  pub params: Vec<Pat>,
  pub ret: Option<Ty>,
  pub body: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct ConstItem {
  pub name: Ident,
  pub generics: GenericParams,
  pub ty: Ty,
  pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct TypeItem {
  pub name: Ident,
  pub generics: GenericParams,
  pub ty: Option<Ty>,
}

#[derive(Debug, Clone)]
pub struct StructItem {
  pub flex_span: Span,
  pub flex: Flex,
  pub name: Ident,
  pub generics: GenericParams,
  pub data_vis: Vis,
  pub data: Vec<Ty>,
}

#[derive(Debug, Clone)]
pub struct EnumItem {
  pub flex_span: Span,
  pub flex: Flex,
  pub name: Ident,
  pub generics: GenericParams,
  pub variants: Vec<Variant>,
}

#[derive(Debug, Clone)]
pub struct Variant {
  pub name: Ident,
  pub data: Vec<Ty>,
}

#[derive(Debug, Clone)]
pub struct ModItem {
  pub name: Ident,
  pub generics: GenericParams,
  pub kind: ModKind,
}

#[derive(Debug, Clone)]
pub enum ModKind {
  Loaded(Span, Option<FileId>, Vec<Item>),
  Unloaded(Span, Option<String>),
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone)]
pub struct TraitItem {
  pub name: Ident,
  pub generics: GenericParams,
  pub items_span: Span,
  pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub struct ImplItem {
  pub name: Option<Ident>,
  pub generics: GenericParams,
  pub trait_: Trait,
  pub kind: ImplItemKind,
}

#[derive(Debug, Clone)]
pub enum ImplItemKind {
  Direct(Span, Vec<Item>),
  Indirect(Option<Impl>),
}

#[derive(Debug, Clone)]
pub struct UseItem {
  pub relative: BTreeMap<Ident, UseTree>,
  pub absolute: BTreeMap<Ident, UseTree>,
}

#[derive(Debug, Clone)]
pub struct UseTree {
  pub span: Span,
  pub absolute: bool,
  pub aliases: Vec<Ident>,
  pub implicit: bool,
  pub children: BTreeMap<Ident, UseTree>,
}

impl UseTree {
  pub fn empty(span: Span) -> Self {
    UseTree {
      span,
      absolute: false,
      aliases: Vec::new(),
      implicit: false,
      children: BTreeMap::new(),
    }
  }

  pub fn prune(&mut self) -> bool {
    self.children.retain(|_, tree| tree.prune());
    self.implicit || !self.aliases.is_empty() || !self.children.is_empty()
  }
}

#[derive(Debug, Clone)]
pub enum Vis {
  Private,
  Public,
  PublicTo(Span, Ident),
}

#[derive(Clone)]
pub struct Attr {
  pub span: Span,
  pub kind: AttrKind,
}

#[derive(Debug, Clone)]
pub enum AttrKind {
  Builtin(Builtin),
  Manual,
  Basic,
  Become(Path),
  Cfg(Cfg),
  Frameless,
  Test,
}

#[derive(Debug, Clone)]
pub struct Cfg {
  pub span: Span,
  pub kind: Box<CfgKind>,
}

#[derive(Debug, Clone)]
pub enum CfgKind {
  Bool(Ident),
  Paren(Cfg),
  Literal(bool),
  And(Cfg, Cfg),
  Or(Cfg, Cfg),
  Not(Cfg),
}

pub type GenericParams = Generics<TypeParam, ImplParam>;
pub type GenericArgs = Generics<Ty, Impl>;

#[derive(Debug, Clone)]
pub struct Generics<T, I> {
  pub span: Span,
  pub inherit: bool,
  pub types: Vec<T>,
  pub impls: Vec<I>,
}

impl<T, I> Generics<T, I> {
  pub fn empty(span: Span) -> Self {
    Generics { span, inherit: false, types: Vec::new(), impls: Vec::new() }
  }
}

#[derive(Debug, Clone)]
pub struct TypeParam {
  pub span: Span,
  pub name: Ident,
  pub flex: Flex,
}

#[derive(Debug, Clone)]
pub struct ImplParam {
  pub span: Span,
  pub name: Option<Ident>,
  pub trait_: Trait,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Flex {
  None,
  Fork,
  Drop,
  Full,
}

impl Flex {
  pub fn fork(self) -> bool {
    matches!(self, Flex::Fork | Flex::Full)
  }

  pub fn drop(self) -> bool {
    matches!(self, Flex::Drop | Flex::Full)
  }

  pub fn sigil(self) -> &'static str {
    match self {
      Flex::None => "",
      Flex::Fork => "+",
      Flex::Drop => "?",
      Flex::Full => "*",
    }
  }
}

#[derive(Debug, Clone)]
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
  Assert(AssertStmt),
  Let(LetStmt),
  LetFn(LetFnStmt),
  Expr(Expr, bool),
  Item(Item),
  Return(Option<Expr>),
  Break(Target, Option<Expr>),
  Continue(Target),
  Empty,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
  pub bind: Pat,
  pub kind: LetStmtKind,
}

#[derive(Debug, Clone)]
pub enum LetStmtKind {
  Init(Expr),
  Uninit,
  Loop,
}

#[derive(Debug, Clone)]
pub struct AssertStmt {
  pub expr: Expr,
  pub else_: Block,
}

#[derive(Debug, Clone)]
pub struct LetFnStmt {
  pub flex: Flex,
  pub name_span: Span,
  pub name: Ident,
  pub params: Vec<Pat>,
  pub ret: Option<Ty>,
  pub body: Block,
}

#[derive(Clone)]
pub struct Expr {
  pub span: Span,
  pub kind: Box<ExprKind>,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
  Hole,
  Paren(Expr),
  Path(Path, Option<Vec<Expr>>),
  Do(Label, Option<Ty>, Block),
  Assign(bool, Expr, Expr),
  Match(Expr, Option<Ty>, Vec<(Pat, Block)>),
  If(Expr, Option<Ty>, Block, Option<Block>),
  When(Label, Option<Ty>, Vec<(Expr, Block)>, Option<Block>),
  While(Label, Expr, Option<Ty>, Block, Option<Block>),
  Loop(Label, Option<Ty>, Block),
  For(Label, Pat, Expr, Option<Ty>, Block, Option<Block>),
  Fn(Flex, Vec<Pat>, Option<Ty>, Block),
  Ref(Expr, bool),
  Deref(Expr, bool),
  Inverse(Expr, bool),
  Place(Expr, Expr),
  Tuple(Vec<Expr>),
  Object(Vec<(Key, Expr)>),
  List(Vec<Expr>),
  TupleField(Expr, usize),
  ObjectField(Expr, Key),
  Method(Expr, Span, Ident, GenericArgs, Vec<Expr>),
  Call(Expr, Vec<Expr>),
  Sign(Sign, Expr),
  BinaryOp(BinaryOp, Expr, Expr),
  Bool(bool),
  Not(Expr),
  Is(Expr, Pat),
  LogicalOp(LogicalOp, Expr, Expr),
  ComparisonOp(Expr, Vec<(ComparisonOp, Expr)>),
  BinaryOpAssign(BinaryOp, Expr, Expr),
  Cast(Expr, Ty, bool),
  Unwrap(Expr),
  Try(Expr),
  RangeExclusive(Option<Expr>, Option<Expr>),
  RangeInclusive(Option<Expr>, Expr),
  N32(u32),
  F32(f32),
  Float(Span, f64, Ty),
  Nat(Span, Nat, Ty),
  Char(char),
  String(StringSegment, Vec<(Expr, StringSegment)>),
  InlineIvy(Vec<(Ident, bool, Expr)>, Ty, Span, Net),
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone)]
pub struct Label(pub Option<Ident>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Target {
  AnyLoop,
  Label(Ident),
  Do,
  Loop,
  While,
  For,
  When,
}

#[derive(Debug, Clone)]
pub struct StringSegment {
  pub content: String,
  pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Sign {
  Pos,
  Neg,
}

#[derive(Clone)]
pub struct Pat {
  pub span: Span,
  pub kind: Box<PatKind>,
}

#[derive(Debug, Clone)]
pub enum PatKind {
  Hole,
  Paren(Pat),
  Annotation(Pat, Ty),
  Path(Path, Option<Vec<Pat>>),
  Ref(Pat),
  Deref(Pat),
  Inverse(Pat),
  Tuple(Vec<Pat>),
  Object(Vec<(Key, Pat)>),
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone)]
pub struct Path {
  pub span: Span,
  pub absolute: bool,
  pub segments: Vec<Ident>,
  pub generics: Option<GenericArgs>,
}

impl Path {
  pub fn as_ident(&self) -> Option<Ident> {
    if self.generics.is_none()
      && let [ident] = &self.segments[..]
    {
      return Some(ident.clone());
    }
    None
  }
}

#[derive(Clone)]
pub struct Ty {
  pub span: Span,
  pub kind: Box<TyKind>,
}

#[derive(Debug, Clone)]
pub enum TyKind {
  Hole,
  Never,
  Paren(Ty),
  Fn(Path),
  Tuple(Vec<Ty>),
  Object(Vec<(Key, Ty)>),
  Ref(Ty),
  Key(Ident),
  Inverse(Ty),
  Path(Path),
  Error(ErrorGuaranteed),
}

#[derive(Clone)]
pub struct Impl {
  pub span: Span,
  pub kind: Box<ImplKind>,
}

#[derive(Debug, Clone)]
pub enum ImplKind {
  Hole,
  Path(Path),
  Fn(Path),
  Error(ErrorGuaranteed),
}

#[derive(Clone)]
pub struct Trait {
  pub span: Span,
  pub kind: Box<TraitKind>,
}

#[derive(Debug, Clone)]
pub enum TraitKind {
  Path(Path),
  Fn(Ty, Vec<Ty>, Option<Ty>),
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOp {
  BitOr,
  BitXor,
  BitAnd,
  Shl,
  Shr,
  Add,
  Sub,
  Mul,
  Div,
  Rem,
  Concat,
  Pow,
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
      BinaryOp::Pow => "**",
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOp {
  And,
  Or,
  Implies,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ComparisonOp {
  Eq,
  Ne,
  Lt,
  Gt,
  Le,
  Ge,
}

impl ComparisonOp {
  pub fn as_str(&self) -> &'static str {
    match self {
      ComparisonOp::Eq => "==",
      ComparisonOp::Ne => "!=",
      ComparisonOp::Lt => "<",
      ComparisonOp::Gt => ">",
      ComparisonOp::Le => "<=",
      ComparisonOp::Ge => ">=",
    }
  }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident(pub String);

#[derive(Debug, Clone)]
pub struct Key {
  pub span: Span,
  pub ident: Ident,
}

impl Display for Ident {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
  pub file: FileId,
  pub start: usize,
  pub end: usize,
}

impl Ord for Span {
  fn cmp(&self, other: &Self) -> Ordering {
    (self.file, self.start, Reverse(self.end)).cmp(&(other.file, other.start, Reverse(other.end)))
  }
}

impl PartialOrd for Span {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Span {
  pub const NONE: Span = Span { file: FileId(usize::MAX), start: 0, end: 0 };
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

impl From<Key> for Path {
  fn from(key: Key) -> Self {
    Path { span: key.span, absolute: false, segments: Vec::from([key.ident]), generics: None }
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
  ($($Ty:ty),*) => {$(
    impl Debug for $Ty {
      fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.kind, f)
      }
    }
  )*};
}

debug_kind!(Item, Attr, Stmt, Expr, Pat, Ty, Impl, Trait);

impl From<ErrorGuaranteed> for ExprKind {
  fn from(err: ErrorGuaranteed) -> Self {
    ExprKind::Error(err)
  }
}

impl From<ErrorGuaranteed> for PatKind {
  fn from(err: ErrorGuaranteed) -> Self {
    PatKind::Error(err)
  }
}

impl From<ErrorGuaranteed> for TyKind {
  fn from(err: ErrorGuaranteed) -> Self {
    TyKind::Error(err)
  }
}

impl From<ErrorGuaranteed> for Expr {
  fn from(err: ErrorGuaranteed) -> Self {
    Expr { span: Span::NONE, kind: Box::new(err.into()) }
  }
}

impl From<ErrorGuaranteed> for Stmt {
  fn from(err: ErrorGuaranteed) -> Self {
    Stmt { span: Span::NONE, kind: StmtKind::Expr(err.into(), false) }
  }
}

impl From<ErrorGuaranteed> for Block {
  fn from(err: ErrorGuaranteed) -> Self {
    Block { span: Span::NONE, stmts: Vec::from([err.into()]) }
  }
}

impl idx::IsEnabled for BinaryOp {}
impl idx::IsEnabled for ComparisonOp {}
