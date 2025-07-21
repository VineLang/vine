use std::{
  collections::BTreeMap,
  fmt::{self, Debug, Display, Write},
  path::PathBuf,
};

use ivy::ast::Net;
use vine_util::{idx, interner::Interned};

use crate::{features::builtin::Builtin, structures::diag::ErrorGuaranteed};

pub mod visit;

#[derive(Clone)]
pub struct Item<'core> {
  pub span: Span,
  pub vis: Vis<'core>,
  pub attrs: Vec<Attr<'core>>,
  pub kind: ItemKind<'core>,
}

#[derive(Debug, Clone)]
pub enum ItemKind<'core> {
  Fn(FnItem<'core>),
  Const(ConstItem<'core>),
  Struct(StructItem<'core>),
  Enum(EnumItem<'core>),
  Type(TypeItem<'core>),
  Mod(ModItem<'core>),
  Trait(TraitItem<'core>),
  Impl(ImplItem<'core>),
  Use(UseItem<'core>),
  Taken,
}

#[derive(Debug, Clone)]
pub struct FnItem<'core> {
  pub method: bool,
  pub name: Ident<'core>,
  pub generics: GenericParams<'core>,
  pub params: Vec<Pat<'core>>,
  pub ret: Option<Ty<'core>>,
  pub body: Option<Block<'core>>,
}

#[derive(Debug, Clone)]
pub struct ConstItem<'core> {
  pub name: Ident<'core>,
  pub generics: GenericParams<'core>,
  pub ty: Ty<'core>,
  pub value: Option<Expr<'core>>,
}

#[derive(Debug, Clone)]
pub struct TypeItem<'core> {
  pub name: Ident<'core>,
  pub generics: GenericParams<'core>,
  pub ty: Option<Ty<'core>>,
}

#[derive(Debug, Clone)]
pub struct StructItem<'core> {
  pub name: Ident<'core>,
  pub generics: GenericParams<'core>,
  pub data_vis: Vis<'core>,
  pub data: Ty<'core>,
}

#[derive(Debug, Clone)]
pub struct EnumItem<'core> {
  pub name: Ident<'core>,
  pub generics: GenericParams<'core>,
  pub variants: Vec<Variant<'core>>,
}

#[derive(Debug, Clone)]
pub struct Variant<'core> {
  pub name: Ident<'core>,
  pub data: Option<Ty<'core>>,
}

#[derive(Debug, Clone)]
pub struct ModItem<'core> {
  pub name: Ident<'core>,
  pub generics: GenericParams<'core>,
  pub kind: ModKind<'core>,
}

#[derive(Debug, Clone)]
pub enum ModKind<'core> {
  Loaded(Span, Vec<Item<'core>>),
  Unloaded(Span, PathBuf),
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone)]
pub struct TraitItem<'core> {
  pub name: Ident<'core>,
  pub generics: GenericParams<'core>,
  pub items: Vec<Item<'core>>,
}

#[derive(Debug, Clone)]
pub struct ImplItem<'core> {
  pub name: Ident<'core>,
  pub generics: GenericParams<'core>,
  pub trait_: Trait<'core>,
  pub kind: ImplItemKind<'core>,
}

#[derive(Debug, Clone)]
pub enum ImplItemKind<'core> {
  Direct(Vec<Item<'core>>),
  Indirect(Option<Impl<'core>>),
}

#[derive(Debug, Clone)]
pub struct UseItem<'core> {
  pub absolute: bool,
  pub tree: UseTree<'core>,
}

#[derive(Debug, Clone)]
pub struct UseTree<'core> {
  pub span: Span,
  pub aliases: Vec<Ident<'core>>,
  pub children: BTreeMap<Ident<'core>, UseTree<'core>>,
}

impl<'core> UseTree<'core> {
  pub fn empty(span: Span) -> Self {
    UseTree { span, aliases: Vec::new(), children: BTreeMap::new() }
  }

  pub fn prune(&mut self) -> bool {
    self.children.retain(|_, tree| tree.prune());
    !self.aliases.is_empty() || !self.children.is_empty()
  }
}

#[derive(Debug, Clone)]
pub enum Vis<'core> {
  Private,
  Public,
  PublicTo(Span, Ident<'core>),
}

#[derive(Clone)]
pub struct Attr<'core> {
  pub span: Span,
  pub kind: AttrKind<'core>,
}

#[derive(Debug, Clone)]
pub enum AttrKind<'core> {
  Builtin(Builtin),
  Main,
  Manual,
  Basic,
  Become(Path<'core>),
}

pub type GenericParams<'core> = Generics<TypeParam<'core>, ImplParam<'core>>;
pub type GenericArgs<'core> = Generics<Ty<'core>, Impl<'core>>;

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

#[derive(Debug, Clone, Copy)]
pub struct TypeParam<'core> {
  pub span: Span,
  pub name: Ident<'core>,
  pub flex: Flex,
}

#[derive(Debug, Clone)]
pub struct ImplParam<'core> {
  pub span: Span,
  pub name: Option<Ident<'core>>,
  pub trait_: Trait<'core>,
}

#[derive(Debug, Clone, Copy)]
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
}

#[derive(Debug, Clone)]
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
  LetFn(LetFnStmt<'core>),
  Expr(Expr<'core>, bool),
  Item(Item<'core>),
  Return(Option<Expr<'core>>),
  Break(Target<'core>, Option<Expr<'core>>),
  Continue(Target<'core>),
  Empty,
}

#[derive(Debug, Clone)]
pub struct LetStmt<'core> {
  pub bind: Pat<'core>,
  pub init: Option<Expr<'core>>,
  pub else_: Option<LetElse<'core>>,
}

#[derive(Debug, Clone)]
pub enum LetElse<'core> {
  Block(Block<'core>),
  Match(Vec<(Pat<'core>, Block<'core>)>),
}

#[derive(Debug, Clone)]
pub struct LetFnStmt<'core> {
  pub flex: Flex,
  pub name: Ident<'core>,
  pub params: Vec<Pat<'core>>,
  pub ret: Option<Ty<'core>>,
  pub body: Block<'core>,
}

#[derive(Clone)]
pub struct Expr<'core> {
  pub span: Span,
  pub kind: Box<ExprKind<'core>>,
}

#[derive(Debug, Clone)]
pub enum ExprKind<'core> {
  Hole,
  Paren(Expr<'core>),
  Path(Path<'core>, Option<Vec<Expr<'core>>>),
  Do(Label<'core>, Option<Ty<'core>>, Block<'core>),
  Assign(bool, Expr<'core>, Expr<'core>),
  Match(Expr<'core>, Option<Ty<'core>>, Vec<(Pat<'core>, Block<'core>)>),
  If(Expr<'core>, Option<Ty<'core>>, Block<'core>, Option<Block<'core>>),
  When(Label<'core>, Option<Ty<'core>>, Vec<(Expr<'core>, Block<'core>)>, Option<Block<'core>>),
  While(Label<'core>, Expr<'core>, Option<Ty<'core>>, Block<'core>, Option<Block<'core>>),
  Loop(Label<'core>, Option<Ty<'core>>, Block<'core>),
  For(Label<'core>, Pat<'core>, Expr<'core>, Option<Ty<'core>>, Block<'core>, Option<Block<'core>>),
  Fn(Flex, Vec<Pat<'core>>, Option<Ty<'core>>, Block<'core>),
  Ref(Expr<'core>, bool),
  Deref(Expr<'core>, bool),
  Inverse(Expr<'core>, bool),
  Place(Expr<'core>, Expr<'core>),
  Tuple(Vec<Expr<'core>>),
  Object(Vec<(Key<'core>, Expr<'core>)>),
  List(Vec<Expr<'core>>),
  TupleField(Expr<'core>, usize),
  ObjectField(Expr<'core>, Key<'core>),
  Method(Expr<'core>, Ident<'core>, GenericArgs<'core>, Vec<Expr<'core>>),
  Call(Expr<'core>, Vec<Expr<'core>>),
  Neg(Expr<'core>),
  BinaryOp(BinaryOp, Expr<'core>, Expr<'core>),
  Bool(bool),
  Not(Expr<'core>),
  Is(Expr<'core>, Pat<'core>),
  LogicalOp(LogicalOp, Expr<'core>, Expr<'core>),
  ComparisonOp(Expr<'core>, Vec<(ComparisonOp, Expr<'core>)>),
  BinaryOpAssign(BinaryOp, Expr<'core>, Expr<'core>),
  Cast(Expr<'core>, Ty<'core>, bool),
  Unwrap(Expr<'core>),
  Try(Expr<'core>),
  RangeExclusive(Option<Expr<'core>>, Option<Expr<'core>>),
  RangeInclusive(Option<Expr<'core>>, Expr<'core>),
  N32(u32),
  I32(i32),
  F32(f32),
  Char(char),
  String(StringSegment, Vec<(Expr<'core>, StringSegment)>),
  InlineIvy(Vec<(Ident<'core>, bool, Expr<'core>)>, Ty<'core>, Span, Net),
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone, Copy)]
pub struct Label<'core>(pub Option<Ident<'core>>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Target<'core> {
  AnyLoop,
  Label(Ident<'core>),
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

#[derive(Clone)]
pub struct Pat<'core> {
  pub span: Span,
  pub kind: Box<PatKind<'core>>,
}

#[derive(Debug, Clone)]
pub enum PatKind<'core> {
  Hole,
  Paren(Pat<'core>),
  Annotation(Pat<'core>, Ty<'core>),
  Path(Path<'core>, Option<Vec<Pat<'core>>>),
  Ref(Pat<'core>),
  Deref(Pat<'core>),
  Inverse(Pat<'core>),
  Tuple(Vec<Pat<'core>>),
  Object(Vec<(Key<'core>, Pat<'core>)>),
  Error(ErrorGuaranteed),
}

#[derive(Debug, Clone)]
pub struct Path<'core> {
  pub span: Span,
  pub absolute: bool,
  pub segments: Vec<Ident<'core>>,
  pub generics: Option<GenericArgs<'core>>,
}

impl<'core> Path<'core> {
  pub fn as_ident(&self) -> Option<Ident<'core>> {
    if self.generics.is_none() {
      if let [ident] = self.segments[..] {
        return Some(ident);
      }
    }
    None
  }
}

#[derive(Clone)]
pub struct Ty<'core> {
  pub span: Span,
  pub kind: Box<TyKind<'core>>,
}

#[derive(Debug, Clone)]
pub enum TyKind<'core> {
  Hole,
  Never,
  Paren(Ty<'core>),
  Fn(Path<'core>),
  Tuple(Vec<Ty<'core>>),
  Object(Vec<(Key<'core>, Ty<'core>)>),
  Ref(Ty<'core>),
  Inverse(Ty<'core>),
  Path(Path<'core>),
  Error(ErrorGuaranteed),
}

#[derive(Clone)]
pub struct Impl<'core> {
  pub span: Span,
  pub kind: Box<ImplKind<'core>>,
}

#[derive(Debug, Clone)]
pub enum ImplKind<'core> {
  Hole,
  Path(Path<'core>),
  Fn(Path<'core>),
  Error(ErrorGuaranteed),
}

#[derive(Clone)]
pub struct Trait<'core> {
  pub span: Span,
  pub kind: Box<TraitKind<'core>>,
}

#[derive(Debug, Clone)]
pub enum TraitKind<'core> {
  Path(Path<'core>),
  Fn(Ty<'core>, Vec<Ty<'core>>, Option<Ty<'core>>),
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident<'core>(pub Interned<'core, str>);

#[derive(Debug, Clone, Copy)]
pub struct Key<'core> {
  pub span: Span,
  pub ident: Ident<'core>,
}

impl Display for Ident<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0 .0)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
  pub file: usize,
  pub start: usize,
  pub end: usize,
}

impl Span {
  pub const NONE: Span = Span { file: usize::MAX, start: 0, end: 0 };
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

impl<'core> From<Key<'core>> for Path<'core> {
  fn from(key: Key<'core>) -> Self {
    Path { span: key.span, absolute: false, segments: Vec::from([key.ident]), generics: None }
  }
}

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

debug_kind!(Item<'_>, Attr<'_>, Stmt<'_>, Expr<'_>, Pat<'_>, Ty<'_>, Impl<'_>, Trait<'_>);

impl From<ErrorGuaranteed> for ExprKind<'_> {
  fn from(err: ErrorGuaranteed) -> Self {
    ExprKind::Error(err)
  }
}

impl From<ErrorGuaranteed> for PatKind<'_> {
  fn from(err: ErrorGuaranteed) -> Self {
    PatKind::Error(err)
  }
}

impl From<ErrorGuaranteed> for TyKind<'_> {
  fn from(err: ErrorGuaranteed) -> Self {
    TyKind::Error(err)
  }
}

impl From<ErrorGuaranteed> for Expr<'_> {
  fn from(err: ErrorGuaranteed) -> Self {
    Expr { span: Span::NONE, kind: Box::new(err.into()) }
  }
}

impl From<ErrorGuaranteed> for Stmt<'_> {
  fn from(err: ErrorGuaranteed) -> Self {
    Stmt { span: Span::NONE, kind: StmtKind::Expr(err.into(), false) }
  }
}

impl From<ErrorGuaranteed> for Block<'_> {
  fn from(err: ErrorGuaranteed) -> Self {
    Block { span: Span::NONE, stmts: Vec::from([err.into()]) }
  }
}

impl idx::IsEnabled for BinaryOp {}
impl idx::IsEnabled for ComparisonOp {}
