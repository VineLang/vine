use std::{
  collections::BTreeMap,
  fmt::{self, Debug, Display, Write},
  mem::take,
  path::PathBuf,
};

use class::Classes;
use ivy::ast::Net;
use vine_util::{interner::Interned, new_idx};

use crate::{
  chart::{AdtId, ImplDefId, TraitDefId, TypeDefId, ValueDefId, VariantId},
  diag::ErrorGuaranteed,
  specializer::RelId,
};

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
  Enum(EnumItem<'core>),
  Type(TypeItem<'core>),
  Mod(ModItem<'core>),
  Trait(TraitItem<'core>),
  Impl(ImplItem<'core>),
  Use(UseItem<'core>),
  Ivy(InlineIvy<'core>),
  #[default]
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
  pub ty: Ty<'core>,
}

#[derive(Debug, Clone)]
pub struct StructItem<'core> {
  pub name: Ident<'core>,
  pub generics: GenericParams<'core>,
  pub fields: Vec<Ty<'core>>,
  pub object: bool,
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
  pub items: Vec<Item<'core>>,
}

#[derive(Debug, Clone)]
pub struct UseItem<'core> {
  pub absolute: bool,
  pub tree: UseTree<'core>,
}

#[derive(Debug, Clone, Default)]
pub struct UseTree<'core> {
  pub span: Span,
  pub aliases: Vec<Ident<'core>>,
  pub children: BTreeMap<Ident<'core>, UseTree<'core>>,
}

impl<'core> UseTree<'core> {
  pub fn prune(&mut self) -> bool {
    self.children.retain(|_, tree| tree.prune());
    !self.aliases.is_empty() || !self.children.is_empty()
  }
}

#[derive(Debug, Clone)]
pub struct InlineIvy<'core> {
  pub method: bool,
  pub name: Ident<'core>,
  pub generics: GenericParams<'core>,
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
  String,
  Concat,
  ToStringTrait,
  ToStringFn,
}

pub type GenericParams<'core> = Generics<Ident<'core>, (Option<Ident<'core>>, Trait<'core>)>;
pub type GenericArgs<'core> = Generics<Ty<'core>, Impl<'core>>;

#[derive(Debug, Clone)]
pub struct Generics<T, I> {
  pub span: Span,
  pub types: Vec<T>,
  pub impls: Vec<I>,
}

impl<T, I> Default for Generics<T, I> {
  fn default() -> Self {
    Self { span: Span::default(), types: Default::default(), impls: Default::default() }
  }
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
  pub init: Option<Expr<'core>>,
  pub else_block: Option<Block<'core>>,
}

#[derive(Debug, Clone)]
pub struct DynFnStmt<'core> {
  pub name: Ident<'core>,
  pub id: Option<DynFnId>,
  pub params: Vec<Pat<'core>>,
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
  Path(Path<'core>),
  #[class(value, resolved)]
  Def(ValueDefId, GenericArgs<'core>),
  #[class(value, synthetic)]
  Rel(RelId),
  #[class(place, resolved)]
  Local(Local),
  #[class(value, resolved)]
  DynFn(DynFnId),
  #[class(value)]
  Do(Label<'core>, Block<'core>),
  #[class(value)]
  Assign(bool, B<Expr<'core>>, B<Expr<'core>>),
  #[class(value)]
  Match(B<Expr<'core>>, Vec<(Pat<'core>, Block<'core>)>),
  #[class(value)]
  If(Vec<(Expr<'core>, Block<'core>)>, Option<Block<'core>>),
  #[class(value)]
  While(Label<'core>, B<Expr<'core>>, Block<'core>),
  #[class(value)]
  Loop(Label<'core>, Block<'core>),
  #[class(value)]
  Fn(Vec<Pat<'core>>, Option<Option<Ty<'core>>>, Block<'core>),
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
  #[class(value, place, space)]
  Object(Vec<(Key<'core>, Expr<'core>)>),
  #[class(value)]
  List(Vec<Expr<'core>>),
  #[class(value, place)]
  TupleField(B<Expr<'core>>, usize, Option<usize>),
  #[class(value, place, sugar)]
  ObjectField(B<Expr<'core>>, Key<'core>),
  #[class(value, sugar)]
  Method(B<Expr<'core>>, Ident<'core>, GenericArgs<'core>, Vec<Expr<'core>>),
  #[class(value)]
  Call(B<Expr<'core>>, Vec<Expr<'core>>),
  #[class(value, place, space, resolved)]
  Adt(AdtId, VariantId, GenericArgs<'core>, Vec<Expr<'core>>),
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
  String(StringSegment, Vec<(Expr<'core>, StringSegment)>),
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

#[derive(Default, Debug, Clone)]
pub struct StringSegment {
  pub content: String,
  pub span: Span,
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
  Annotation(B<Pat<'core>>, B<Ty<'core>>),
  #[class(value, place, space)]
  PathCall(Path<'core>, Option<Vec<Pat<'core>>>),
  #[class(value, place, space)]
  Adt(AdtId, VariantId, GenericArgs<'core>, Vec<Pat<'core>>),
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
  #[class(value, place, space)]
  Object(Vec<(Key<'core>, Pat<'core>)>),
  #[class(error)]
  Error(ErrorGuaranteed),
}

#[derive(Default, Debug, Clone)]
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

  pub fn take_generics(&mut self) -> GenericArgs<'core> {
    self.generics.take().unwrap_or(Generics {
      span: self.span,
      types: Vec::new(),
      impls: Vec::new(),
    })
  }
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
  Object(Vec<(Key<'core>, Ty<'core>)>),
  Ref(B<Ty<'core>>),
  Inverse(B<Ty<'core>>),
  Path(Path<'core>),
  Param(usize),
  Def(TypeDefId, GenericArgs<'core>),
  Error(ErrorGuaranteed),
}

#[derive(Clone)]
pub struct Impl<'core> {
  pub span: Span,
  pub kind: ImplKind<'core>,
}

#[derive(Debug, Clone)]
pub enum ImplKind<'core> {
  Hole,
  Param(usize),
  Path(Path<'core>),
  Def(ImplDefId, GenericArgs<'core>),
  Error(ErrorGuaranteed),
}

#[derive(Clone)]
pub struct Trait<'core> {
  pub span: Span,
  pub kind: TraitKind<'core>,
}

#[derive(Debug, Clone)]
pub enum TraitKind<'core> {
  Path(Path<'core>),
  Def(TraitDefId, GenericArgs<'core>),
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

impl Default for Span {
  fn default() -> Self {
    Span::NONE
  }
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

debug_kind!(Item<'_>, Attr, Stmt<'_>, Expr<'_>, Pat<'_>, Ty<'_>, Impl<'_>, Trait<'_>);

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
    Expr { span: Span::NONE, kind: err.into() }
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
