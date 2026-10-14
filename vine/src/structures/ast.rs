use std::{
  any::{Any, TypeId},
  mem::transmute,
  ops::Range,
};

use vine_util::{apply, for_enum, idx::IdxVec, new_idx};

use derive_more::From;

#[derive(Debug, Clone, Copy)]
pub struct Type();
#[derive(Debug, Clone, Copy)]
pub struct IdentValue();

new_idx!(pub FileId);
new_idx!(pub ItemId);

pub struct Ast {
  pub files: IdxVec<FileId, File>,
}

pub struct File {
  pub items: IdxVec<ItemId, Item>,
}

#[derive(Debug, Clone, Copy)]
pub struct FullSpan {
  pub file: FileId,
  pub item: ItemId,
  pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
  pub start: usize,
  pub end: usize,
}

#[derive(Debug, Clone)]
pub struct SpanIdent {
  pub span: Span,
  pub ident: IdentValue,
}

#[derive(Debug, Clone)]
pub struct Item {
  pub range: Range<usize>,
  pub docs: Vec<String>,
  pub attrs: Vec<Attr>,
  pub vis: Vis,
  pub unsafe_: bool,
  pub kind: Box<ItemKind>,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
  Use(ItemUse),
  Mod(ItemMod),
  Fn(ItemFn),
  Const(ItemConst),
  Type(ItemType),
  Trait(ItemTrait),
  Impl(ItemImpl),
}

#[derive(Debug, Clone)]
pub struct ItemMod {
  pub name: SpanIdent,
  pub generics: GenericParams,
  pub kind: ItemModKind,
}

#[derive(Debug, Clone)]
pub enum ItemModKind {
  Local(Vec<ItemId>),
  External,
}

#[derive(Debug, Clone)]
pub struct ItemFn {
  pub method: bool,
  pub name: SpanIdent,
  pub generics: GenericParams,
  pub args: Sequence<Pat>,
  pub ret: Return,
  pub body: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct ItemConst {
  pub name: Ident,
  pub generics: GenericParams,
  pub ty: Ty,
  pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct ItemType {
  pub flex: Flex,
  pub name: Ident,
  pub generics: GenericParams,
  pub kind: ItemTypeKind,
}

#[derive(Debug, Clone)]
pub enum ItemTypeKind {
  Opaque,
  Alias(Ty),
  Wrapper(Vis, Ty),
}

#[derive(Debug, Clone)]
pub struct ItemTrait {
  pub name: Ident,
  pub inherent: Vec<Ident>,
  pub generics: GenericParams,
  pub items: Vec<ItemId>,
}

#[derive(Debug, Clone)]
pub struct ItemImpl {
  pub name: Ident,
  pub generics: GenericParams,
  pub trait_: Trait,
  pub kind: ItemImplKind,
}

#[derive(Debug, Clone)]
pub enum ItemImplKind {
  IndirectBasic,
  Indirect { impl_: Impl },
  Direct { items: Vec<ItemId> },
}

#[derive(Debug, Clone)]
pub enum Vis {
  Private,
  Public,
  Within(Ident),
}

#[derive(Debug, Clone)]
pub enum Return {
  Implicit,
  Ty(Ty),
  Pat(Pat),
}

#[derive(Debug, Clone)]
pub struct Expr {
  pub span: Span,
  pub kind: Box<ExprKind>,
  pub type_: Resolve<Type>,
}

#[apply(for_enum!)]
#[derive(Debug, Clone, From)]
pub enum ExprKind {
  Hole(ExprHole),
  Underscore(ExprUnderscore),

  Path(ExprPath),
  Unwrap(ExprUnwrap),
  Field(ExprField),
  Method(ExprMethod),
  Call(ExprCall),
  Assign(ExprAssign),
  Safe(ExprSafe),

  Variant(ExprVariant),
  Tuple(ExprTuple),
  Object(ExprObject),
  List(ExprList),
  Annotation(ExprAnnotation),

  Loan(ExprLoan),
  Borrow(ExprBorrow),
  Inverse(ExprInverse),
  Placement(ExprPlacement),
  Read(ExprRead),

  // Control flow
  Do(ExprDo),
  Match(ExprMatch),
  If(ExprIf),
  IfConst(ExprIfConst),
  When(ExprWhen),
  While(ExprWhile),
  Loop(ExprLoop),
  For(ExprFor),
  Fn(ExprFn),
  Try(ExprTry),

  // Conditions
  Is(ExprIs),
  Not(ExprNot),
  Any(ExprAny),
  All(ExprAll),
  Implies(ExprImplies),

  // Trait call sugar
  BinaryOp(ExprBinaryOp),
  BinaryOpAssign(ExprBinaryOpAssign),
  ComparisonOp(ExprComparisonOp),
  Sign(ExprSign),
  Cast(ExprCast),
  Index(ExprIndex),

  Literal(ExprLiteral),
  Range(ExprRange),
  String(ExprString),
  InlineIvy(ExprInlineIvy),
}

pub enum Literal {
  Char,
}

#[derive(Debug, Clone)]
pub struct Pat {
  pub span: Span,
  pub kind: Box<PatKind>,
  pub type_: Resolve<Type>,
}

#[apply(for_enum!)]
#[derive(Debug, Clone, From)]
pub enum PatKind {
  Hole(PatHole),
  Underscore(PatUnderscore),

  Path(PatPath),
  Safe(PatSafe),
  Annotation(PatAnnotation),

  Is(PatIs),
  Any(PatAny),
  If(PatIf),

  Loan(PatLoan),
  Borrow(PatBorrow),
  Inverse(PatInverse),

  Variant(PatVariant),
  Tuple(PatTuple),
  Object(PatObject),
  List(PatList),

  Literal(PatLiteral),
  Range(PatRange),
  String(PatString),
}

#[derive(Debug, Clone)]
pub struct Ty {
  pub span: Span,
  pub kind: Box<TyKind>,
  pub type_: Resolve<Type>,
}

#[apply(for_enum!)]
#[derive(Debug, Clone, From)]
pub enum TyKind {
  Hole(TyHole),
  Underscore(TyUnderscore),

  Path(TyPath),

  FnPath(TyFnPath),
  IfConst(TyIfConst),

  Loan(TyLoan),
  Inverse(TyInverse),

  Enum(TyEnum),
  Tuple(TyTuple),
  Object(TyObject),

  Unwrap(TyUnwrap),
  Field(TyField),
  Borrow(TyBorrow),
}

pub struct Text {
  pub lines: Vec<String>,
}

#[derive(Debug, Clone, Copy)]
pub enum Resolve<T> {
  Resolved(T),
  Unresolved,
}

pub use Resolve::*;

use crate::features::use_::ItemUse;

impl<T> Resolve<T> {
  pub fn resolved(self) -> T {
    match self {
      Resolved(value) => value,
      Unresolved => panic!("not yet resolved"),
    }
  }

  pub fn resolved_ref(&self) -> &T {
    self.as_ref().resolved()
  }

  pub fn resolved_mut(&mut self) -> &mut T {
    self.as_mut().resolved()
  }

  pub fn as_ref(&self) -> Resolve<&T> {
    match self {
      Resolved(value) => Resolved(value),
      Unresolved => Unresolved,
    }
  }

  pub fn as_mut(&mut self) -> Resolve<&mut T> {
    match self {
      Resolved(value) => Resolved(value),
      Unresolved => Unresolved,
    }
  }
}

#[derive(Debug, Clone)]
pub struct Children<T> {
  pub entries: Vec<ChildrenEntry<T>>,
}

#[derive(Debug, Clone)]
pub enum ChildrenEntry<T> {
  Blank,
  Text(Text),
  Exclude { block: bool, lines: Vec<String> },
  Child { pretext: Text, child: T, post_text: Text },
}

impl<T> ChildrenEntry<T> {
  pub fn get(&self) -> Option<&T> {
    match self {
      ChildrenEntry::Child { child, .. } => Some(child),
      _ => None,
    }
  }

  pub fn get_mut(&mut self) -> Option<&mut T> {
    match self {
      ChildrenEntry::Child { child, .. } => Some(child),
      _ => None,
    }
  }

  pub fn with_pretext(&self) -> Option<(&Text, &T)> {
    match self {
      ChildrenEntry::Child { pretext, child, .. } => Some((pretext, child)),
      _ => None,
    }
  }

  pub fn with_pretext_mut(&mut self) -> Option<(&Text, &mut T)> {
    match self {
      ChildrenEntry::Child { pretext, child, .. } => Some((pretext, child)),
      _ => None,
    }
  }
}

impl<T> Children<T> {
  pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> {
    self.entries.iter().filter_map(ChildrenEntry::get)
  }

  pub fn iter_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut T> {
    self.entries.iter_mut().filter_map(ChildrenEntry::get_mut)
  }

  pub fn with_pretext(&self) -> impl DoubleEndedIterator<Item = (&Text, &T)> {
    self.entries.iter().filter_map(ChildrenEntry::with_pretext)
  }

  pub fn with_pretext_mut(&mut self) -> impl DoubleEndedIterator<Item = (&Text, &mut T)> {
    self.entries.iter_mut().filter_map(ChildrenEntry::with_pretext_mut)
  }
}

#[derive(Debug, Clone)]
pub struct Sequence<T> {
  pub children: Children<SequenceChild<T>>,
}

#[derive(Debug, Clone)]
pub enum SequenceChild<T> {
  Entry(T),
  Spread(T),
}

#[derive(Debug, Clone)]
pub struct Object<T> {
  pub children: Children<ObjectChild<T>>,
}

#[derive(Debug, Clone)]
pub enum ObjectChild<T> {
  Entry(Ident, T),
  Spread(T),
}
