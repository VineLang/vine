use super::*;

pub use crate::feature::{control::block::Block, path::Path};

use crate::feature::{
  annotation::Annotation,
  builtin::AttrBuiltin,
  call::{ExprCall, TraitFn},
  control::{
    assert::StmtAssert,
    block::StmtExpr,
    closure::{ExprClosure, StmtClosure},
    condition::{ExprAll, ExprAny, ExprImplies, ExprIs, ExprNot, PatAny, PatCond, PatIs},
    do_::ExprDo,
    for_::ExprFor,
    if_::{ExprIf, TyIfConst},
    label::{StmtBreak, StmtContinue},
    let_::StmtLet,
    loop_::ExprLoop,
    match_::ExprMatch,
    try_::ExprTry,
    when::ExprWhen,
    while_::ExprWhile,
  },
  enum_::{TyEnum, Variant},
  field::Field,
  hole::Hole,
  inline_ivy::ExprInlineIvy,
  inverse::Inverse,
  item::{
    const_::{AttrConfigurable, ItemConst},
    fn_::{AttrFrameless, AttrTest, ItemFn, TyFnPath},
    impl_::{AttrBasic, AttrBecome, AttrManual, ItemImpl},
    mod_::ItemMod,
    trait_::ItemTrait,
    type_::{AttrImpl, AttrSelfDual, ItemType},
    use_::ItemUse,
  },
  list::List,
  literal::ExprLiteral,
  loan::{Borrow, Loan},
  method::ExprMethod,
  object::Object,
  operator::{
    ExprBinaryOp, ExprCast, ExprComparisonOp, ExprIndex, ExprUnaryOp, PatComparisonOp, Range,
    StmtAssign,
  },
  path::{ExprPath, ImplPath, PatPath, TraitPath, TyPath},
  placement::Placement,
  read::Read,
  string::StringLiteral,
  tuple::Tuple,
  underscore::{ImplAuto, Underscore},
  unsafe_::Safe,
  unwrap::Unwrap,
};

use vine_util::{
  trivial_visit,
  visit::{Visitor, VisitorMut},
};

#[derive_(Debug, Clone, Copy)]
pub struct Type();
#[derive_(Debug, Clone, Copy)]
pub struct ImplType();
#[derive_(Debug, Clone)]
pub struct Ident(pub String);

new_idx!(pub FileId);
new_idx!(pub ItemId);

trivial_visit!(Span, Ident, ItemId);

pub struct Ast {
  pub files: IdxVec<FileId, File>,
}

pub struct File {
  pub items: IdxVec<ItemId, Item>,
}

#[derive_(Debug, Clone, Copy)]
pub struct FullSpan {
  pub file: FileId,
  pub item: ItemId,
  pub span: Span,
}

#[derive_(Debug, Clone, Copy)]
pub struct FileSpan {
  pub start: usize,
  pub end: usize,
}

#[derive_(Debug, Clone, Copy)]
pub struct Span {
  pub start: usize,
  pub end: usize,
}

#[derive_(Debug, Clone, Copy, Visit!)]
pub struct Spanned<T> {
  pub span: Span,
  pub inner: T,
}

impl From<Span> for Spanned<()> {
  fn from(span: Span) -> Self {
    Spanned { span, inner: () }
  }
}

#[derive_(Debug, Clone, Visit!)]
pub struct Item<I> {
  pub file_span: FileSpan,
  pub docs: Text,
  pub attrs: Vec<Attr>,
  pub vis: Vis,
  pub unsafe_: bool,
  pub kind: Box<ItemKind<I>>,
}

#[derive_(Debug, Clone, Visit!)]
pub enum ItemKind<I> {
  Use(ItemUse),
  Mod(ItemMod<I>),
  Fn(ItemFn),
  Const(ItemConst),
  Type(ItemType),
  Trait(ItemTrait<I>),
  Impl(ItemImpl<I>),
}

#[derive_(Debug, Clone, Visit!)]
pub struct Attr {
  pub span: Span,
  pub safe: bool,
  pub kind: AttrKind,
}

#[derive_(Debug, Clone, Visit!, for_enum!)]
pub enum AttrKind {
  Builtin(AttrBuiltin),
  Manual(AttrManual),
  Basic(AttrBasic),
  Become(AttrBecome),
  Frameless(AttrFrameless),
  Configurable(AttrConfigurable),
  Test(AttrTest),
  SelfDual(AttrSelfDual),
  Impl(AttrImpl),
}

#[derive_(Debug, Clone, Visit!)]
pub enum Vis {
  Private,
  Public,
  Within(Spanned<Ident>),
}

#[derive_(Debug, Clone, Visit!)]
pub struct Stmt {
  pub span: Span,
  pub kind: Box<StmtKind>,
}

#[derive_(Debug, Clone, From, Visit!, for_enum!)]
pub enum StmtKind {
  Let(StmtLet),
  Closure(StmtClosure),
  Assert(StmtAssert),
  Assign(StmtAssign),
  Break(StmtBreak),
  Continue(StmtContinue),
  Expr(StmtExpr),
  Hole(Hole),
}

#[derive_(Debug, Clone, Visit!)]
pub struct Expr {
  pub span: Span,
  pub kind: Box<ExprKind>,
  pub type_: Resolve<Type>,
}

#[derive_(Debug, Clone, From, Visit!, for_enum!)]
pub enum ExprKind {
  Hole(Hole),
  Underscore(Underscore),

  Path(ExprPath),
  Unwrap(Unwrap<Expr>),
  Field(Field<Expr>),
  Method(ExprMethod),
  Call(ExprCall),
  Safe(Safe<Expr>),

  Variant(Variant<Expr>),
  Tuple(Tuple<Expr>),
  Object(Object<Expr>),
  List(List<Expr>),
  Annotation(Annotation<Expr>),

  Loan(Loan<Expr>),
  Borrow(Borrow<Expr>),
  Inverse(Inverse<Expr>),
  Placement(Placement<Expr>),
  Read(Read<Expr>),

  // Control flow
  Do(ExprDo),
  Match(ExprMatch),
  If(ExprIf),
  When(ExprWhen),
  While(ExprWhile),
  Loop(ExprLoop),
  For(ExprFor),
  Closure(ExprClosure),
  Try(ExprTry),

  // Conditions
  Is(ExprIs),
  Not(ExprNot),
  Any(ExprAny),
  All(ExprAll),
  Implies(ExprImplies),

  // Trait call sugar
  BinaryOp(ExprBinaryOp),
  ComparisonOp(ExprComparisonOp),
  UnaryOp(ExprUnaryOp),
  Cast(ExprCast),
  Index(ExprIndex),
  Range(Range<Expr>),

  Literal(ExprLiteral),
  String(StringLiteral<Expr>),
  InlineIvy(ExprInlineIvy),
}

#[derive_(Debug, Clone, Visit!)]
pub struct Pat {
  pub span: Span,
  pub kind: Box<PatKind>,
  pub type_: Resolve<Type>,
}

#[derive_(Debug, Clone, From, Visit!, for_enum!)]
pub enum PatKind {
  Hole(Hole),
  Underscore(Underscore),

  Path(PatPath),
  Safe(Safe<Pat>),
  Annotation(Annotation<Pat>),

  Is(PatIs),
  Any(PatAny),
  Cond(PatCond),

  Loan(Loan<Pat>),
  Borrow(Borrow<Pat>),
  Inverse(Inverse<Pat>),
  Placement(Placement<Pat>),
  Read(Read<Pat>),

  Variant(Variant<Pat>),
  Tuple(Tuple<Pat>),
  Object(Object<Pat>),
  List(List<Pat>),

  Comparison(PatComparisonOp),
  String(StringLiteral<Pat>),
}

#[derive_(Debug, Clone, Visit!)]
pub struct Ty {
  pub span: Span,
  pub kind: Box<TyKind>,
  pub type_: Resolve<Type>,
}

#[derive_(Debug, Clone, From, Visit!, for_enum!)]
pub enum TyKind {
  Hole(Hole),
  Underscore(Underscore),

  Path(TyPath),

  FnPath(TyFnPath),
  IfConst(TyIfConst),

  Loan(Loan<Ty>),
  Inverse(Inverse<Ty>),

  Enum(TyEnum),
  Tuple(Tuple<Ty>),
  Object(Object<Ty>),

  Unwrap(Unwrap<Ty>),
  Field(Field<Ty>),
  Borrow(Borrow<Ty>),

  Range(Range<Ty>),
}

#[derive_(Debug, Clone, Visit!)]
pub struct Impl {
  pub span: Span,
  pub kind: Box<ImplKind>,
  pub type_: Resolve<ImplType>,
}

#[derive_(Debug, Clone, From, Visit!, for_enum!)]
pub enum ImplKind {
  Hole(Hole),
  Underscore(Underscore),
  Path(ImplPath),
  Auto(ImplAuto),
  Annotation(Annotation<Impl>),
}

#[derive_(Debug, Clone, Visit!)]
pub struct Trait {
  pub span: Span,
  pub kind: Box<TraitKind>,
  pub type_: Resolve<ImplType>,
}

#[derive_(Debug, Clone, From, Visit!, for_enum!)]
pub enum TraitKind {
  Hole(Hole),
  Path(TraitPath),
  Fn(TraitFn),
}

#[derive_(Debug, Clone, Copy)]
pub enum Resolve<T> {
  Resolved(T),
  Unresolved,
}

pub use Resolve::*;

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

impl<T: Debug + 'static> Visit for Resolve<T> {
  fn visit_children<'a>(&'a self, _: &mut impl Visitor<'a>) {}

  fn visit_children_mut<'a>(&'a mut self, _: &mut impl VisitorMut<'a>) {}
}

/// A textual comment, possibly spanning multiple lines.
#[derive_(Default, Debug, Clone, Visit!)]
pub struct Text {
  pub lines: Vec<String>,
}

/// A list of child nodes, potentially interspersed with blank lines and
/// comments.
#[derive_(Debug, Clone)]
pub struct Children<T> {
  /// These entries should be normalized such that:
  /// - there are no two adjacent `Blank` entries
  /// - `entries` does not begin or end with a `Blank` entry
  /// - there are no two adjacent `Text` entries
  /// - no `Text` entry immediately precedes a `Child` entry (as it would be
  ///   incorporated into the `docs`)
  ///
  /// Note that a `Text` entry can immediately succeed a `Child` entry, as the
  /// `trailing` is only textual comments starting on the same line, not
  /// including subsequent lines.
  pub entries: Vec<ChildrenEntry<T>>,
}

#[derive_(Debug, Clone)]
pub enum ChildrenEntry<T> {
  Child {
    /// Textual comments on lines immediately preceding the child; these may be
    /// interpreted as documentation for the child.
    docs: Text,
    child: T,
    /// Textual comments following the child, on the same line.
    trailing: Text,
  },
  Blank,
  Text(Text),
  Exclude {
    block: bool,
    /// Should be non-empty. If `block` is true, should have exactly one
    /// element.
    lines: Vec<String>,
  },
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

  pub fn with_docs(&self) -> Option<(&Text, &T)> {
    match self {
      ChildrenEntry::Child { docs, child, .. } => Some((docs, child)),
      _ => None,
    }
  }

  pub fn with_docs_mut(&mut self) -> Option<(&Text, &mut T)> {
    match self {
      ChildrenEntry::Child { docs, child, .. } => Some((docs, child)),
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

  pub fn with_docs(&self) -> impl DoubleEndedIterator<Item = (&Text, &T)> {
    self.entries.iter().filter_map(ChildrenEntry::with_docs)
  }

  pub fn with_docs_mut(&mut self) -> impl DoubleEndedIterator<Item = (&Text, &mut T)> {
    self.entries.iter_mut().filter_map(ChildrenEntry::with_docs_mut)
  }
}

impl<T> From<T> for ChildrenEntry<T> {
  fn from(child: T) -> Self {
    ChildrenEntry::Child { docs: Text::default(), child, trailing: Text::default() }
  }
}

impl<T: Visit> Visit for Children<T> {
  fn visit_children<'a>(&'a self, visitor: &mut impl Visitor<'a>) {
    for child in self.iter() {
      visitor.visit(child);
    }
  }

  fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl VisitorMut<'a>) {
    for child in self.iter_mut() {
      visitor.visit(child);
    }
  }
}

impl<T> Default for Children<T> {
  fn default() -> Self {
    Self { entries: Default::default() }
  }
}

/// A tuple-like collection of children, with regular entries and spreads.
pub type Sequence<T> = Children<SequenceChild<T>>;

#[derive_(Debug, Clone, Visit!)]
pub enum SequenceChild<T> {
  Entry {
    /// Can be present in tuple types; serves solely as documentation.
    label: Option<Spanned<Ident>>,
    child: T,
  },
  Spread(Option<T>),
}

impl Expr {
  pub fn new(span: Span, kind: impl Into<ExprKind>) -> Self {
    Expr { span, kind: Box::new(kind.into()), type_: Unresolved }
  }
}

impl Pat {
  pub fn new(span: Span, kind: impl Into<PatKind>) -> Self {
    Pat { span, kind: Box::new(kind.into()), type_: Unresolved }
  }
}

impl Ty {
  pub fn new(span: Span, kind: impl Into<TyKind>) -> Self {
    Ty { span, kind: Box::new(kind.into()), type_: Unresolved }
  }
}

impl Impl {
  pub fn new(span: Span, kind: impl Into<ImplKind>) -> Self {
    Impl { span, kind: Box::new(kind.into()), type_: Unresolved }
  }
}

impl Trait {
  pub fn new(span: Span, kind: impl Into<TraitKind>) -> Self {
    Trait { span, kind: Box::new(kind.into()), type_: Unresolved }
  }
}
