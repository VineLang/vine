use super::*;

pub use crate::feature::path::Path;

use crate::feature::{
  annotation::{ExprAnnotation, ImplAnnotation, PatAnnotation},
  builtin::AttrBuiltin,
  call::{ExprCall, TraitFn},
  control::{
    assert::StmtAssert,
    closure::{ExprClosure, StmtClosure},
    condition::{ExprAll, ExprAny, ExprImplies, ExprIs, ExprNot, PatAny, PatCond, PatIs},
    do_::ExprDo,
    for_::ExprFor,
    if_::ExprIf,
    if_const::{ExprIfConst, TyIfConst},
    label::{StmtBreak, StmtContinue},
    let_::StmtLet,
    loop_::ExprLoop,
    match_::ExprMatch,
    try_::ExprTry,
    when::ExprWhen,
    while_::ExprWhile,
  },
  enum_::{ExprVariant, PatVariant, TyEnum},
  field::{ExprField, TyField},
  hole::{ExprHole, ImplHole, PatHole, TraitHole, TyHole},
  inline_ivy::ExprInlineIvy,
  inverse::{ExprInverse, PatInverse, TyInverse},
  item::{
    const_::{AttrConfigurable, ItemConst},
    fn_::{AttrFrameless, AttrTest, ItemFn, TyFnPath},
    impl_::{AttrBasic, AttrBecome, AttrManual, ItemImpl},
    mod_::ItemMod,
    trait_::ItemTrait,
    type_::{AttrImpl, AttrSelfDual, ItemType},
    use_::ItemUse,
  },
  list::{ExprList, PatList},
  literal::ExprLiteral,
  loan::{ExprBorrow, ExprLoan, PatBorrow, PatLoan, TyBorrow, TyLoan},
  method::ExprMethod,
  object::{ExprObject, PatObject, TyObject},
  operator::{
    ExprBinaryOp, ExprCast, ExprComparisonOp, ExprIndex, ExprRange, ExprUnaryOp, PatComparisonOp,
    StmtAssign,
  },
  path::{ExprPath, ImplPath, PatPath, TraitPath, TyPath},
  placement::ExprPlacement,
  read::ExprRead,
  string::{ExprString, PatString},
  tuple::{ExprTuple, PatTuple, TyTuple},
  underscore::{ExprUnderscore, ImplAuto, ImplUnderscore, PatUnderscore, TyUnderscore},
  unsafe_::{ExprSafe, PatSafe},
  unwrap::{ExprUnwrap, TyUnwrap},
};

use std::ops::Range;

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
pub struct Span {
  pub start: usize,
  pub end: usize,
}

#[derive_(Debug, Clone, Copy)]
pub struct Spanned<T> {
  pub span: Span,
  pub inner: T,
}

impl<T: Visit> Visit for Spanned<T> {
  fn visit_children<'a>(&'a self, visitor: &mut impl Visitor<'a>) {
    visitor.visit(&self.span);
    visitor.visit(&self.inner);
  }

  fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl VisitorMut<'a>) {
    visitor.visit(&mut self.span);
    visitor.visit(&mut self.inner);
  }
}

#[derive_(Debug, Clone, Visit!)]
pub struct Item {
  pub range: Range<usize>,
  pub docs: Vec<String>,
  pub attrs: Vec<Attr>,
  pub vis: Vis,
  pub unsafe_: bool,
  pub kind: Box<ItemKind>,
}

#[derive_(Debug, Clone, Visit!)]
pub enum ItemKind {
  Use(ItemUse),
  Mod(ItemMod),
  Fn(ItemFn),
  Const(ItemConst),
  Type(ItemType),
  Trait(ItemTrait),
  Impl(ItemImpl),
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
pub struct Block {
  pub span: Span,
  pub pat: Option<Pat>,
  pub stmts: Children<Stmt>,
  pub close_brace: Spanned<()>,
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
}

#[derive_(Debug, Clone, Visit!)]
pub struct StmtExpr {
  pub expr: Expr,
  /// Whether this expression statement is terminated by a semicolon.
  pub semi: bool,
}

#[derive_(Debug, Clone, Visit!)]
pub struct Expr {
  pub span: Span,
  pub kind: Box<ExprKind>,
  pub type_: Resolve<Type>,
}

#[derive_(Debug, Clone, From, Visit!, for_enum!)]
pub enum ExprKind {
  Hole(ExprHole),
  Underscore(ExprUnderscore),

  Path(ExprPath),
  Unwrap(ExprUnwrap),
  Field(ExprField),
  Method(ExprMethod),
  Call(ExprCall),
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
  Range(ExprRange),

  Literal(ExprLiteral),
  String(ExprString),
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
  Hole(PatHole),
  Underscore(PatUnderscore),

  Path(PatPath),
  Safe(PatSafe),
  Annotation(PatAnnotation),

  Is(PatIs),
  Any(PatAny),
  Cond(PatCond),

  Loan(PatLoan),
  Borrow(PatBorrow),
  Inverse(PatInverse),

  Variant(PatVariant),
  Tuple(PatTuple),
  Object(PatObject),
  List(PatList),

  Comparison(PatComparisonOp),
  String(PatString),
}

#[derive_(Debug, Clone, Visit!)]
pub struct Ty {
  pub span: Span,
  pub kind: Box<TyKind>,
  pub type_: Resolve<Type>,
}

#[derive_(Debug, Clone, From, Visit!, for_enum!)]
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

#[derive_(Debug, Clone, Visit!)]
pub struct Impl {
  pub span: Span,
  pub kind: Box<ImplKind>,
  pub type_: Resolve<ImplType>,
}

#[derive_(Debug, Clone, From, Visit!, for_enum!)]
pub enum ImplKind {
  Hole(ImplHole),
  Underscore(ImplUnderscore),
  Path(ImplPath),
  Auto(ImplAuto),
  Annotation(ImplAnnotation),
}

#[derive_(Debug, Clone, Visit!)]
pub struct Trait {
  pub span: Span,
  pub kind: Box<TraitKind>,
  pub type_: Resolve<ImplType>,
}

#[derive_(Debug, Clone, From, Visit!, for_enum!)]
pub enum TraitKind {
  Hole(TraitHole),
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
#[derive_(Debug, Clone)]
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

/// A tuple-like collection of children, with regular entries and spreads.
pub type Sequence<T> = Children<SequenceChild<T>>;

#[derive_(Debug, Clone)]
pub enum SequenceChild<T> {
  Entry {
    /// Can be present in tuple types; serves solely as documentation.
    label: Option<Spanned<Ident>>,
    child: T,
  },
  Spread(T),
}

impl<T: Visit> Visit for SequenceChild<T> {
  fn visit_children<'a>(&'a self, visitor: &mut impl Visitor<'a>) {
    match self {
      SequenceChild::Entry { label, child } => {
        visitor.visit(label);
        visitor.visit(child);
      }
      SequenceChild::Spread(child) => visitor.visit(child),
    }
  }

  fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl VisitorMut<'a>) {
    match self {
      SequenceChild::Entry { label, child } => {
        visitor.visit(label);
        visitor.visit(child);
      }
      SequenceChild::Spread(child) => visitor.visit(child),
    }
  }
}

/// An object-like key-value collection, with key-value pairs and spreads.
#[derive_(Debug, Clone)]
pub struct Object<T> {
  /// Whether the entries are specified in order.
  pub ordered: bool,
  pub children: Children<ObjectChild<T>>,
}

impl<T: Visit> Visit for Object<T> {
  fn visit_children<'a>(&'a self, visitor: &mut impl Visitor<'a>) {
    visitor.visit(&self.children);
  }

  fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl VisitorMut<'a>) {
    visitor.visit(&mut self.children);
  }
}

#[derive_(Debug, Clone)]
pub enum ObjectChild<T> {
  Entry { key: Spanned<Ident>, value: T },
  Spread(T),
}

impl<T: Visit> Visit for ObjectChild<T> {
  fn visit_children<'a>(&'a self, visitor: &mut impl Visitor<'a>) {
    match self {
      ObjectChild::Entry { key, value } => {
        visitor.visit(key);
        visitor.visit(value);
      }
      ObjectChild::Spread(child) => visitor.visit(child),
    }
  }

  fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl VisitorMut<'a>) {
    match self {
      ObjectChild::Entry { key, value } => {
        visitor.visit(key);
        visitor.visit(value);
      }
      ObjectChild::Spread(child) => visitor.visit(child),
    }
  }
}

use crate::component::parser::{ParseKind, Parser};

impl ParseKind<Stmt, StmtLet> for Parser<'_> {}
impl ParseKind<Stmt, StmtClosure> for Parser<'_> {}
impl ParseKind<Stmt, StmtAssert> for Parser<'_> {}
impl ParseKind<Stmt, StmtAssign> for Parser<'_> {}
impl ParseKind<Stmt, StmtBreak> for Parser<'_> {}
impl ParseKind<Stmt, StmtContinue> for Parser<'_> {}
impl ParseKind<Stmt, StmtExpr> for Parser<'_> {}
impl ParseKind<Expr, ExprHole> for Parser<'_> {}
impl ParseKind<Expr, ExprUnderscore> for Parser<'_> {}
impl ParseKind<Expr, ExprPath> for Parser<'_> {}
impl ParseKind<Expr, ExprUnwrap> for Parser<'_> {}
impl ParseKind<Expr, ExprField> for Parser<'_> {}
impl ParseKind<Expr, ExprMethod> for Parser<'_> {}
impl ParseKind<Expr, ExprCall> for Parser<'_> {}
impl ParseKind<Expr, ExprSafe> for Parser<'_> {}
impl ParseKind<Expr, ExprVariant> for Parser<'_> {}
impl ParseKind<Expr, ExprTuple> for Parser<'_> {}
impl ParseKind<Expr, ExprObject> for Parser<'_> {}
impl ParseKind<Expr, ExprList> for Parser<'_> {}
impl ParseKind<Expr, ExprAnnotation> for Parser<'_> {}
impl ParseKind<Expr, ExprLoan> for Parser<'_> {}
impl ParseKind<Expr, ExprBorrow> for Parser<'_> {}
impl ParseKind<Expr, ExprInverse> for Parser<'_> {}
impl ParseKind<Expr, ExprPlacement> for Parser<'_> {}
impl ParseKind<Expr, ExprRead> for Parser<'_> {}
impl ParseKind<Expr, ExprDo> for Parser<'_> {}
impl ParseKind<Expr, ExprMatch> for Parser<'_> {}
impl ParseKind<Expr, ExprIf> for Parser<'_> {}
impl ParseKind<Expr, ExprIfConst> for Parser<'_> {}
impl ParseKind<Expr, ExprWhen> for Parser<'_> {}
impl ParseKind<Expr, ExprWhile> for Parser<'_> {}
impl ParseKind<Expr, ExprLoop> for Parser<'_> {}
impl ParseKind<Expr, ExprFor> for Parser<'_> {}
impl ParseKind<Expr, ExprClosure> for Parser<'_> {}
impl ParseKind<Expr, ExprTry> for Parser<'_> {}
impl ParseKind<Expr, ExprIs> for Parser<'_> {}
impl ParseKind<Expr, ExprNot> for Parser<'_> {}
impl ParseKind<Expr, ExprAny> for Parser<'_> {}
impl ParseKind<Expr, ExprAll> for Parser<'_> {}
impl ParseKind<Expr, ExprImplies> for Parser<'_> {}
impl ParseKind<Expr, ExprBinaryOp> for Parser<'_> {}
impl ParseKind<Expr, ExprComparisonOp> for Parser<'_> {}
impl ParseKind<Expr, ExprUnaryOp> for Parser<'_> {}
impl ParseKind<Expr, ExprCast> for Parser<'_> {}
impl ParseKind<Expr, ExprIndex> for Parser<'_> {}
impl ParseKind<Expr, ExprRange> for Parser<'_> {}
impl ParseKind<Expr, ExprLiteral> for Parser<'_> {}
impl ParseKind<Expr, ExprString> for Parser<'_> {}
impl ParseKind<Expr, ExprInlineIvy> for Parser<'_> {}
impl ParseKind<Pat, PatHole> for Parser<'_> {}
impl ParseKind<Pat, PatUnderscore> for Parser<'_> {}
impl ParseKind<Pat, PatPath> for Parser<'_> {}
impl ParseKind<Pat, PatSafe> for Parser<'_> {}
impl ParseKind<Pat, PatAnnotation> for Parser<'_> {}
impl ParseKind<Pat, PatIs> for Parser<'_> {}
impl ParseKind<Pat, PatAny> for Parser<'_> {}
impl ParseKind<Pat, PatCond> for Parser<'_> {}
impl ParseKind<Pat, PatLoan> for Parser<'_> {}
impl ParseKind<Pat, PatBorrow> for Parser<'_> {}
impl ParseKind<Pat, PatInverse> for Parser<'_> {}
impl ParseKind<Pat, PatVariant> for Parser<'_> {}
impl ParseKind<Pat, PatTuple> for Parser<'_> {}
impl ParseKind<Pat, PatObject> for Parser<'_> {}
impl ParseKind<Pat, PatList> for Parser<'_> {}
impl ParseKind<Pat, PatComparisonOp> for Parser<'_> {}
impl ParseKind<Pat, PatString> for Parser<'_> {}
impl ParseKind<Ty, TyHole> for Parser<'_> {}
impl ParseKind<Ty, TyUnderscore> for Parser<'_> {}
impl ParseKind<Ty, TyPath> for Parser<'_> {}
impl ParseKind<Ty, TyFnPath> for Parser<'_> {}
impl ParseKind<Ty, TyIfConst> for Parser<'_> {}
impl ParseKind<Ty, TyLoan> for Parser<'_> {}
impl ParseKind<Ty, TyInverse> for Parser<'_> {}
impl ParseKind<Ty, TyEnum> for Parser<'_> {}
impl ParseKind<Ty, TyTuple> for Parser<'_> {}
impl ParseKind<Ty, TyObject> for Parser<'_> {}
impl ParseKind<Ty, TyUnwrap> for Parser<'_> {}
impl ParseKind<Ty, TyField> for Parser<'_> {}
impl ParseKind<Ty, TyBorrow> for Parser<'_> {}
impl ParseKind<Impl, ImplHole> for Parser<'_> {}
impl ParseKind<Impl, ImplUnderscore> for Parser<'_> {}
impl ParseKind<Impl, ImplPath> for Parser<'_> {}
impl ParseKind<Impl, ImplAuto> for Parser<'_> {}
impl ParseKind<Impl, ImplAnnotation> for Parser<'_> {}
impl ParseKind<Trait, TraitHole> for Parser<'_> {}
impl ParseKind<Trait, TraitPath> for Parser<'_> {}
impl ParseKind<Trait, TraitFn> for Parser<'_> {}

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
