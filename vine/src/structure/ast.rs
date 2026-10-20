use super::*;

pub use crate::feature::{
  children::Children, control::block::Block, path::Path, sequence::Sequence,
};

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
  path::{ExprPathCall, ImplPath, PatPathCall, TraitPath, TyPath},
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

trivial_visit!(FileSpan, LocalSpan, Ident, ItemId);

/// An absolute span.
#[derive_(Debug, Clone, Copy)]
pub struct AbsSpan {
  pub file: FileId,
  pub item: ItemId,
  pub span: LocalSpan,
}

/// An file-relative span.
#[derive_(Debug, Clone, Copy)]
pub struct FileSpan {
  pub start: usize,
  pub end: usize,
}

/// An item-relative span.
#[derive_(Debug, Clone, Copy)]
pub struct LocalSpan {
  pub start: usize,
  pub end: usize,
}

#[derive_(Debug, Clone, Copy, Visit!)]
pub struct Spanned<T> {
  pub span: LocalSpan,
  pub inner: T,
}

impl From<LocalSpan> for Spanned<()> {
  fn from(span: LocalSpan) -> Self {
    Spanned { span, inner: () }
  }
}

#[derive_(Debug, Clone, Visit!)]
pub struct Item {
  pub file_span: FileSpan,
  pub attrs: Vec<Attr>,
  pub vis: Vis,
  pub unsafe_: Option<Spanned<()>>,
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
pub enum Subitem {
  Inline(Item),
  Extracted(ItemId),
}

#[derive_(Debug, Clone, Visit!)]
pub struct Attr {
  pub span: LocalSpan,
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
  pub span: LocalSpan,
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
pub struct Expr {
  pub span: LocalSpan,
  pub kind: Box<ExprKind>,
  pub type_: Resolve<Type>,
}

#[derive_(Debug, Clone, From, Visit!, for_enum!)]
pub enum ExprKind {
  Hole(Hole),
  Underscore(Underscore),

  PathCall(ExprPathCall),
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
  pub span: LocalSpan,
  pub kind: Box<PatKind>,
  pub type_: Resolve<Type>,
}

#[derive_(Debug, Clone, From, Visit!, for_enum!)]
pub enum PatKind {
  Hole(Hole),
  Underscore(Underscore),

  PathCall(PatPathCall),
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
  pub span: LocalSpan,
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
  pub span: LocalSpan,
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
  pub span: LocalSpan,
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

impl Expr {
  pub fn new(span: LocalSpan, kind: impl Into<ExprKind>) -> Self {
    Expr { span, kind: Box::new(kind.into()), type_: Unresolved }
  }
}

impl Pat {
  pub fn new(span: LocalSpan, kind: impl Into<PatKind>) -> Self {
    Pat { span, kind: Box::new(kind.into()), type_: Unresolved }
  }
}

impl Ty {
  pub fn new(span: LocalSpan, kind: impl Into<TyKind>) -> Self {
    Ty { span, kind: Box::new(kind.into()), type_: Unresolved }
  }
}

impl Impl {
  pub fn new(span: LocalSpan, kind: impl Into<ImplKind>) -> Self {
    Impl { span, kind: Box::new(kind.into()), type_: Unresolved }
  }
}

impl Trait {
  pub fn new(span: LocalSpan, kind: impl Into<TraitKind>) -> Self {
    Trait { span, kind: Box::new(kind.into()), type_: Unresolved }
  }
}
