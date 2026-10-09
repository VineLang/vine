use vine_util::{apply, for_enum};

use derive_more::From;

#[derive(Debug, Clone, Copy)]
pub struct Type();
#[derive(Debug, Clone, Copy)]
pub struct Span();
#[derive(Debug, Clone, Copy)]
pub struct IdentValue();

#[derive(Debug, Clone)]
pub struct Ident {
  pub span: Span,
  pub value: IdentValue,
}

#[derive(Debug, Clone, Copy)]
pub struct Item {
  pub span: Span,
  pub kind: Box<ItemKind>,
}

#[derive(Debug, Clone, Copy)]
pub enum ItemKind {}

// #[derive(Clone)]
// pub struct Item {
//   pub span: Span,
//   pub name_span: Span,
//   pub docs: Vec<String>,
//   pub vis: Vis,
//   pub unsafe_: bool,
//   pub attrs: Vec<Attr>,
//   pub kind: ItemKind,
// }

// #[derive(Debug, Clone)]
// pub enum ItemKind {
//   Fn(FnItem),
//   Const(ConstItem),
//   Struct(StructItem),
//   Enum(EnumItem),
//   Type(TypeItem),
//   Mod(ModItem),
//   Trait(TraitItem),
//   Impl(ImplItem),
//   Use(UseItem),
//   OuterMod,
//   Taken,
// }

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
  Project(ExprProject),
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
  Safe(TySafe),
  Annotation(TyAnnotation),

  FnPath(TyFnPath),

  IfConst(TyIfConst),

  Loan(TyLoan),
  Borrow(TyBorrow),
  Inverse(TyInverse),

  Enum(TyEnum),
  Tuple(TyTuple),
  Object(TyObject),
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

pub struct Children<T> {
  pub entries: Vec<ChildrenEntry<T>>,
}

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

pub struct Sequence<T> {
  pub children: Children<SequenceChild<T>>,
}

pub enum SequenceChild<T> {
  Entry(T),
  Spread(T),
}

pub struct Object<T> {
  pub children: Children<ObjectChild<T>>,
}

pub enum ObjectChild<T> {
  Entry(Ident, T),
  Spread(T),
}
