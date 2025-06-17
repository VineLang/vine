use std::{
  cell::Ref,
  fmt::{self, Display, Write},
  io,
  mem::take,
  path::PathBuf,
};

use vine_util::lexer::TokenSet;

use crate::{
  components::lexer::{StrToken, Token},
  structures::{
    ast::{BinaryOp, Ident, Span},
    core::Core,
  },
};

macro_rules! diags {
  ($(
    $name:ident $({ $($field:ident: $ty:ty),* $(,)? })?
      [$($fmt:tt)*]
  )*) => {
    #[derive(Debug)]
    pub enum Diag<'core> {
      $( $name { span: Span, $($($field: $ty),*)? },)*
      Guaranteed(ErrorGuaranteed),
    }

    impl<'core> Diag<'core> {
      pub fn span(&self) -> Option<Span> {
        match self {
          $( Self::$name { span, .. } => Some(*span), )*
          Diag::Guaranteed(_) => None,
        }
      }
    }

    impl<'core> Display for Diag<'core> {
      fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
          $( Self::$name { span: _, $($($field),*)? } => write!(f, $($fmt)*),)*
          Diag::Guaranteed(_) => unreachable!(),
        }
      }
    }
  };
}

diags! {
  FsError { path: PathBuf, err: io::Error }
    ["cannot read file `{path}`: {err}", path = path.display()]
  LexError
    ["lexing error"]
  UnexpectedToken { expected: TokenSet<Token>, found: Option<Token> }
    ["expected one of {expected:?}; found {found:?}"]
  UnexpectedStringToken { found: Option<StrToken> }
    ["unexpected token {found:?} inside string"]
  UnexpectedInterpolation
    ["unexpected interpolation"]
  InvalidNum
    ["invalid numeric literal"]
  InvalidUnicode
    ["invalid unicode escape"]
  InvalidIvy
    ["invalid inline ivy"]
  UnknownAttribute
    ["unknown attribute"]
  BadBuiltin
    ["bad builtin"]
  CannotResolve { ident: Ident<'core>, module: &'core str }
    ["cannot find `{ident}` in `{module}`"]
  BadPatternPath
    ["invalid pattern; this path is not a struct or enum variant"]
  DuplicateItem { name: Ident<'core> }
    ["duplicate definition of `{name}`"]
  ExpectedSpaceFoundValueExpr
    ["expected a space expression; found a value expression"]
  ExpectedValueFoundSpaceExpr
    ["expected a value expression; found a space expression"]
  ExpectedPlaceFoundValueExpr
    ["expected a place expression; found a value expression"]
  ExpectedPlaceFoundSpaceExpr
    ["expected a place expression; found a space expression"]
  InconsistentTupleForm
    ["tuple members have inconsistent forms"]
  DerefNonPlacePat
    ["`*` is only valid in a place pattern"]
  RefSpacePat
    ["`&` is invalid in a space pattern"]
  ExpectedCompletePat
    ["expected a complete pattern"]
  CannotInfer
    ["cannot infer type"]
  BadBinOp { op: BinaryOp, assign: bool, lhs: String, rhs: String }
    ["cannot apply operator `{op}{}` to types `{lhs}` and `{rhs}`", if *assign { "=" } else { "" }]
  MismatchedValueSpaceTypes { value: String, space: String }
    ["value has type `{value}` but space has type `{space}`"]
  BadArgCount { expected: usize, got: usize }
    ["function expects {expected} argument{}; was passed {got}", plural(*expected, "s", "")]
  NonFunctionCall { ty: String }
    ["cannot call non-function type `{ty}`"]
  CannotCompare { lhs: String, rhs: String}
    ["cannot compare `{lhs}` and `{rhs}`"]
  NilaryMethod
    ["invalid method; function takes no parameters"]
  ExpectedTypeFound { expected: String, found: String }
    ["expected type `{expected}`; found `{found}`"]
  PathNoAssociated { desc: &'static str, path: &'core str }
    ["no {desc} associated with `{path}`"]
  BadGenericCount { path: &'core str, expected: usize, got: usize, kind: &'static str }
    ["`{path}` expects {expected} {kind} parameter{}; was passed {got}", plural(*expected, "s", "")]
  MissingTupleField { ty: String, i: usize }
    ["type `{ty}` has no field `{i}`"]
  MissingObjectField { ty: String, key: Ident<'core> }
    ["type `{ty}` has no field `{key}`"]
  SpaceField
    ["cannot access a field of a space expression"]
  FnItemUntypedParam
    ["fn item parameters must be explicitly typed"]
  ItemTypeHole
    ["types in item signatures cannot be elided"]
  RecursiveTypeAlias
    ["type aliases cannot be recursive"]
  MissingBuiltin { builtin: &'static str }
    ["cannot find builtin `{builtin}`"]
  MissingOperatorBuiltin
    ["cannot find builtin for operator"]
  NoReturn
    ["no function to return from"]
  NoLoopBreak
    ["no loop to break from"]
  NoLoopContinue
    ["no loop to continue"]
  UnboundLabel { label: Ident<'core> }
    ["cannot find label `{label}`"]
  NoContinueLabel { label: Ident<'core> }
    ["cannot continue label `{label}`"]
  MissingReturnExpr { ty: String }
    ["expected a value of type `{ty}` to return"]
  MissingBreakExpr { ty: String }
    ["expected a value of type `{ty}` to break with"]
  NoMethods { ty: String }
    ["type `{ty}` has no methods"]
  BadMethodReceiver { base_path: &'core str, ident: Ident<'core> }
    ["`{base_path}::{ident}` cannot be used as a method; it does not take `{base_path}` as its first parameter"]
  Invisible { module: &'core str, ident: Ident<'core>, vis: &'core str }
    ["`{module}::{ident}` is only visible within `{vis}`"]
  BadVis
    ["invalid visibility; expected the name of an ancestor module"]
  InvisibleAssociated { desc: &'static str, path: &'core str, vis: &'core str }
    ["the {desc} `{path}` is only visible within `{vis}`"]
  VisibleSubitem
    ["subitems must be private"]
  DuplicateKey
    ["duplicate object key"]
  MissingImplementation
    ["missing implementation"]
  UnexpectedImplParam
    ["impl parameters are not allowed here"]
  InvalidTraitItem
    ["invalid trait item"]
  InvalidImplItem
    ["invalid impl item"]
  TraitItemVis
    ["trait items cannot have visibility"]
  ImplItemVis
    ["impl items cannot have visibility"]
  TraitItemGen
    ["trait items cannot have generics"]
  ImplItemGen
    ["impl items cannot have generics"]
  ImplItemMethod
    ["impl fns cannot be marked as methods; this is done at the trait level"]
  ImplementedTraitItem
    ["trait items cannot have implementations"]
  UnexpectedImplArgs
    ["impl arguments are not allowed in types or patterns"]
  ExtraneousImplItem { name: Ident<'core> }
    ["no item `{name}` exists in trait"]
  UnspecifiedImpl
    ["impl parameters must be explicitly specified"]
  IncompleteImpl { name: Ident<'core> }
    ["missing implementation of `{name}`"]
  WrongImplSubitemKind { expected: &'static str }
    ["expected a {expected}"]
  DuplicateTypeParam
    ["duplicate type param"]
  DuplicateImplParam
    ["duplicate impl param"]
  CannotFindImpl { ty: String }
    ["cannot find impl of trait `{ty}`"]
  AmbiguousImpl { ty: String }
    ["found several impls of trait `{ty}`"]
  SearchLimit { ty: String }
    ["search limit reached when finding impl of trait `{ty}`"]
  NoMethod { ty: String, name: Ident<'core> }
    ["type `{ty}` has no method `{name}`"]
  AmbiguousMethod { ty: String, name: Ident<'core> }
    ["multiple methods named `{name}` for `{ty}`"]
  CircularImport
    ["circular import"]
  UnwrapNonStruct
    ["only struct types can be unwrapped"]
  EnumVariantNoData
    ["this enum variant has no data"]
  ExpectedDataSubpattern
    ["expected data subpattern"]
  ExpectedDataExpr
    ["constructor expects data"]
  StructDataInvisible { ty: String, vis: &'core str }
    ["the data of `{ty}` is only visible within `{vis}`"]
  TryBadReturnType { tried: String, ret: String }
    ["cannot try `{tried}` in a function returning `{ret}`"]
  MissingBlockExpr { ty: String }
    ["expected a value of type `{ty}` to evaluate to"]
  GenericMain
    ["main cannot be generic"]
  InfiniteLoop
    ["unconditional infinite loops are invalid"]
  NonExhaustiveMatch
    ["match arms do not cover all possible cases"]
  CannotImplFn
    ["cannot directly implement the fn trait"]
}

fn plural<'a>(n: usize, plural: &'a str, singular: &'a str) -> &'a str {
  if n == 1 {
    singular
  } else {
    plural
  }
}

impl<'core> Core<'core> {
  pub(crate) fn report(&self, diag: Diag<'core>) -> ErrorGuaranteed {
    self.diags.borrow_mut().push(diag);
    ErrorGuaranteed(())
  }

  pub fn files(&self) -> Ref<'_, Vec<FileInfo>> {
    self.files.borrow()
  }

  pub fn has_diags(&self) -> bool {
    !self.diags.borrow().is_empty()
  }

  pub fn take_diags(&self) -> Vec<Diag<'core>> {
    take(&mut *self.diags.borrow_mut())
  }

  pub fn bail(&self) -> Result<(), Vec<Diag<'core>>> {
    let diags = self.take_diags();
    if diags.is_empty() {
      Ok(())
    } else {
      Err(diags)
    }
  }

  pub fn print_diags(&self, diags: &[Diag<'core>]) -> String {
    let mut err = String::new();
    for diag in diags.iter() {
      if let Some(span) = diag.span() {
        match self.show_span(span) {
          Some(span) => writeln!(err, "error {span} - {diag}").unwrap(),
          None => writeln!(err, "error - {diag}").unwrap(),
        }
      }
    }
    err.pop();
    err
  }

  pub fn show_span(&self, span: Span) -> Option<String> {
    (span != Span::NONE).then(|| format!("{}", self.files.borrow()[span.file].get_pos(span.start)))
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ErrorGuaranteed(());

impl ErrorGuaranteed {
  pub const fn new_unchecked() -> Self {
    ErrorGuaranteed(())
  }
}

impl From<ErrorGuaranteed> for Diag<'_> {
  fn from(value: ErrorGuaranteed) -> Self {
    Diag::Guaranteed(value)
  }
}

pub struct FileInfo {
  pub path: Option<PathBuf>,
  pub name: String,
  line_starts: Vec<usize>,
}

impl FileInfo {
  pub fn new(path: Option<PathBuf>, name: String, src: &str) -> Self {
    let line_starts = src.lines().map(|x| x.as_ptr() as usize - src.as_ptr() as usize).collect();
    FileInfo { path, name, line_starts }
  }
}

impl FileInfo {
  pub fn get_pos(&self, byte: usize) -> Pos<'_> {
    let line = self.line_starts.partition_point(|&x| x <= byte) - 1;
    let col = byte - self.line_starts[line];
    Pos { file: &self.name, line, col }
  }
}

pub struct Pos<'f> {
  pub file: &'f str,
  pub line: usize,
  pub col: usize,
}

impl Display for Pos<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}:{}:{}", self.file, self.line + 1, self.col + 1)
  }
}

impl<T> From<ErrorGuaranteed> for Result<T, Diag<'_>> {
  fn from(value: ErrorGuaranteed) -> Self {
    Err(value.into())
  }
}

impl From<&ErrorGuaranteed> for ErrorGuaranteed {
  fn from(value: &ErrorGuaranteed) -> Self {
    *value
  }
}
