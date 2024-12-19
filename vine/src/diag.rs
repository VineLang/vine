use std::{
  cell::Ref,
  fmt::{self, Display, Write},
  io,
  mem::take,
  path::PathBuf,
};

use vine_util::lexer::TokenSet;

use crate::{
  ast::{BinaryOp, Ident, Path, Span},
  core::Core,
  lexer::Token,
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
  InvalidNum
    ["invalid numeric literal"]
  InvalidStr
    ["invalid string literal"]
  InvalidChar
    ["invalid character literal"]
  InvalidIvy
    ["invalid inline ivy"]
  UnknownAttribute
    ["unknown attribute"]
  BadBuiltin
    ["bad builtin"]
  CannotResolve { name: Ident<'core>, module: Path<'core> }
    ["cannot find `{name}` in `{module}`"]
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
  MoveNonPlacePat
    ["`move` is only valid in a place pattern"]
  DerefNonPlacePat
    ["`*` is only valid in a place pattern"]
  RefSpacePat
    ["`&` is invalid in a space pattern"]
  ExpectedIrrefutablePat
    ["expected an irrefutable pattern"]
  CannotInfer
    ["cannot infer type"]
  BadBinOp { op: BinaryOp, assign: bool, lhs: String, rhs: String }
    ["cannot apply operator `{op}{}` to types `{lhs}` and `{rhs}`", if *assign { "=" } else { "" }]
  MismatchedThenElseTypes { then: String, els: String }
    ["then block has type `{then}` but else block has type `{els}`"]
  MismatchedValueSpaceTypes { value: String, space: String }
    ["value has type `{value}` but space has type `{space}`"]
  BadArgCount { ty: String, expected: usize, got: usize }
    ["function type `{ty}` expects {expected} argument{}; was passed {got}", plural(*expected, "s", "")]
  NonFunctionCall { ty: String }
    ["cannot call non-function type `{ty}`"]
  CannotCompare { lhs: String, rhs: String}
    ["cannot compare `{lhs}` and `{rhs}`"]
  NilaryMethod { ty: String }
    ["invalid method; function type `{ty}` takes no parameters"]
  ExpectedTypeFound { expected: String, found: String }
    ["expected type `{expected}`; found `{found}`"]
  PathNoValue { path: Path<'core> }
    ["no value associated with `{path}`"]
  PathNoType { path: Path<'core> }
    ["no type associated with `{path}`"]
  PathNoPat { path: Path<'core> }
    ["no pattern associated with `{path}`"]
  BadGenericCount { path: Path<'core>, expected: usize, got: usize }
    ["`{path}` expects {expected} generic{}; was passed {got}", plural(*expected, "s", "")]
  BadFieldCount { path: Path<'core>, expected: usize, got: usize }
    ["`{path}` has {expected} field{}; {got} {} matched", plural(*expected, "s", ""), plural(*got, "were", "was")]
  MissingTupleField { ty: String, i: usize }
    ["type `{ty}` has no field `{i}`"]
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
  BadMethodReceiver { base_path: Path<'core>, sub_path: Path<'core> }
    ["`{base_path}::{sub_path}` cannot be used as a method; it does not take `{base_path}` as its first parameter"]
  Invisible { path: Path<'core>, vis: Path<'core> }
    ["`{path}` is only visible within `{vis}`"]
  BadVis
    ["invalid visibility; expected the name of an ancestor module"]
  ValueInvisible { path: Path<'core>, vis: Path<'core> }
    ["the value `{path}` is only visible within `{vis}`"]
  TypeInvisible { path: Path<'core>, vis: Path<'core> }
    ["the type `{path}` is only visible within `{vis}`"]
  PatInvisible { path: Path<'core>, vis: Path<'core> }
    ["the pattern `{path}` is only visible within `{vis}`"]
  VisibleSubitem
    ["subitems must be private"]
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

  pub fn files(&self) -> Ref<Vec<FileInfo>> {
    self.files.borrow()
  }

  pub fn has_diags(&self) -> bool {
    !self.diags.borrow().is_empty()
  }

  pub fn take_diags(&self) -> Vec<Diag> {
    take(&mut *self.diags.borrow_mut())
  }

  pub fn bail(&self) -> Result<(), String> {
    let mut diags = self.diags.borrow_mut();
    let files = self.files.borrow();
    if diags.is_empty() {
      Ok(())
    } else {
      let mut err = String::new();
      for diag in diags.iter() {
        match diag.span() {
          Some(Span::NONE) => writeln!(err, "error - {}", diag).unwrap(),
          Some(span) => {
            let file = &files[span.file];
            writeln!(err, "error {} - {}", file.get_pos(span.start), diag).unwrap();
          }
          None => {}
        }
      }
      err.pop();
      diags.clear();
      Err(err)
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ErrorGuaranteed(());

impl ErrorGuaranteed {
  pub fn new_unchecked() -> Self {
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

macro_rules! report {
  ($group:expr $(, $target:expr)*; $result:expr) => {
    match $result {
      Ok(value) => value,
      Err(diag) => {
        let err = $group.report(diag);
        $($target = err.into();)*
        return err.into();
      }
    }
  };
}

pub(crate) use report;
