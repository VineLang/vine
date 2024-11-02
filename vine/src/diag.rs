use std::{
  fmt::{self, Display, Write},
  io,
  mem::take,
  path::PathBuf,
  slice,
};

use vine_util::lexer::TokenSet;

use crate::{
  ast::{BinaryOp, Ident, Path, Span},
  lexer::Token,
};

macro_rules! diags {
  ($(
    $name:ident $({ $($field:ident: $ty:ty),* $(,)? })?
      [$($fmt:tt)*]
  )*) => {
    #[derive(Debug)]
    pub enum Diag {
      $( $name { span: Span, $($($field: $ty),*)? },)*
      Group(Vec<Diag>),
      Guaranteed(ErrorGuaranteed),
    }

    impl Diag {
      fn span(&self) -> Result<Span, &[Diag]> {
        match self {
          $( Self::$name { span, .. } => Ok(*span), )*
          Diag::Group(diags) => Err(diags),
          Diag::Guaranteed(_) => Err(&[]),
        }
      }
    }

    impl Display for Diag {
      fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
          $( Self::$name { span: _, $($($field),*)? } => write!(f, $($fmt)*),)*
          Diag::Group(_) => unreachable!(),
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
  CannotResolve { name: Ident, module: Path }
    ["cannot find `{name}` in `{module}`"]
  BadPatternPath
    ["invalid pattern; this path is not a struct or enum variant"]
  DuplicateItem { name: Ident }
    ["duplicate definition of `{name}`"]
  ExpectedSpaceFoundValueExpr
    ["expected a space expression; found a value expression"]
  ExpectedValueFoundSpaceExpr
    ["expected a value expression; found a space expression"]
  ExpectedPlaceFoundSpaceExpr
    ["expected a place expression; found a space expression"]
  MoveNonPlacePat
    ["`move` is only valid in a place pattern"]
  DerefNonPlacePat
    ["`*` is only valid in a place pattern"]
  RefSpacePat
    ["`&` is invalid in a space pattern"]
  ExpectedRefutablePat
    ["expected an irrefutable pattern"]
  CannotInfer
    ["cannot infer type"]
  BadBinOp { op: BinaryOp, lhs: String, rhs: String }
    ["cannot apply operator `{op:?}` to values of types `{lhs}` and `{rhs}`"]
  MismatchedThenElseTypes { then: String, els: String }
    ["then block has type `{then}` but else block has type `{els}`"]
  BadArgCount { ty: String, expected: usize, got: usize }
    ["function type `{ty}` expects {expected} arguments; was passed {got}"]
  NonFunctionCall { ty: String }
    ["cannot call non-function type `{ty}`"]
  CannotCompare { lhs: String, rhs: String}
    ["cannot compare `{lhs}` and `{rhs}`"]
  NonMethodFunction { ty: String }
    ["invalid method; function type `{ty}` does not accept reference as first parameter"]
  ExpectedTypeFound { expected: String, found: String }
    ["expected type `{expected}`; found `{found}`"]
  PathNoValue { path: Path }
    ["no value associated with `{path}`"]
  PathNoType { path: Path }
    ["no type associated with `{path}`"]
  PathNoPat { path: Path }
    ["no pattern associated with `{path}`"]
  BadGenericCount { path: Path, expected: usize, got: usize }
    ["`{path}` expects {expected} generics; was passed {got}"]
  BadFieldCount { path: Path, expected: usize, got: usize }
    ["`{path}` has {expected} fields; {got} were matched"]
  FnItemUntypedParam
    ["fn item parameters must be explicitly typed"]
  ItemTypeHole
    ["types in item signatures cannot be elided"]
  RecursiveTypeAlias
    ["type aliases cannot be recursive"]
  NoList
    ["cannot find `std::list::List`"]
}

#[derive(Default, Debug)]
pub struct DiagGroup {
  diags: Vec<Diag>,
}

impl DiagGroup {
  pub(crate) fn add(&mut self, diag: Diag) -> ErrorGuaranteed {
    self.diags.push(diag);
    ErrorGuaranteed(())
  }

  pub fn ok(&mut self) -> Result<(), Diag> {
    if self.diags.is_empty() {
      Ok(())
    } else {
      Err(Diag::Group(take(&mut self.diags)))
    }
  }

  pub fn report(&mut self, files: &[FileInfo]) -> Result<(), String> {
    if self.diags.is_empty() {
      Ok(())
    } else {
      let mut str = String::new();
      Self::_report(&self.diags, files, &mut str);
      str.pop();
      self.diags.clear();
      Err(str)
    }
  }

  fn _report(diags: &[Diag], files: &[FileInfo], err: &mut String) {
    for diag in diags {
      match diag.span() {
        Ok(Span::NONE) => writeln!(err, "error - {}", diag).unwrap(),
        Ok(span) => {
          let file = &files[span.file];
          writeln!(err, "error {} - {}", file.get_pos(span.start), diag).unwrap();
        }
        Err(diags) => Self::_report(diags, files, err),
      }
    }
  }
}

impl Diag {
  pub fn report(&self, files: &[FileInfo]) -> String {
    let mut str = String::new();
    DiagGroup::_report(slice::from_ref(self), files, &mut str);
    str.pop();
    str
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ErrorGuaranteed(());

impl ErrorGuaranteed {
  pub fn new_unchecked() -> Self {
    ErrorGuaranteed(())
  }
}

impl From<ErrorGuaranteed> for Diag {
  fn from(value: ErrorGuaranteed) -> Self {
    Diag::Guaranteed(value)
  }
}

pub struct FileInfo {
  name: String,
  line_starts: Vec<usize>,
}

impl FileInfo {
  pub fn new(name: String, src: &str) -> Self {
    let line_starts = src.lines().map(|x| x.as_ptr() as usize - src.as_ptr() as usize).collect();
    FileInfo { name, line_starts }
  }
}

impl FileInfo {
  fn get_pos(&self, byte: usize) -> Pos<'_> {
    let line = self.line_starts.partition_point(|&x| x <= byte) - 1;
    let col = byte - self.line_starts[line];
    Pos { file: &self.name, line, col }
  }
}

struct Pos<'f> {
  file: &'f str,
  line: usize,
  col: usize,
}

impl Display for Pos<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}:{}:{}", self.file, self.line + 1, self.col + 1)
  }
}
