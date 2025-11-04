use std::{
  cell::Ref,
  fmt::{self, Display, Write},
  io,
  mem::take,
  path::PathBuf,
};

use vine_util::{idx::IdxVec, lexer::TokenSet};

use crate::{
  components::{
    lexer::{StrToken, Token},
    loader::{FileId, Loader},
  },
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
    pub enum Diag {
      $( $name { span: Span, $($($field: $ty),*)? },)*
      Guaranteed(ErrorGuaranteed),
    }

    impl Diag {
      pub fn span(&self) -> Option<Span> {
        match self {
          $( Self::$name { span, .. } => Some(*span), )*
          Diag::Guaranteed(_) => None,
        }
      }
    }

    impl Display for Diag {
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
  DisallowedImplicitMod
    ["implicit submodule paths are only allowed in files of the form `{{mod_name}}/{{mod_name}}.vi`"]
  AmbiguousImplicitMod { name: Ident }
    ["ambiguous implicit submodule path; both `{name}.vi` and `{name}/{name}.vi` exist"]
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
  CannotResolve { ident: Ident, module: String }
    ["cannot find `{ident}` in `{module}`"]
  BadPatternPath
    ["invalid pattern; this path is not a struct or enum variant"]
  DuplicateItem { name: Ident }
    ["duplicate definition of `{name}`"]
  ExpectedSpaceFoundValueExpr
    ["expected a space expression; found a value expression"]
  ExpectedValueFoundSpaceExpr
    ["expected a value expression; found a space expression"]
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
  PathNoAssociated { desc: &'static str, path: String }
    ["no {desc} associated with `{path}`"]
  BadGenericCount { path: String, expected: usize, got: usize, kind: &'static str }
    ["`{path}` expects {expected} {kind} parameter{}; was passed {got}", plural(*expected, "s", "")]
  MissingTupleField { ty: String, i: usize }
    ["type `{ty}` has no field `{i}`"]
  MissingObjectField { ty: String, key: Ident }
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
  InvalidBreakTarget
    ["invalid break target"]
  InvalidContinueTarget
    ["invalid continue target"]
  MissingReturnExpr { ty: String }
    ["expected a value of type `{ty}` to return"]
  MissingBreakExpr { ty: String }
    ["expected a value of type `{ty}` to break with"]
  NoMethods { ty: String }
    ["type `{ty}` has no methods"]
  BadMethodReceiver { base_path: String, ident: Ident }
    ["`{base_path}::{ident}` cannot be used as a method; it does not take `{base_path}` as its first parameter"]
  Invisible { module: String, ident: Ident, vis: String }
    ["`{module}::{ident}` is only visible within `{vis}`"]
  BadVis
    ["invalid visibility; expected the name of an ancestor module"]
  InvisibleAssociated { desc: &'static str, path: String, vis: String }
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
  TraitItemInheritGen
    ["trait items always inherit generics"]
  ImplItemInheritGen
    ["impl items always inherit generics"]
  ImplItemMethod
    ["impl fns cannot be marked as methods; this is done at the trait level"]
  ImplementedTraitItem
    ["trait items cannot have implementations"]
  UnexpectedImplArgs
    ["impl arguments are not allowed in types or patterns"]
  ExtraneousImplItem { name: Ident }
    ["no item `{name}` exists in trait"]
  UnspecifiedImpl
    ["impl parameters must be explicitly specified"]
  IncompleteImpl { name: Ident }
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
  NoMethod { ty: String, name: Ident }
    ["type `{ty}` has no method `{name}`"]
  AmbiguousMethod { ty: String, name: Ident }
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
  StructDataInvisible { ty: String, vis: String }
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
  AmbiguousPolyformicComposite
    ["composite expression in polyformic position has elements of mixed forms"]
  FlexSearchLimit { ty: String }
    ["search limit reached when finding flex of type `{ty}`"]
  BiFlexible { ty: String }
    ["implementation of `Fork`/`Drop` found for both `{ty}` and its inverse"]
  IncompatibleForkDropInference { ty: String, fork_ty: String, drop_ty: String }
    ["finding flex of `{ty}` resulted in incompatible impls `Fork[{fork_ty}]` and `Drop[{drop_ty}]`"]
  CannotFork { ty: String }
    ["cannot fork `{ty}`"]
  CannotDrop { ty: String }
    ["cannot drop `{ty}`"]
  UninitializedVariable { ty: String }
    ["variable of type `{ty}` read whilst uninitialized"]
  BadManualAttr
    ["the `#[manual]` attribute can only be applied to an impl"]
  BadBasicAttr
    ["the `#[basic]` attribute can only be applied to an impl"]
  BadBecomeAttr
    ["the `#[become]` attribute can only be applied to an impl"]
  DuplicateBecomeAttr
    ["the `#[become]` attribute can only be applied to an impl once"]
  GenericBecomeAttr
    ["generic arguments cannot be supplied here"]
  BecomeOtherTrait
    ["can only `#[become]` an impl of the same trait"]
  BecomeGenericImpl
    ["can only `#[become]` an impl with no impl parameters"]
  InvalidCommand
    ["invalid command; type `/help` for a list of commands"]
  MissingElse
    ["missing else block"]
  MissingTerminalArm
    ["missing terminal arm in `when`"]
  ExpectedImplItemTypeParams { expected: usize, found: usize }
    ["expected impl item to have {expected} type parameters; found {found}"]
  ExpectedImplItemImplParams { expected: usize, found: usize }
    ["expected impl item to have {expected} impl parameters; found {found}"]
  ExpectedImplItemImplParam { expected: String, found: String }
    ["expected impl parameter of trait `{expected}`; found `{found}`"]
  UnknownCfg { name: Ident }
    ["no configuration value named `{name}`"]
  BadCfgType { name: Ident, kind: &'static str }
    ["expected `{name}` to be a {kind} configuration value"]
  BadFramelessAttr
    ["the `#[frameless]` attribute can only be applied to an fn"]
}

fn plural<'a>(n: usize, plural: &'a str, singular: &'a str) -> &'a str {
  if n == 1 {
    singular
  } else {
    plural
  }
}

impl Core {
  pub(crate) fn report(&self, diag: Diag) -> ErrorGuaranteed {
    self.diags.borrow_mut().push(diag);
    ErrorGuaranteed(())
  }

  pub fn has_diags(&self) -> bool {
    !self.diags.borrow().is_empty()
  }

  pub fn take_diags(&self) -> Vec<Diag> {
    take(&mut *self.diags.borrow_mut())
  }

  pub fn bail(&self) -> Result<(), Vec<Diag>> {
    let diags = self.take_diags();
    if diags.is_empty() {
      Ok(())
    } else {
      Err(diags)
    }
  }
}

impl Loader {
  pub fn files(&self) -> Ref<'_, IdxVec<FileId, FileInfo>> {
    self.files.borrow()
  }

  pub fn print_diags(&self, diags: &[Diag]) -> String {
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
    (span != Span::NONE).then(|| format!("{}", self.files()[span.file].get_pos(span.start)))
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ErrorGuaranteed(());

impl ErrorGuaranteed {
  pub const fn new_unchecked() -> Self {
    ErrorGuaranteed(())
  }
}

impl From<ErrorGuaranteed> for Diag {
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

impl<T> From<ErrorGuaranteed> for Result<T, Diag> {
  fn from(value: ErrorGuaranteed) -> Self {
    Err(value.into())
  }
}

impl From<&ErrorGuaranteed> for ErrorGuaranteed {
  fn from(value: &ErrorGuaranteed) -> Self {
    *value
  }
}
