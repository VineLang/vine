use std::fmt::{self, Display};

use vine_util::lexer::TokenSet;

use crate::{
  compiler::Compiler,
  components::lexer::Token,
  structures::{
    ast::{BinaryOp, Ident, Span},
    checkpoint::Checkpoint,
  },
};

#[derive(Default, Debug)]
pub struct Diags {
  pub errors: Vec<Diag>,
  pub warnings: Vec<Diag>,
}

impl Diags {
  pub(crate) fn error(&mut self, diag: Diag) -> ErrorGuaranteed {
    self.errors.push(diag);
    ErrorGuaranteed(())
  }

  pub(crate) fn warn(&mut self, diag: Diag) {
    self.warnings.push(diag);
  }

  pub fn bail(&mut self) -> Result<(), ErrorGuaranteed> {
    if self.errors.is_empty() { Ok(()) } else { Err(ErrorGuaranteed(())) }
  }

  pub(crate) fn revert(&mut self, checkpoint: &Checkpoint) {
    self.errors.truncate(checkpoint.errors);
    self.warnings.truncate(checkpoint.warnings);
  }
}

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
  CannotRead { path: String }
    ["cannot read `{path}`"]
  AmbiguousSubmodule { file_path: String, dir_path: String }
    ["ambiguous submodule source; both `{file_path}` and `{dir_path}/` exist"]
  MissingSubmodule { file_path: String, dir_path: String }
    ["missing submodule; neither `{file_path}` nor `{dir_path}/` exist"]
  InvalidSubmodule
    ["cannot include a submodule from another file in a source file"]
  LexError
    ["lexing error"]
  UnexpectedToken { expected: TokenSet<Token>, found: Token }
    ["expected {expected}; found {found}"]
  UnexpectedEofString
    ["unexpected eof inside string"]
  MultiCharLiteral
    ["character literals must contain exactly one character"]
  UnexpectedInterpolation
    ["unexpected interpolation"]
  InvalidNum
    ["invalid numeric literal"]
  InvalidEscape
    ["invalid escape"]
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
  CannotResolveAbsolute { ident: Ident }
    ["cannot find `#{ident}`"]
  BadPatternPath
    ["invalid pattern; this path is not a struct or enum variant"]
  DuplicateItem { name: Ident }
    ["duplicate definition of `{name}`"]
  DuplicateImpl { path: String, trait_: Ident }
    ["duplicate implementation of `{trait_}` at `{path}`"]
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
  ExpectedTypeFnFound { expected: String, found: String }
    ["expected type `fn {expected}`; found `fn {found}`"]
  PathNoAssociated { desc: &'static str, path: String }
    ["no {desc} associated with `{path}`"]
  PathNoImpl { trait_: Ident, path: String }
    ["no impl of `{trait_}` associated with `{path}`"]
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
  InvisibleImpl { trait_: Ident, path: String, vis: String }
    ["the impl of `{trait_}` at `{path}` is only visible within `{vis}`"]
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
  ExpectedDataSubpattern
    ["expected content subpattern"]
  ExpectedDataExpr
    ["constructor expects content"]
  StructDataInvisible { ty: String, vis: String }
    ["the content of `{ty}` is only visible within `{vis}`"]
  TryBadReturnType { tried: String, ret: String }
    ["cannot try `{tried}` in a function returning `{ret}`"]
  MissingBlockExpr { ty: String }
    ["expected a value of type `{ty}` to evaluate to"]
  GenericEntrypoint
    ["entrypoint cannot be generic"]
  UnsafeEntrypoint
    ["entrypoint cannot be unsafe"]
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
    ["the `#[frameless]` attribute can only be applied to a function"]
  UnusedVariable
    ["unused variable"]
  UnusedItem
    ["unused item"]
  IfConstGeneric
    ["the condition of an `if const` cannot be generic"]
  BadTestAttr
    ["the `#[test]` attribute can only be applied to a function"]
  BadSelfDualAttr
    ["the `#[self_dual]` attribute can only be applied to a struct or unsafe enum"]
  NotSelfDual
    ["this type cannot be self-dual as its content is not self-dual"]
  InvalidUnsafe
    ["`unsafe` cannot be applied to this kind of item"]
  Unsafe
   ["this is unsafe"]
  UnusedSafe
   ["unused `safe`"]
  ImplExpectedSafe { kind: &'static str }
    ["expected a safe {kind}"]
  UnsafeItemInSafeTrait
    ["unsafe items can only exist in unsafe traits"]
  UnsafeEnumFlex
    ["unsafe enums cannot have a flex annotation"]
  BadConfigurableAttr
    ["the `#[configurable]` attribute can only be applied to a constant"]
  InvalidConfigType
    ["invalid configuration type"]
  InvalidConfigValue { value: String }
    ["invalid configuration value `{value}`"]
}

fn plural<'a>(n: usize, plural: &'a str, singular: &'a str) -> &'a str {
  if n == 1 { singular } else { plural }
}

impl Compiler {
  pub fn show_span(&self, span: Span) -> Option<String> {
    (span != Span::NONE).then(|| format!("{}", self.files[span.file].get_pos(span.start)))
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
  pub name: String,
  pub src: String,
  pub line_starts: Vec<usize>,
}

impl FileInfo {
  pub fn new(name: String, src: String) -> Self {
    let line_starts = src.lines().map(|x| x.as_ptr() as usize - src.as_ptr() as usize).collect();
    FileInfo { name, src, line_starts }
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
