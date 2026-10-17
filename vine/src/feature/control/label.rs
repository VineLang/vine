use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Label {
  pub kind: LabelKind,
}

#[derive_(Debug, Clone, Visit!)]
pub enum LabelKind {
  None,
  Named(Spanned<Ident>),
}

#[derive_(Debug, Clone, Visit!)]
pub struct Target {
  pub kind: TargetKind,
}

#[derive_(Debug, Clone, Visit!)]
pub enum TargetKind {
  None,
  Named(Spanned<Ident>),
  Loop,
  For,
  While,
  Do,
  If,
  Match,
  When,
  Fn,
}

#[derive_(Debug, Clone, Visit!)]
pub struct StmtBreak {
  pub target: Target,
  pub value: Option<Expr>,
}

impl ParsePratt<Stmt, StmtBreak> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct StmtContinue {
  pub target: Target,
}

impl ParsePratt<Stmt, StmtContinue> for Parser<'_> {} // todo
