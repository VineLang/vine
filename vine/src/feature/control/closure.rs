use super::*;

use crate::feature::flex::Flex;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprClosure {
  pub flex: Flex,
  pub args: Sequence<Pat>,
  pub return_ty: Option<Ty>,
  pub body: Block,
}

impl ParsePratt<Expr, ExprClosure> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct StmtClosure {
  pub name: Spanned<Ident>,
  pub closure: ExprClosure,
}

impl ParsePratt<Stmt, StmtClosure> for Parser<'_> {} // todo
