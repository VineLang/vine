use super::*;

use crate::feature::flex::Flex;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprClosure {
  pub flex: Flex,
  pub args: Sequence<Pat>,
  pub return_ty: Option<Ty>,
  pub body: Block,
}

#[derive_(Debug, Clone, Visit!)]
pub struct StmtClosure {
  pub name: Spanned<Ident>,
  pub closure: ExprClosure,
}


impl GrammarPratt for ExprClosure {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![] // todo
  }
}

impl ParsePratt<Expr, ExprClosure> for Parser<'_> {
  fn _parse_full(&mut self) -> Result<Option<ExprClosure>, Diag> {
    if self.eat(Token::Fn)? {
      let flex = self.parse()?;
      let args = self.parse()?;
      let return_ty = self.eat_then(Token::Colon, Self::parse)?;
      let body = self.parse()?;
      Ok(Some(ExprClosure { flex, args, return_ty, body }))
    } else {
      Ok(None)
    }
  }
}

// Handled directly by `Parse<StmtKind>`
impl Parse<StmtClosure> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<StmtClosure>, Diag> {
    Ok(None)
  }
}
