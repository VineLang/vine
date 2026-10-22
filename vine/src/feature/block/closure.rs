use super::*;

use crate::feature::flex::Flex;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprClosure {
  pub flex: Flex,
  pub params: Sequence<Pat>,
  pub return_ty: Option<Ty>,
  pub body: Block,
}

#[derive_(Debug, Clone, Visit!)]
pub struct StmtClosure {
  pub name: Spanned<Ident>,
  pub closure: ExprClosure,
}

impl GrammarRule for ExprClosure {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "ExprClosure",
      (
        Token::Fn,
        rule::<Flex>().field("flex").optional(),
        rule::<Sequence<Pat>>().field("params"),
        (Token::Colon, rule::<Ty>().field("return")).optional(),
        rule::<Block>().field("body"),
      ),
    )
  }
}

impl GrammarRule for StmtClosure {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "StmtClosure",
      (
        Token::Let,
        Token::Fn,
        rule::<Flex>().field("flex").optional(),
        rule::<Ident>().field("name"),
        rule::<Sequence<Pat>>().field("params"),
        (Token::Colon, rule::<Ty>().field("return")).optional(),
        rule::<Block>().field("body"),
      ),
    )
  }
}

impl Parse<ExprClosure> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ExprClosure>, Diag> {
    if self.eat(Token::Fn)? {
      let flex = self.parse()?;
      let args = self.parse()?;
      let return_ty = self.eat_then(Token::Colon, Self::parse)?;
      let body = self.parse()?;
      Ok(Some(ExprClosure { flex, params: args, return_ty, body }))
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
