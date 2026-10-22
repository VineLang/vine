use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct StmtLet {
  pub pat: Pat,
  pub kind: LetKind,
}

impl GrammarRule for StmtLet {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "StmtLet",
      (
        Token::Let,
        choice((
          (Token::Loop, rule::<Pat>()),
          (rule::<Pat>(), (Token::Eq, rule::<Expr>()).optional()),
        )),
        Token::Semi,
      ),
    )
  }
}

// Handled directly by `Parse<StmtKind>`
impl Parse<StmtLet> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<StmtLet>, Diag> {
    Ok(None)
  }
}

#[derive_(Debug, Clone, Visit!)]
pub enum LetKind {
  Uninit,
  Init(Expr),
  Loop,
}
