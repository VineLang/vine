use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct StringLiteral<T = !> {
  pub span: LocalSpan,
  pub kind: StringKind,
  pub lang: Option<String>,
  pub lines: Vec<StringLine<T>>,
}

#[derive_(Debug, Clone, Visit!)]
pub enum StringKind {
  Raw,
  Processed,
}

#[derive_(Debug, Clone, Visit!)]
pub struct StringLine<T = !> {
  pub pieces: Vec<StringPiece<T>>,
}

#[derive_(Debug, Clone, Visit!)]
pub enum StringPiece<T = !> {
  Segment(String),
  Escape(char),
  Interpolation(T),
}

impl StringLiteral {
  pub fn value(&self) -> String {
    let mut value = String::new();
    for (i, line) in self.lines.iter().enumerate() {
      if i != 0 {
        value.push('\n');
      }
      for piece in &line.pieces {
        match piece {
          StringPiece::Segment(str) => value.push_str(str),
          StringPiece::Escape(char) => value.push(*char),
          StringPiece::Interpolation(interp) => match *interp {},
        }
      }
    }
    value
  }
}

impl Parse<StringLiteral> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<StringLiteral>, Diag> {
    todo!()
  }
} // todo

impl GrammarPratt for StringLiteral<Expr> {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![] // todo
  }
}

impl ParsePratt<Expr, StringLiteral<Expr>> for Parser<'_> {} // todo

impl GrammarPratt for StringLiteral<Pat> {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![] // todo
  }
}

impl ParsePratt<Pat, StringLiteral<Pat>> for Parser<'_> {} // todo
