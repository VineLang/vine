use super::*;

use crate::component::lexer::Token;

#[derive_(Debug, Clone, Copy, Visit!, for_enum!)]
pub enum Flex {
  None,
  Drop,
  Fork,
  Copy,
  ForkDrop,
  CopyDrop,
}

impl Flex {
  pub fn token(self) -> Option<Token> {
    match self {
      Flex::None => None,
      Flex::Drop => Some(Token::Quest),
      Flex::Fork => Some(Token::Plus),
      Flex::Copy => Some(Token::PlusPlus),
      Flex::ForkDrop => Some(Token::Star),
      Flex::CopyDrop => Some(Token::StarStar),
    }
  }
}

impl GrammarRule for Flex {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "flex",
      choice((Token::Quest, Token::Plus, Token::PlusPlus, Token::Star, Token::StarStar)).optional(),
    )
  }
}

impl Parse<Flex> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<Flex>, Diag> {
    for_enum!(Flex in Flex {
      $(if let Some(token) = Flex::$Flex.token() && self.eat(token)? {
        Ok(Some(Flex::$Flex))
      } else)* {
        Ok(Some(Flex::None))
      }
    })
  }
}
