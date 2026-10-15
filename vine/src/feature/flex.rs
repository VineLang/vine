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
      Flex::Drop => Some(Token::Question),
      Flex::Fork => Some(Token::Plus),
      Flex::Copy => Some(Token::PlusPlus),
      Flex::ForkDrop => Some(Token::Star),
      Flex::CopyDrop => Some(Token::StarStar),
    }
  }
}
