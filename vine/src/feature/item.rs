use super::*;

use crate::feature::generics::GenericParams;

pub mod const_;
pub mod fn_;
pub mod impl_;
pub mod mod_;
pub mod trait_;
pub mod type_;
pub mod use_;

impl GrammarRule for Vis {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named("Vis", (Token::Pub, (Token::Colon, rule::<Ident>()).optional()))
  }
}

impl GrammarRule for Attr {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*; // todo
    grammar.named("Attr", (Token::Hash, Token::OpenBracket, Token::CloseBracket))
  }
}

impl GrammarRule for Item {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "Item",
      (
        // rule::<Attr>().repeat(false),
        rule::<Vis>().optional(),
        Token::Unsafe.optional(),
        choice(for_enum!(Kind in ItemKind { ($(
          rule_variant(ItemKind::$Kind),
        )*) })),
      ),
    )
  }
}
