use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ItemFn {
  pub method: bool,
  pub name: Spanned<Ident>,
  pub generics: GenericParams,
  pub args: Sequence<Pat>,
  pub return_ty: Option<Ty>,
  pub body: Option<Block>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyFnPath {
  pub path: Path,
}

#[derive_(Debug, Clone, Visit!)]
pub struct AttrTest;

#[derive_(Debug, Clone, Visit!)]
pub struct AttrFrameless;

impl Parse<ItemFn> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ItemFn>, Diag> {
    if self.eat(Token::Fn)? {
      let method = self.eat(Token::Dot)?;
      let name = self.parse()?;
      let generics = self.parse()?;
      let args = self.parse()?;
      let return_ty = self.eat_then(Token::Colon, Self::parse)?;
      let body = self.try_parse()?;
      if body.is_none() {
        self.semi();
      }
      Ok(Some(ItemFn { method, name, generics, args, return_ty, body }))
    } else {
      Ok(None)
    }
  }
}

impl ParsePratt<Ty, TyFnPath> for Parser<'_> {
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<TyFnPath>, Diag> {
    if self.eat(Token::Fn)? { Ok(Some(TyFnPath { path: self.parse()? })) } else { Ok(None) }
  }
}
