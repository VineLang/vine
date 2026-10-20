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
  pub path: Path<!>,
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
      self.semi(body.is_none())?;
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

impl ParseAttr<AttrTest> for Parser<'_> {
  fn _parse_attr(&mut self, name: &str) -> Result<Option<AttrTest>, Diag> {
    if name == "test" { Ok(Some(AttrTest)) } else { Ok(None) }
  }
}

impl ParseAttr<AttrFrameless> for Parser<'_> {
  fn _parse_attr(&mut self, name: &str) -> Result<Option<AttrFrameless>, Diag> {
    if name == "frameless" { Ok(Some(AttrFrameless)) } else { Ok(None) }
  }
}
