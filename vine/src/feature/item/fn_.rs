use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ItemFn {
  pub method: bool,
  pub name: Spanned<Ident>,
  pub generics: GenericParams,
  pub args: Sequence<Pat>,
  pub ret: Option<Ty>,
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

impl ParsePratt<Ty, TyFnPath> for Parser<'_> {
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<TyFnPath>, Diag> {
    if self.eat(Token::Fn)? { Ok(Some(TyFnPath { path: self.parse()? })) } else { Ok(None) }
  }
}
