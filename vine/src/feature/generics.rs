use super::*;

use crate::feature::flex::Flex;

#[derive_(Default, Debug, Clone, Visit!)]
pub struct GenericParams {
  pub inherit: bool,
  pub types: Sequence<TypeParam>,
  pub impls: Sequence<ImplParam>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TypeParam {
  pub name: Spanned<Ident>,
  pub flex: Flex,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ImplParam {
  pub name: Option<Spanned<Ident>>,
  pub trait_: Trait,
}

#[derive_(Default, Debug, Clone, Visit!)]
pub struct GenericArgs {
  pub types: Sequence<TypeArg>,
  pub impls: Sequence<ImplArg>,
}

impl GenericArgs {
  pub fn is_empty(&self) -> bool {
    self.types.iter().next().is_none() && self.impls.iter().next().is_none()
  }
}

#[derive_(Debug, Clone, Visit!)]
pub struct TypeArg {
  pub name: Option<Spanned<Ident>>,
  pub ty: Ty,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ImplArg {
  pub impl_: Impl,
}

impl Parse<GenericParams> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<GenericParams>, Diag> {
    todo!()
  }
} // todo

impl Parse<GenericArgs> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<GenericArgs>, Diag> {
    todo!()
  }
} // todo
