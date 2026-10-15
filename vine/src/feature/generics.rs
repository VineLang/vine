use super::*;

use crate::feature::flex::Flex;

#[derive_(Debug, Clone, Visit!)]
pub struct GenericParams {
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

#[derive_(Debug, Clone, Visit!)]
pub struct GenericArgs {
  pub types: Sequence<TypeArg>,
  pub impls: Sequence<ImplArg>,
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
