use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ItemTrait<I> {
  pub name: Spanned<Ident>,
  pub inherent: Vec<Spanned<Ident>>,
  pub generics: GenericParams,
  pub items: Vec<I>,
}
