use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ItemTrait {
  pub name: Spanned<Ident>,
  pub inherent: Vec<Spanned<Ident>>,
  pub generics: GenericParams,
  pub items: Vec<ItemId>,
}
