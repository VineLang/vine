use super::*;

use vine_util::trivial_visit;

#[derive_(Debug, Clone, Visit!)]
pub struct Field<T> {
  pub composite: T,
  pub field: FieldKind,
}

impl ParsePratt<Expr, Field<Expr>> for Parser<'_> {} // todo

impl ParsePratt<Ty, Field<Ty>> for Parser<'_> {
  fn _parse_dot(&mut self, initial: Ty) -> Result<Result<Field<Ty>, Ty>, Diag> {
    if let Some(ident) = self.try_parse()? {
      Ok(Ok(Field { composite: initial, field: FieldKind::Object(ident) }))
    } else {
      Ok(Err(initial))
    }
  }

  fn _parse_suffix(&mut self, _: &mut PrecCtx, initial: Ty) -> Result<Result<Field<Ty>, Ty>, Diag> {
    if let Some(field) = self.try_parse()? {
      Ok(Ok(Field { composite: initial, field: FieldKind::Tuple(field) }))
    } else {
      Ok(Err(initial))
    }
  }
}

#[derive_(Debug, Clone, Visit!)]
pub enum FieldKind {
  Tuple(Spanned<TupleField>),
  Object(Spanned<Ident>),
}

#[derive_(Debug, Clone)]
pub struct TupleField(usize);

trivial_visit!(TupleField);

impl Parse<TupleField> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<TupleField>, Diag> {
    if self.check(Token::TupleField) {
      let _ = self.expect(Token::TupleField)?;
      let key = 0; // todo
      Ok(Some(TupleField(key)))
    } else {
      Ok(None)
    }
  }
}
