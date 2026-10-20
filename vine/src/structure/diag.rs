use std::{ops::Deref, slice};

use super::*;

use crate::{feature::lint::Lint, structure::content::Content};

new_idx!(pub DiagId);

#[derive(Debug, Clone, Copy)]
pub struct ErrorId(DiagId);

pub struct Diags {
  diags: IdxVec<DiagId, Diagnostic>,
}

impl Diags {
  pub fn error(&mut self, diag: impl Diag) -> ErrorId {
    self.error_with(diag, ())
  }

  pub fn error_with<C>(&mut self, diag: impl Diag<C>, ctx: C) -> ErrorId {
    let diag = diag.emit(self, ctx);

    ErrorId(diag)
  }

  pub fn emit(&mut self, diag: impl Diag) -> DiagId {
    self.emit_with(diag, ())
  }

  pub fn emit_with<C>(&mut self, diag: impl Diag<C>, ctx: C) -> DiagId {
    diag.emit(self, ctx)
  }
}

pub enum ErrorSet {
  Single(DiagId),
  Multiple(Vec<DiagId>),
}

pub trait Diag<C = ()> {
  fn emit(self, diags: &mut Diags, ctx: C) -> DiagId;
}

pub struct Diagnostic {
  span: FileSpan,
  message: Content,
  level: Level,
}

pub enum Level {
  Error,
  SoftError,
  Lint(Lint),
}

impl<C> Diag<C> for Diagnostic {
  fn emit(self, diags: &mut Diags, _: C) -> DiagId {
    diags.diags.push(self)
  }
}

impl<C> Diag<C> for DiagId {
  fn emit(self, _: &mut Diags, _: C) -> DiagId {
    self
  }
}
