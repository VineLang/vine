use std::{collections::HashMap, mem::take};

use vine_util::{idx::IdxVec, new_idx};

use crate::{
  ast::Span,
  chart::{AdtId, TraitDefId},
  checker::{Type, Var},
  core::Core,
  diag::{Diag, ErrorGuaranteed},
};

#[derive(Debug)]
pub struct Unifier<'core> {
  core: &'core Core<'core>,
  pub(crate) vars: IdxVec<Var, VarInfo<'core>>,
  version: Version,
}

impl<'core> Unifier<'core> {
  pub fn new(core: &'core Core<'core>) -> Self {
    Unifier { core, vars: IdxVec::new(), version: Version(0) }
  }

  pub fn new_var(&mut self, span: Span) -> Type<'core> {
    Type::Var(self._new_var(span))
  }

  pub fn _new_var(&mut self, span: Span) -> Var {
    self.vars.push(VarInfo { span, bound: None })
  }

  #[must_use]
  pub fn unify(&mut self, a: &mut Type<'core>, b: &mut Type<'core>) -> bool {
    self._unify(a, b, false)
  }

  #[must_use]
  fn _unify(&mut self, a: &mut Type<'core>, b: &mut Type<'core>, inverted: bool) -> bool {
    match (&mut *a, &mut *b) {
      (Type::Error(_), _) | (_, Type::Error(_)) => true,
      (Type::Inverse(a), Type::Inverse(b)) => self._unify(a, b, inverted),
      (a, Type::Inverse(b)) => self._unify(a, b, !inverted),
      (Type::Inverse(a), b) => self._unify(a, b, !inverted),
      (Type::Var(v), Type::Var(u)) if *v == *u => !inverted,
      (Type::Var(v), Type::Var(u)) => {
        let (v, u) = self.vars.get2_mut(*v, *u).unwrap();
        match (&mut v.bound, &mut u.bound) {
          (Some((_, v)), Some((_, u))) => {
            *a = v.clone();
            *b = u.clone();
            self._unify(a, b, inverted)
          }
          (Some((_, v)), None) => {
            *a = v.clone();
            self._unify(a, b, inverted)
          }
          (None, Some((_, u))) => {
            *b = u.clone();
            self._unify(a, b, inverted)
          }
          (None, None) => {
            v.bound = Some((self.version, b.clone().invert_if(inverted)));
            *a = b.clone().invert_if(inverted);
            true
          }
        }
      }
      (&mut ref mut o @ Type::Var(v), t) | (t, &mut ref mut o @ Type::Var(v)) => {
        let v = &mut self.vars[v];
        if let Some((_, u)) = &v.bound {
          *o = u.clone();
          self._unify(a, b, inverted)
        } else {
          *o = t.clone().invert_if(inverted);
          v.bound = Some((self.version, t.clone().invert_if(inverted)));
          true
        }
      }
      (Type::Bool, Type::Bool)
      | (Type::N32, Type::N32)
      | (Type::F32, Type::F32)
      | (Type::Char, Type::Char)
      | (Type::IO, Type::IO) => !inverted,
      (Type::Opaque(a), Type::Opaque(b)) => a == b && !inverted,
      (Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => {
        let mut success = true;
        for (a, b) in a.iter_mut().zip(b) {
          success &= self._unify(a, b, inverted);
        }
        success
      }
      (Type::Object(a), Type::Object(b)) => {
        let mut success = true;
        for (k, a) in a {
          if let Some(b) = b.get_mut(k) {
            success &= self._unify(a, b, inverted);
          } else {
            success = false;
          }
        }
        success
      }
      (Type::Ref(a), Type::Ref(b)) if !inverted => self._unify(a, b, false),
      (Type::Fn(x, a), Type::Fn(y, b)) if !inverted => {
        let mut success = true;
        success &= x.len() == y.len();
        for (x, y) in x.iter_mut().zip(y) {
          success &= self._unify(x, y, false);
        }
        success &= self._unify(a, b, false);
        success
      }
      (Type::Adt(AdtId(n), a), Type::Adt(AdtId(m), b))
      | (Type::Trait(TraitDefId(n), a), Type::Trait(TraitDefId(m), b))
        if n == m && !inverted =>
      {
        let mut success = true;
        for (a, b) in a.iter_mut().zip(b) {
          success &= self._unify(a, b, false);
        }
        success
      }
      _ => false,
    }
  }

  pub fn concretize(&mut self, ty: &mut Type<'core>) {
    while let Type::Var(v) = *ty {
      let v = &self.vars[v];
      *ty = if let Some((_, t)) = &v.bound {
        t.clone()
      } else {
        let err = self.core.report(Diag::CannotInfer { span: v.span });
        Type::Error(err)
      };
    }
    if let Type::Inverse(t) = ty {
      self.concretize(t);
      if let Type::Inverse(t) = &mut **t {
        *ty = take(&mut **t);
      }
    }
  }

  pub fn try_concretize(&self, ty: &mut Type<'core>) {
    while let Type::Var(v) = *ty {
      if let Some((_, t)) = &self.vars[v].bound {
        *ty = t.clone()
      } else {
        break;
      }
    }
    if let Type::Inverse(t) = ty {
      self.try_concretize(t);
      if let Type::Inverse(t) = &mut **t {
        *ty = take(&mut **t);
      }
    }
  }

  pub fn checkpoint(&mut self) -> UnifierCheckpoint {
    let checkpoint = UnifierCheckpoint { vars: self.vars.next_index(), version: self.version };
    self.version.0 += 1;
    checkpoint
  }

  pub fn reset(&mut self) {
    self.vars.clear();
    self.version = Version(0);
  }

  pub fn revert(&mut self, checkpoint: &UnifierCheckpoint) {
    self.vars.truncate(checkpoint.vars.0);
    for var_info in self.vars.values_mut() {
      if let Some((version, _)) = var_info.bound {
        if version > checkpoint.version {
          var_info.bound = None;
        }
      }
    }
    self.version = checkpoint.version;
  }

  pub fn export(&mut self, checkpoint: &UnifierCheckpoint, ty: &mut Type<'core>) {
    loop {
      if let Type::Var(var) = ty {
        let info = &self.vars[*var];
        if let Some((version, bound)) = &info.bound {
          if *version > checkpoint.version {
            *ty = bound.clone();
            continue;
          }
        }
        if *var >= checkpoint.vars {
          *ty = Type::Fresh(*var);
        }
      }
      break;
    }
    for child in ty.children_mut() {
      self.export(checkpoint, child);
    }
  }

  pub fn import<'a, 'b: 'a>(
    &mut self,
    span: Span,
    tys: impl IntoIterator<Item = &'a mut Type<'b>>,
  ) {
    let mut fresh = HashMap::new();
    for ty in tys {
      self._import(span, &mut fresh, ty);
    }
  }

  fn _import(&mut self, span: Span, fresh: &mut HashMap<Var, Var>, ty: &mut Type) {
    if let Type::Fresh(var) = ty {
      *ty = Type::Var(*fresh.entry(*var).or_insert_with(|| self._new_var(span)))
    }
    for child in ty.children_mut() {
      self._import(span, fresh, child);
    }
  }
}

new_idx!(pub(crate) Version);

#[derive(Debug)]
pub(crate) struct VarInfo<'core> {
  span: Span,
  pub(crate) bound: Option<(Version, Type<'core>)>,
}

#[derive(Debug)]
pub struct UnifierCheckpoint {
  vars: Var,
  version: Version,
}

#[must_use]
#[derive(Debug, Clone, Copy)]
pub enum UnificationResult {
  Success,
  Failure,
  Indeterminate(ErrorGuaranteed),
}

impl UnificationResult {
  pub fn success(self) -> bool {
    matches!(self, UnificationResult::Success)
  }

  pub fn failure(self) -> bool {
    matches!(self, UnificationResult::Failure)
  }
}
