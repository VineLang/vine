use crate::{
  checker::{Checker, Type},
  diag::Diag,
};

impl Checker<'_> {
  #[must_use]
  pub(super) fn unify(&mut self, a: &mut Type, b: &mut Type) -> bool {
    self._unify(a, b, false, false)
  }

  #[must_use]
  fn _unify(&mut self, a: &mut Type, b: &mut Type, i: bool, j: bool) -> bool {
    match (&mut *a, &mut *b) {
      (Type::Error(_), _) | (_, Type::Error(_)) => true,
      (Type::Inverse(a), Type::Inverse(b)) => self._unify(a, b, !i, !j),
      (a, Type::Inverse(b)) => self._unify(a, b, i, !j),
      (Type::Inverse(a), b) => self._unify(a, b, !i, j),
      (Type::Var(v), Type::Var(u)) if *v == *u => i == j,
      (Type::Var(v), Type::Var(u)) => {
        let (v, u) = get2_mut(&mut self.state.vars, *v, *u);
        match (&mut *v, &mut *u) {
          (Ok(v), Ok(u)) => {
            *a = v.clone();
            *b = u.clone();
            self._unify(a, b, i, j)
          }
          (Ok(v), Err(_)) => {
            *a = v.clone();
            self._unify(a, b, i, j)
          }
          (Err(_), Ok(u)) => {
            *b = u.clone();
            self._unify(a, b, i, j)
          }
          (Err(_), Err(_)) => {
            *v = Ok(b.clone().invert_if(i != j));
            *a = b.clone().invert_if(i != j);
            true
          }
        }
      }
      (&mut ref mut o @ Type::Var(v), t) | (t, &mut ref mut o @ Type::Var(v)) => {
        let v = &mut self.state.vars[v];
        if let Ok(u) = v {
          *o = u.clone();
          self._unify(a, b, i, j)
        } else {
          *o = t.clone();
          *v = Ok(t.clone().invert_if(i != j));
          true
        }
      }
      (Type::U32, Type::U32) | (Type::F32, Type::F32) | (Type::IO, Type::IO) if i == j => true,
      (Type::Opaque(a), Type::Opaque(b)) if a == b && i == j => true,
      (Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => {
        let mut success = true;
        for (a, b) in a.iter_mut().zip(b) {
          success &= self._unify(a, b, i, j);
        }
        success
      }
      (Type::Ref(a), Type::Ref(b)) if i == j => self._unify(a, b, i, j),
      (Type::Fn(x, a), Type::Fn(y, b)) if i == j => {
        let mut success = true;
        for (x, y) in x.iter_mut().zip(y) {
          success &= self._unify(x, y, i, j);
        }
        success &= self._unify(a, b, i, j);
        success
      }
      (Type::Adt(n, a), Type::Adt(m, b)) if n == m && i == j => {
        let mut success = true;
        for (a, b) in a.iter_mut().zip(b) {
          success &= self._unify(a, b, i, j);
        }
        success
      }
      _ => false,
    }
  }

  pub(super) fn concretize(&mut self, ty: &mut Type) {
    while let Type::Var(v) = *ty {
      match self.state.vars[v].clone() {
        Ok(t) => *ty = t,
        Err(span) => {
          let err = self.diags.add(Diag::CannotInfer { span });
          *ty = Type::Error(err);
        }
      }
    }
  }
}

fn get2_mut<T>(slice: &mut [T], a: usize, b: usize) -> (&mut T, &mut T) {
  assert!(a != b);
  if a < b {
    let (l, r) = slice.split_at_mut(b);
    (&mut l[a], &mut r[0])
  } else {
    let (l, r) = slice.split_at_mut(a);
    (&mut r[0], &mut l[b])
  }
}
