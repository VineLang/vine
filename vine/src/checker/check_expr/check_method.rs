use std::mem::{replace, take};

use crate::{
  ast::{Expr, ExprKind, GenericPath, Span},
  checker::{Checker, Form, Type},
  diag::Diag,
  resolver::DefId,
};

impl<'core> Checker<'core, '_> {
  pub(super) fn check_method(
    &mut self,
    span: Span,
    receiver: &mut Box<Expr<'core>>,
    path: &mut GenericPath<'core>,
    args: &mut Vec<Expr<'core>>,
  ) -> (ExprKind<'core>, Type) {
    if path.path.resolved.is_some() {
      match self.method_sig(span, path, args.len()) {
        Ok((form, mut rec, params, ret)) => {
          self.check_expr_form_type(receiver, form, &mut rec);
          for (mut ty, arg) in params.into_iter().skip(1).zip(args.iter_mut()) {
            self.check_expr_form_type(arg, Form::Value, &mut ty);
          }
          (self.desugar_method(receiver, args, path, form), ret)
        }
        Err(e) => {
          self.check_expr_form(receiver, Form::Place);
          for arg in args {
            self.check_expr_form(arg, Form::Value);
          }
          let err = self.core.report(e);
          (ExprKind::Error(err), Type::Error(err))
        }
      }
    } else {
      match self.check_associated_method(span, receiver, path, args) {
        Ok((form, ret)) => (self.desugar_method(receiver, args, path, form), ret),
        Err(e) => {
          for arg in args {
            self.check_expr_form(arg, Form::Value);
          }
          let err = self.core.report(e);
          (ExprKind::Error(err), Type::Error(err))
        }
      }
    }
  }

  fn check_associated_method(
    &mut self,
    span: Span,
    receiver: &mut Box<Expr<'core>>,
    path: &mut GenericPath<'core>,
    args: &mut [Expr<'core>],
  ) -> Result<(Form, Type), Diag<'core>> {
    let (receiver_form, mut ty) = self.check_expr(receiver);
    self.concretize(&mut ty);
    let mod_id = self.get_ty_mod(&ty)?;
    let Some(mod_id) = mod_id else { Err(Diag::NoMethods { span, ty: self.display_type(&ty) })? };
    let fn_id = self.resolver.resolve_path(path.span, self.cur_def, mod_id, &path.path)?;
    let sub_path = replace(&mut path.path, self.resolver.defs[fn_id].canonical.clone());
    let (form, mut receiver_ty, params, ret) = self.method_sig(span, path, args.len())?;
    if self.get_ty_mod(&receiver_ty)? != Some(mod_id) {
      Err(Diag::BadMethodReceiver {
        span: path.span,
        base_path: self.resolver.defs[mod_id].canonical.clone(),
        sub_path,
      })?
    }
    self.coerce_expr(receiver, receiver_form, form);
    if !self.unify(&mut ty, &mut receiver_ty) {
      Err(Diag::ExpectedTypeFound {
        span: receiver.span,
        expected: self.display_type(&receiver_ty),
        found: self.display_type(&ty),
      })?
    }
    for (mut ty, arg) in params.into_iter().skip(1).zip(args.iter_mut()) {
      self.check_expr_form_type(arg, Form::Value, &mut ty);
    }
    Ok((form, ret))
  }

  fn method_sig(
    &mut self,
    span: Span,
    path: &mut GenericPath<'core>,
    args: usize,
  ) -> Result<(Form, Type, Vec<Type>, Type), Diag<'core>> {
    let ty = self.typeof_value_def(path)?;
    match ty {
      Type::Fn(mut params, ret) => {
        let (form, receiver) = match params.first_mut() {
          Some(Type::Error(e)) => return Err((*e).into()),
          None => {
            return Err(Diag::NilaryMethod { span, ty: self.display_type(&Type::Fn(params, ret)) })
          }
          Some(Type::Ref(receiver)) => (Form::Place, &mut **receiver),
          Some(receiver) => (Form::Value, receiver),
        };
        let receiver = take(receiver);
        if params.len() != args + 1 {
          Err(Diag::BadArgCount {
            span,
            expected: params.len(),
            got: args,
            ty: self.display_type(&Type::Fn(params, ret)),
          })
        } else {
          Ok((form, receiver, params, *ret))
        }
      }
      Type::Error(e) => Err(e.into()),
      ty => Err(Diag::NonFunctionCall { span, ty: self.display_type(&ty) }),
    }
  }

  fn desugar_method(
    &mut self,
    receiver: &mut Box<Expr<'core>>,
    args: &mut Vec<Expr<'core>>,
    path: &mut GenericPath<'core>,
    form: Form,
  ) -> ExprKind<'core> {
    let mut receiver = take(&mut **receiver);
    if form == Form::Place {
      receiver = Expr { span: receiver.span, kind: ExprKind::Ref(Box::new(receiver)) };
    }
    let mut args = take(args);
    args.insert(0, receiver);
    let path = take(path);
    let func = Expr { span: path.span, kind: ExprKind::Path(path) };
    ExprKind::Call(Box::new(func), args)
  }

  fn get_ty_mod(&mut self, ty: &Type) -> Result<Option<DefId>, Diag<'core>> {
    Ok(match ty {
      Type::Adt(mod_id, _) => Some(*mod_id),
      Type::Bool => self.bool,
      Type::N32 => self.n32,
      Type::F32 => self.f32,
      Type::Char => self.char,
      Type::IO => self.io,
      Type::Tuple(_) | Type::Fn(..) | Type::Ref(_) | Type::Inverse(_) | Type::Opaque(_) => None,
      Type::Var(_) => unreachable!(),
      Type::Error(e) => Err(*e)?,
    })
  }
}
