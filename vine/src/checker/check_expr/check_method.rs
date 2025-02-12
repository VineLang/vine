use std::mem::take;

use crate::{
  ast::{Expr, ExprKind, GenericArgs, Ident, Span},
  chart::ValueDefId,
  checker::{Checker, Form, Type},
  diag::{Diag, ErrorGuaranteed},
};

impl<'core> Checker<'core, '_> {
  pub(super) fn check_method(
    &mut self,
    span: Span,
    receiver: &mut Box<Expr<'core>>,
    name: Ident<'core>,
    generics: &mut GenericArgs<'core>,
    args: &mut Vec<Expr<'core>>,
  ) -> (ExprKind<'core>, Type<'core>) {
    match self._check_method(span, receiver, name, generics, args) {
      Ok((form, id, ret)) => (self.desugar_method(span, receiver, args, id, generics, form), ret),
      Err(e) => {
        for arg in args {
          self.check_expr_form(arg, Form::Value);
        }
        let err = self.core.report(e);
        (ExprKind::Error(err), Type::Error(err))
      }
    }
  }

  fn _check_method(
    &mut self,
    span: Span,
    receiver: &mut Box<Expr<'core>>,
    ident: Ident<'core>,
    generics: &mut GenericArgs<'core>,
    args: &mut [Expr<'core>],
  ) -> Result<(Form, ValueDefId, Type<'core>), Diag<'core>> {
    let (receiver_form, mut ty) = self.check_expr(receiver);
    self.unifier.concretize(&mut ty);
    let (id, type_params) = self.find_method(span, &ty, ident)?;
    let (form, mut receiver_ty, params, ret) =
      self.method_sig(span, id, generics, type_params, args.len())?;
    self.coerce_expr(receiver, receiver_form, form);
    if !self.unifier.unify(&mut ty, &mut receiver_ty) {
      Err(Diag::ExpectedTypeFound {
        span: receiver.span,
        expected: self.display_type(&receiver_ty),
        found: self.display_type(&ty),
      })?
    }
    for (mut ty, arg) in params.into_iter().skip(1).zip(args.iter_mut()) {
      self.check_expr_form_type(arg, Form::Value, &mut ty);
    }
    Ok((form, id, ret))
  }

  fn method_sig(
    &mut self,
    span: Span,
    id: ValueDefId,
    generics: &mut GenericArgs<'core>,
    type_params: Vec<Type<'core>>,
    args: usize,
  ) -> Result<(Form, Type<'core>, Vec<Type<'core>>, Type<'core>), Diag<'core>> {
    let type_params =
      self._check_generics(generics, self.chart.values[id].generics, true, Some(type_params));
    let ty = self.types.value_types[id].instantiate(&type_params);
    match ty {
      Type::Fn(mut params, ret) => {
        if params.len() != args + 1 {
          return Err(Diag::BadArgCount {
            span,
            expected: params.len(),
            got: args + 1,
            ty: self.display_type(&Type::Fn(params, ret)),
          });
        }
        let (form, receiver) = match params.first_mut() {
          Some(Type::Error(e)) => return Err((*e).into()),
          None => {
            return Err(Diag::NilaryMethod { span, ty: self.display_type(&Type::Fn(params, ret)) })
          }
          Some(Type::Ref(receiver)) => (Form::Place, &mut **receiver),
          Some(receiver) => (Form::Value, receiver),
        };
        let receiver = take(receiver);
        Ok((form, receiver, params, *ret))
      }
      Type::Error(e) => Err(e.into()),
      ty => Err(Diag::NonFunctionCall { span, ty: self.display_type(&ty) }),
    }
  }

  fn desugar_method(
    &mut self,
    span: Span,
    receiver: &mut Box<Expr<'core>>,
    args: &mut Vec<Expr<'core>>,
    id: ValueDefId,
    generics: &mut GenericArgs<'core>,
    form: Form,
  ) -> ExprKind<'core> {
    let mut receiver = take(&mut **receiver);
    if form == Form::Place {
      receiver = Expr { span: receiver.span, kind: ExprKind::Ref(Box::new(receiver), false) };
    }
    let mut args = take(args);
    args.insert(0, receiver);
    let func = Expr { span, kind: ExprKind::Def(id, take(generics)) };
    ExprKind::Call(Box::new(func), args)
  }

  fn find_method(
    &mut self,
    span: Span,
    receiver: &Type<'core>,
    name: Ident<'core>,
  ) -> Result<(ValueDefId, Vec<Type<'core>>), ErrorGuaranteed> {
    let mut results = self.finder(span).find_method(receiver, name);
    if results.len() == 1 {
      Ok(results.pop().unwrap())
    } else {
      Err(self.core.report(if results.is_empty() {
        Diag::NoMethod { span, ty: self.display_type(receiver), name }
      } else {
        Diag::AmbiguousMethod { span, ty: self.display_type(receiver), name }
      }))
    }
  }
}
