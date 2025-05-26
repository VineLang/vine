use std::mem::take;

use crate::{
  ast::{Expr, ExprKind, GenericArgs, Ident, Span},
  chart::ValueDefId,
  checker::{Checker, Form, Type},
  diag::{Diag, ErrorGuaranteed},
  types::{Inverted, TypeCtx, TypeKind},
};

impl<'core> Checker<'core, '_> {
  pub(super) fn check_method(
    &mut self,
    span: Span,
    receiver: &mut Box<Expr<'core>>,
    name: Ident<'core>,
    generics: &mut GenericArgs<'core>,
    args: &mut Vec<Expr<'core>>,
  ) -> (ExprKind<'core>, Type) {
    match self._check_method(span, receiver, name, generics, args) {
      Ok((form, id, ret)) => (self.desugar_method(span, receiver, args, id, generics, form), ret),
      Err(e) => {
        for arg in args {
          self.check_expr_form(arg, Form::Value);
        }
        let err = self.core.report(e);
        (ExprKind::Error(err), self.types.error(err))
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
  ) -> Result<(Form, ValueDefId, Type), Diag<'core>> {
    let (receiver_form, ty) = self.check_expr(receiver);
    let (id, type_params) = self.find_method(span, ty, ident)?;
    let type_params =
      self.types.import(&type_params, None, |t, p| p.iter().map(|&ty| t.transfer(ty)).collect());
    let (form, receiver_ty, params, ret) =
      self.method_sig(span, id, generics, type_params, args.len())?;
    self.coerce_expr(receiver, receiver_form, form);
    if !self.types.unify(ty, receiver_ty).is_ok() {
      Err(Diag::ExpectedTypeFound {
        span: receiver.span,
        expected: self.types.show(self.chart, receiver_ty),
        found: self.types.show(self.chart, ty),
      })?
    }
    for (ty, arg) in params.into_iter().skip(1).zip(args.iter_mut()) {
      self.check_expr_form_type(arg, Form::Value, ty);
    }
    Ok((form, id, ret))
  }

  fn method_sig(
    &mut self,
    span: Span,
    id: ValueDefId,
    generics: &mut GenericArgs<'core>,
    type_params: Vec<Type>,
    args: usize,
  ) -> Result<(Form, Type, Vec<Type>, Type), Diag<'core>> {
    let type_params =
      self._check_generics(generics, self.chart.values[id].generics, true, Some(type_params));
    let ty =
      self.types.import(&self.sigs.values[id], Some(&type_params), |t, sig| t.transfer(sig.ty));
    match self.types.force_kind(self.core, ty) {
      (Inverted(false), TypeKind::Fn(params, ret)) => {
        if params.len() != args + 1 {
          return Err(Diag::BadArgCount {
            span,
            expected: params.len(),
            got: args + 1,
            ty: self.types.show(self.chart, ty),
          });
        }
        let params = params.clone();
        let ret = *ret;
        let (form, receiver) = match params.first().copied() {
          None => return Err(Diag::NilaryMethod { span, ty: self.types.show(self.chart, ty) }),
          Some(receiver) => match self.types.force_kind(self.core, receiver) {
            (_, TypeKind::Error(e)) => return Err((*e).into()),
            (Inverted(false), TypeKind::Ref(receiver)) => (Form::Place, *receiver),
            _ => (Form::Value, receiver),
          },
        };
        Ok((form, receiver, params, ret))
      }
      (_, TypeKind::Error(e)) => Err(*e)?,
      _ => Err(Diag::NonFunctionCall { span, ty: self.types.show(self.chart, ty) }),
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
    receiver: Type,
    name: Ident<'core>,
  ) -> Result<(ValueDefId, TypeCtx<'core, Vec<Type>>), ErrorGuaranteed> {
    let mut results = self.finder(span).find_method(&self.types, receiver, name);
    if results.len() == 1 {
      Ok(results.pop().unwrap())
    } else {
      Err(self.core.report(if results.is_empty() {
        Diag::NoMethod { span, ty: self.types.show(self.chart, receiver), name }
      } else {
        Diag::AmbiguousMethod { span, ty: self.types.show(self.chart, receiver), name }
      }))
    }
  }
}
