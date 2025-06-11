use crate::{
  components::resolver::{Form, Resolver, Type},
  structures::{
    ast::{Expr, GenericArgs, Ident, Span},
    chart::FnId,
    diag::{Diag, ErrorGuaranteed},
    resolutions::FnRel,
    signatures::FnSig,
    tir::{TirExpr, TirExprKind},
    types::{Inverted, TypeCtx, TypeKind},
  },
};

impl<'core> Resolver<'core, '_> {
  pub(super) fn resolve_method(
    &mut self,
    span: Span,
    receiver: &Expr<'core>,
    name: Ident<'core>,
    generics: &GenericArgs<'core>,
    args: &[Expr<'core>],
  ) -> Result<(Form, Type, TirExprKind), Diag<'core>> {
    match self._resolve_method(span, receiver, name, generics, args) {
      Ok((ty, kind)) => Ok((Form::Value, ty, kind)),
      Err(diag) => {
        for arg in args {
          self.resolve_expr_form(arg, Form::Value);
        }
        Err(diag)
      }
    }
  }

  fn _resolve_method(
    &mut self,
    span: Span,
    receiver: &Expr<'core>,
    ident: Ident<'core>,
    generics: &GenericArgs<'core>,
    args: &[Expr<'core>],
  ) -> Result<(Type, TirExprKind), Diag<'core>> {
    let mut receiver = self.resolve_expr(receiver);
    let (fn_id, type_params) = self.find_method(span, receiver.ty, ident)?;
    let type_params = self.types.import(&type_params, None);
    let (type_params, impl_params) = self._resolve_generics(
      span,
      Some(generics),
      self.chart.fn_generics(fn_id),
      true,
      Some(type_params),
    );
    let FnSig { params, ret_ty } = self.types.import(self.sigs.fn_sig(fn_id), Some(&type_params));
    if params.len() != args.len() + 1 {
      return Err(Diag::BadArgCount { span, expected: params.len(), got: args.len() + 1 });
    }
    let (receiver_form, receiver_ty) = match params.first().copied() {
      None => return Err(Diag::NilaryMethod { span }),
      Some(receiver) => match self.types.force_kind(self.core, receiver) {
        (_, TypeKind::Error(e)) => return Err((*e).into()),
        (Inverted(false), TypeKind::Ref(receiver)) => (Form::Place, *receiver),
        _ => (Form::Value, receiver),
      },
    };
    self.coerce_expr(&mut receiver, receiver_form);
    self.expect_type(span, receiver.ty, receiver_ty);
    let receiver = if receiver_form == Form::Place {
      TirExpr {
        span,
        ty: self.types.new(TypeKind::Ref(receiver.ty)),
        form: Form::Value,
        kind: Box::new(TirExprKind::Ref(receiver)),
      }
    } else {
      receiver
    };
    let args = Vec::from_iter(
      [receiver].into_iter().chain(
        args
          .iter()
          .zip(params.into_iter().skip(1))
          .map(|(arg, ty)| self.resolve_expr_form_type(arg, Form::Value, ty)),
      ),
    );
    Ok((ret_ty, TirExprKind::Call(self.rels.fns.push(FnRel::Item(fn_id, impl_params)), None, args)))
  }

  fn find_method(
    &mut self,
    span: Span,
    receiver: Type,
    name: Ident<'core>,
  ) -> Result<(FnId, TypeCtx<'core, Vec<Type>>), ErrorGuaranteed> {
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
