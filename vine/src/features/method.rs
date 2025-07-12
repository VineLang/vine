use crate::{
  components::resolver::Resolver,
  structures::{
    ast::{Expr, GenericArgs, Ident, Span},
    chart::FnId,
    diag::{Diag, ErrorGuaranteed},
    resolutions::FnRel,
    tir::{TirExpr, TirExprKind},
    types::{Inverted, Type, TypeCtx, TypeKind},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_expr_method(
    &self,
    receiver: &Expr<'core>,
    name: &Ident<'core>,
    generics: &GenericArgs<'core>,
    args: &Vec<Expr<'core>>,
  ) -> Doc<'src> {
    Doc::concat([
      self.fmt_expr(receiver),
      Doc("."),
      Doc(*name),
      self.fmt_generic_args(generics),
      Doc::paren_comma(args.iter().map(|x| self.fmt_expr(x))),
    ])
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_method(
    &mut self,
    span: Span,
    receiver: &Expr<'core>,
    name: Ident<'core>,
    generics: &GenericArgs<'core>,
    args: &[Expr<'core>],
  ) -> Result<TirExpr, Diag<'core>> {
    let receiver = self.resolve_expr(receiver);
    let mut args = args.iter().map(|arg| self.resolve_expr(arg)).collect::<Vec<_>>();
    let (fn_id, type_params) = self.find_method(span, receiver.ty, name)?;
    let type_params = self.types.import(&type_params, None);
    let sig = self.types.import(self.sigs.fn_sig(fn_id), Some(&type_params));
    if sig.params.len() != args.len() + 1 {
      return Err(Diag::BadArgCount { span, expected: sig.params.len(), got: args.len() + 1 });
    }
    for (arg, ty) in args.iter().zip(sig.params.iter().skip(1)) {
      // just need inference; errors will be reported later
      _ = self.types.unify(arg.ty, *ty);
    }
    let (type_params, impl_params) = self._resolve_generics(
      span,
      Some(generics),
      self.chart.fn_generics(fn_id),
      true,
      Some(type_params),
    );
    let sig = self.types.import(self.sigs.fn_sig(fn_id), Some(&type_params));
    for (arg, ty) in args.iter().zip(sig.params.iter().skip(1)) {
      self.expect_type(arg.span, arg.ty, *ty);
    }
    let (receiver_place, receiver_ty) = match sig.params.first().copied() {
      None => return Err(Diag::NilaryMethod { span }),
      Some(receiver) => match self.types.force_kind(self.core, receiver) {
        (_, TypeKind::Error(e)) => return Err((*e).into()),
        (Inverted(false), TypeKind::Ref(receiver)) => (true, *receiver),
        _ => (false, receiver),
      },
    };
    self.expect_type(span, receiver.ty, receiver_ty);
    let receiver = if receiver_place {
      TirExpr {
        span,
        ty: self.types.new(TypeKind::Ref(receiver.ty)),
        kind: Box::new(TirExprKind::Ref(receiver)),
      }
    } else {
      receiver
    };
    args.insert(0, receiver);
    Ok(TirExpr::new(
      span,
      sig.ret_ty,
      TirExprKind::Call(self.rels.fns.push(FnRel::Item(fn_id, impl_params)), None, args),
    ))
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
