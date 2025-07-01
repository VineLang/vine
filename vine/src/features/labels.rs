use vine_util::parser::Parser;

use crate::{
  components::{
    distiller::Distiller,
    lexer::Token,
    parser::{VineParser, BP},
    resolver::{LabelInfo, Resolver},
  },
  structures::{
    ast::{Expr, ExprKind, Ident, Span},
    diag::Diag,
    tir::{LabelId, TirExpr, TirExprKind},
    types::Type,
    vir::{Port, PortKind, Stage, Step, Transfer},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_label(&mut self) -> Result<Option<Ident<'core>>, Diag<'core>> {
    self.eat(Token::Dot)?.then(|| self.parse_ident()).transpose()
  }

  pub(crate) fn parse_expr_break(&mut self) -> Result<ExprKind<'core>, Diag<'core>> {
    let label = self.parse_label()?;
    let expr = self.maybe_parse_expr_bp(BP::Min)?;
    Ok(ExprKind::Break(label, expr))
  }

  pub(crate) fn parse_expr_continue(&mut self) -> Result<ExprKind<'core>, Diag<'core>> {
    Ok(ExprKind::Continue(self.parse_label()?))
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_label(&self, label: Option<Ident<'core>>) -> Doc<'src> {
    if let Some(label) = label {
      Doc::concat([Doc("."), Doc(label)])
    } else {
      Doc("")
    }
  }

  pub(crate) fn fmt_expr_break(
    &self,
    label: Option<Ident<'core>>,
    expr: &Option<Expr<'core>>,
  ) -> Doc<'src> {
    match expr {
      Some(expr) => {
        Doc::concat([Doc("break"), self.fmt_label(label), Doc(" "), self.fmt_expr(expr)])
      }
      None => Doc::concat([Doc("break"), self.fmt_label(label)]),
    }
  }

  pub(crate) fn fmt_expr_continue(&self, label: Option<Ident<'core>>) -> Doc<'src> {
    Doc::concat([Doc("continue"), self.fmt_label(label)])
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn bind_label<T>(
    &mut self,
    label: Option<Ident<'core>>,
    is_loop: bool,
    break_ty: Type,
    f: impl FnOnce(&mut Self) -> T,
  ) -> (LabelId, T) {
    let id = self.label_id.next();
    let info = LabelInfo { id, is_loop, break_ty };
    if is_loop {
      self.loops.push(info);
    }
    let result;
    if let Some(label) = label {
      let old = self.labels.insert(label, info);
      result = f(self);
      if let Some(old) = old {
        self.labels.insert(label, old);
      } else {
        self.labels.remove(&label);
      }
    } else {
      result = f(self);
    }
    if is_loop {
      self.loops.pop();
    }
    (id, result)
  }

  pub(crate) fn resolve_expr_break(
    &mut self,
    span: Span,
    label: Option<Ident<'core>>,
    value: &Option<Expr<'core>>,
  ) -> Result<TirExpr, Diag<'core>> {
    let nil = self.types.nil();
    let label_info = if let Some(label) = label {
      self.labels.get(&label).copied().ok_or(Diag::UnboundLabel { span, label })
    } else {
      self.loops.last().copied().ok_or(Diag::NoLoopBreak { span })
    };
    match label_info {
      Ok(label_info) => {
        let value = match value {
          Some(value) => Some(self.resolve_expr_type(value, label_info.break_ty)),
          None => {
            if self.types.unify(label_info.break_ty, nil).is_failure() {
              self.core.report(Diag::MissingBreakExpr {
                span,
                ty: self.types.show(self.chart, label_info.break_ty),
              });
            }
            None
          }
        };
        Ok(TirExpr::new(span, self.types.new_var(span), TirExprKind::Break(label_info.id, value)))
      }
      Err(diag) => {
        if let Some(value) = value {
          self.resolve_expr(value);
        }
        Err(diag)?
      }
    }
  }

  pub(crate) fn resolve_expr_continue(
    &mut self,
    span: Span,
    label: Option<Ident<'core>>,
  ) -> Result<TirExpr, Diag<'core>> {
    let label_info = if let Some(label) = label {
      let label_info = *self.labels.get(&label).ok_or(Diag::UnboundLabel { span, label })?;
      if !label_info.is_loop {
        Err(Diag::NoContinueLabel { span, label })?
      }
      label_info
    } else {
      self.loops.last().copied().ok_or(Diag::NoLoopContinue { span })?
    };
    Ok(TirExpr::new(span, self.types.new_var(span), TirExprKind::Continue(label_info.id)))
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_break(
    &mut self,
    stage: &mut Stage,
    label: LabelId,
    value: &Option<TirExpr>,
  ) {
    let value = match value {
      Some(v) => self.distill_expr_value(stage, v),
      None => Port { ty: self.types.nil(), kind: PortKind::Nil },
    };

    let label = self.labels[label].as_ref().unwrap();
    if let Some(local) = label.break_value {
      stage.local_barrier_write_to(local, value);
    } else {
      stage.steps.push(Step::Link(value, Port { ty: self.types.nil(), kind: PortKind::Nil }));
    }

    stage.steps.push(Step::Diverge(label.layer, None));
  }

  pub(crate) fn distill_continue(&mut self, stage: &mut Stage, label: LabelId) {
    let label = self.labels[label].as_ref().unwrap();
    let transfer = Transfer::unconditional(label.continue_transfer.unwrap());
    stage.steps.push(Step::Diverge(label.layer, Some(transfer)));
  }
}
