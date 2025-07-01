use vine_util::parser::Parser;

use crate::{
  components::{
    distiller::Distiller,
    lexer::Token,
    parser::{VineParser, BP},
    resolver::{Resolver, TargetInfo},
  },
  structures::{
    ast::{Expr, ExprKind, Label, Span, Target},
    diag::Diag,
    tir::{TargetId, TirExpr, TirExprKind},
    vir::{Port, PortKind, Stage, Step, Transfer},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_label(&mut self) -> Result<Label<'core>, Diag<'core>> {
    Ok(Label(self.eat(Token::Dot)?.then(|| self.parse_ident()).transpose()?))
  }

  pub(crate) fn parse_target(&mut self) -> Result<Target<'core>, Diag<'core>> {
    Ok(if self.eat(Token::Dot)? {
      if self.eat(Token::Do)? {
        Target::Do
      } else if self.eat(Token::Loop)? {
        Target::Loop
      } else if self.eat(Token::While)? {
        Target::While
      } else if self.eat(Token::For)? {
        Target::For
      } else if self.eat(Token::When)? {
        Target::When
      } else {
        Target::Label(self.parse_ident()?)
      }
    } else {
      Target::AnyLoop
    })
  }

  pub(crate) fn parse_expr_break(&mut self) -> Result<ExprKind<'core>, Diag<'core>> {
    let target = self.parse_target()?;
    let expr = self.maybe_parse_expr_bp(BP::Min)?;
    Ok(ExprKind::Break(target, expr))
  }

  pub(crate) fn parse_expr_continue(&mut self) -> Result<ExprKind<'core>, Diag<'core>> {
    Ok(ExprKind::Continue(self.parse_target()?))
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_label(&self, label: Label<'core>) -> Doc<'src> {
    if let Some(label) = label.0 {
      Doc::concat([Doc("."), Doc(label)])
    } else {
      Doc("")
    }
  }

  pub(crate) fn fmt_target(&self, target: Target<'core>) -> Doc<'src> {
    match target {
      Target::AnyLoop => Doc(""),
      Target::Label(label) => Doc::concat([Doc("."), Doc(label)]),
      Target::Do => Doc(".do"),
      Target::Loop => Doc(".loop"),
      Target::While => Doc(".while"),
      Target::For => Doc(".for"),
      Target::When => Doc(".when"),
    }
  }

  pub(crate) fn fmt_expr_break(
    &self,
    target: Target<'core>,
    expr: &Option<Expr<'core>>,
  ) -> Doc<'src> {
    match expr {
      Some(expr) => {
        Doc::concat([Doc("break"), self.fmt_target(target), Doc(" "), self.fmt_expr(expr)])
      }
      None => Doc::concat([Doc("break"), self.fmt_target(target)]),
    }
  }

  pub(crate) fn fmt_expr_continue(&self, target: Target<'core>) -> Doc<'src> {
    Doc::concat([Doc("continue"), self.fmt_target(target)])
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn bind_target<T>(
    &mut self,
    label: Label<'core>,
    targets: impl IntoIterator<IntoIter: Clone, Item = Target<'core>>,
    kind: TargetInfo,
    f: impl FnOnce(&mut Self) -> T,
  ) -> T {
    let targets = targets.into_iter().chain(label.0.map(|ident| Target::Label(ident)));
    for target in targets.clone() {
      self.targets.entry(target).or_default().push(kind);
    }
    let result = f(self);
    for target in targets {
      self.targets.get_mut(&target).unwrap().pop();
    }
    result
  }

  pub(crate) fn resolve_expr_break(
    &mut self,
    span: Span,
    target: Target<'core>,
    value: &Option<Expr<'core>>,
  ) -> Result<TirExpr, Diag<'core>> {
    let nil = self.types.nil();
    match self.targets.get(&target).and_then(|x| x.last()) {
      Some(&target) => {
        let value = match value {
          Some(value) => Some(self.resolve_expr_type(value, target.break_ty)),
          None => {
            if self.types.unify(target.break_ty, nil).is_failure() {
              self.core.report(Diag::MissingBreakExpr {
                span,
                ty: self.types.show(self.chart, target.break_ty),
              });
            }
            None
          }
        };
        Ok(TirExpr::new(span, self.types.new_var(span), TirExprKind::Break(target.id, value)))
      }
      None => {
        if let Some(value) = value {
          self.resolve_expr(value);
        }
        Err(Diag::InvalidBreakTarget { span })?
      }
    }
  }

  pub(crate) fn resolve_expr_continue(
    &mut self,
    span: Span,
    target: Target<'core>,
  ) -> Result<TirExpr, Diag<'core>> {
    match self.targets.get(&target).and_then(|x| x.last()) {
      Some(target) if target.continue_ => {
        Ok(TirExpr::new(span, self.types.new_var(span), TirExprKind::Continue(target.id)))
      }
      _ => Err(Diag::InvalidContinueTarget { span })?,
    }
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_break(
    &mut self,
    stage: &mut Stage,
    label: TargetId,
    value: &Option<TirExpr>,
  ) {
    let value = match value {
      Some(v) => self.distill_expr_value(stage, v),
      None => Port { ty: self.types.nil(), kind: PortKind::Nil },
    };

    let label = self.targets[label].as_ref().unwrap();
    stage.local_barrier_write_to(label.break_value, value);

    stage.steps.push(Step::Diverge(label.layer, None));
  }

  pub(crate) fn distill_continue(&mut self, stage: &mut Stage, label: TargetId) {
    let label = self.targets[label].as_ref().unwrap();
    let transfer = Transfer::unconditional(label.continue_transfer.unwrap());
    stage.steps.push(Step::Diverge(label.layer, Some(transfer)));
  }
}
