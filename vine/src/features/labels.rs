use vine_util::parser::Parse;

use crate::{
  components::{
    distiller::Distiller,
    lexer::Token,
    parser::{BP, Parser},
    resolver::{Resolver, TargetInfo},
  },
  structures::{
    ast::{Expr, Label, Span, StmtKind, Target},
    content::{Content, Indent, Keyword, Punct, Space},
    diag::Diag,
    tir::{TargetId, TirExpr, TirExprKind},
    vir::{Port, PortKind, Stage, Step, Transfer},
  },
  tools::fmt::Formatter,
};

impl Parser<'_> {
  pub(crate) fn parse_label(&mut self) -> Result<Label, Diag> {
    Ok(Label(self.eat_then(Token::Dot, Self::parse_ident)?))
  }

  pub(crate) fn parse_target(&mut self) -> Result<Target, Diag> {
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

  pub(crate) fn parse_stmt_break(&mut self) -> Result<StmtKind, Diag> {
    let target = self.parse_target()?;
    let expr = self.maybe_parse_expr_bp(BP::Min)?;
    self.eat(Token::Semi)?;
    Ok(StmtKind::Break(target, expr))
  }

  pub(crate) fn parse_stmt_continue(&mut self) -> Result<StmtKind, Diag> {
    let target = self.parse_target()?;
    self.eat(Token::Semi)?;
    Ok(StmtKind::Continue(target))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_label(&self, label: Label) -> Content {
    Content::even(label.0.map(|label| (Punct(":"), label)))
  }

  pub(crate) fn fmt_target(&self, target: Target) -> Content {
    match target {
      Target::AnyLoop => Content::even(()),
      Target::Label(label) => Content::even((Punct(":"), label)),
      Target::Do => Content::even((Punct(":"), Keyword("do"))),
      Target::Loop => Content::even((Punct(":"), Keyword("loop"))),
      Target::While => Content::even((Punct(":"), Keyword("while"))),
      Target::For => Content::even((Punct(":"), Keyword("for"))),
      Target::When => Content::even((Punct(":"), Keyword("when"))),
    }
  }

  pub(crate) fn fmt_stmt_break(&self, target: Target, expr: &Option<Expr>) -> Content {
    Content::even((
      Keyword("break"),
      self.fmt_target(target),
      expr.as_ref().map(|expr| (Space, Indent::lazy(self.fmt_expr(expr)))),
      Punct(";"),
    ))
  }

  pub(crate) fn fmt_stmt_continue(&self, target: Target) -> Content {
    Content::even((Keyword("continue"), self.fmt_target(target), Punct(";")))
  }
}

impl Resolver<'_> {
  pub(crate) fn bind_target<T>(
    &mut self,
    label: Label,
    targets: impl IntoIterator<IntoIter: Clone, Item = Target>,
    kind: TargetInfo,
    f: impl FnOnce(&mut Self) -> T,
  ) -> T {
    let targets = targets.into_iter().chain(label.0.map(Target::Label));
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
    target: Target,
    value: &Option<Expr>,
  ) -> Result<TirExpr, Diag> {
    let nil = self.types.nil();
    match self.targets.get(&target).and_then(|x| x.last()) {
      Some(&target) => {
        let value = match value {
          Some(value) => Some(self.resolve_expr_type(value, target.break_ty)),
          None => {
            if self.types.unify(target.break_ty, nil).is_failure() {
              self.diags.error(Diag::MissingBreakExpr {
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
    target: Target,
  ) -> Result<TirExpr, Diag> {
    match self.targets.get(&target).and_then(|x| x.last()) {
      Some(target) if target.continue_ => {
        Ok(TirExpr::new(span, self.types.new_var(span), TirExprKind::Continue(target.id)))
      }
      _ => Err(Diag::InvalidContinueTarget { span })?,
    }
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_break(
    &mut self,
    stage: &mut Stage,
    span: Span,
    label: TargetId,
    value: &Option<TirExpr>,
  ) {
    let value = match value {
      Some(v) => self.distill_expr_value(stage, v),
      None => Port { ty: self.types.nil(), kind: PortKind::Nil },
    };

    let label = self.targets[label].as_ref().unwrap();
    stage.local_barrier_write_to(label.break_value, span, value);

    stage.steps.push(Step::Diverge(label.layer, None));
  }

  pub(crate) fn distill_continue(&mut self, stage: &mut Stage, label: TargetId) {
    let label = self.targets[label].as_ref().unwrap();
    let transfer = Transfer::unconditional(label.continue_transfer.unwrap());
    stage.steps.push(Step::Diverge(label.layer, Some(transfer)));
  }
}
