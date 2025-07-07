use vine_util::parser::Parser;

use crate::{
  components::{
    distiller::Distiller,
    parser::{VineParser, BRACE},
    resolver::Resolver,
  },
  structures::{
    ast::{Block, Span, Stmt, StmtKind},
    diag::Diag,
    tir::{TirExpr, TirExprKind},
    types::Type,
    vir::{Port, Stage},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_block(&mut self) -> Result<Block<'core>, Diag<'core>> {
    let span = self.start_span();
    let stmts = self.parse_delimited(BRACE, Self::parse_stmt)?;
    let span = self.end_span(span);
    Ok(Block { span, stmts })
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_block(&self, block: &Block<'core>, force_open: bool) -> Doc<'src> {
    if !force_open {
      if let [stmt] = &*block.stmts {
        if matches!(stmt.kind, StmtKind::Expr(_, false)) {
          let str = " ";
          return Doc::concat([
            Doc("{"),
            Doc::group([Doc::if_single(str), self.fmt_stmt(stmt), Doc::if_single(" ")]),
            Doc("}"),
          ]);
        }
      }
    }
    self.fmt_block_like(block.span, block.stmts.iter().map(|x| (x.span, self.fmt_stmt(x))))
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_block_nil(&mut self, block: &Block<'core>) -> TirExpr {
    let nil = self.types.nil();
    self.resolve_block_type(block, nil)
  }

  pub(crate) fn resolve_block_type(&mut self, block: &Block<'core>, ty: Type) -> TirExpr {
    self.enter_scope();
    let block = self.resolve_stmts_type(block.span, &block.stmts, ty);
    self.exit_scope();
    block
  }

  pub(crate) fn resolve_stmts_type(
    &mut self,
    span: Span,
    stmts: &[Stmt<'core>],
    ty: Type,
  ) -> TirExpr {
    let [stmt, rest @ ..] = stmts else {
      let nil = self.types.nil();
      let kind = if self.types.unify(ty, nil).is_failure() {
        TirExprKind::Error(
          self.core.report(Diag::MissingBlockExpr { span, ty: self.types.show(self.chart, ty) }),
        )
      } else {
        TirExprKind::Composite(Vec::new())
      };
      return TirExpr { span, ty, kind: Box::new(kind) };
    };
    let kind = match &stmt.kind {
      StmtKind::Let(stmt) => self.resolve_stmts_let(span, ty, stmt, rest),
      StmtKind::LetFn(_) => {
        let rest = self.resolve_stmts_let_fn_group(stmts);
        return self.resolve_stmts_type(span, rest, ty);
      }
      StmtKind::Expr(expr, semi) => {
        if !semi && rest.is_empty() {
          return self.resolve_expr_type(expr, ty);
        } else {
          let expr = self.resolve_expr(expr);
          TirExprKind::Seq(expr, self.resolve_stmts_type(span, rest, ty))
        }
      }
      StmtKind::Return(_) | StmtKind::Break(..) | StmtKind::Continue(..) => {
        let stmt = match &stmt.kind {
          StmtKind::Break(label, value) => self.resolve_expr_break(stmt.span, *label, value),
          StmtKind::Return(value) => self.resolve_expr_return(stmt.span, value),
          StmtKind::Continue(label) => self.resolve_expr_continue(stmt.span, *label),
          _ => unreachable!(),
        };
        let mut stmt = stmt.unwrap_or_else(|diag| self.error_expr(span, diag));
        if rest.is_empty() {
          _ = self.types.unify(stmt.ty, ty);
          return stmt;
        } else {
          let nil = self.types.nil();
          _ = self.types.unify(stmt.ty, nil);
          stmt.ty = nil;
          TirExprKind::Seq(stmt, self.resolve_stmts_type(span, rest, ty))
        }
      }
      StmtKind::Item(_) | StmtKind::Empty => return self.resolve_stmts_type(span, rest, ty),
    };
    TirExpr { span, ty, kind: Box::new(kind) }
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_seq(
    &mut self,
    stage: &mut Stage,
    ignored: &TirExpr,
    continuation: &TirExpr,
  ) -> Port {
    self.distill_expr_nil(stage, ignored);
    self.distill_expr_value(stage, continuation)
  }
}
