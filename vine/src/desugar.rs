use std::mem::take;

use crate::{
  ast::{Block, Span, Stmt, StmtKind, Term, TermKind},
  resolve::Node,
  visit::VisitMut,
};

pub struct Desugar;

impl VisitMut<'_> for Desugar {
  fn visit_node(&mut self, node: &mut Node) {
    self._visit_node(node)
  }

  fn visit_term(&mut self, term: &mut Term) {
    match &mut term.kind {
      TermKind::Method(receiver, path, args) => {
        let receiver = take(&mut **receiver);
        let path = take(path);
        let mut args = take(args);
        args.insert(0, Term { span: receiver.span, kind: TermKind::Ref(Box::new(receiver)) });
        term.kind =
          TermKind::Call(Box::new(Term { span: path.span, kind: TermKind::Path(path) }), args);
      }
      TermKind::Field(receiver, path) => {
        let o = take(&mut **receiver);
        let path = take(path);
        term.kind = TermKind::Deref(Box::new(Term {
          span: term.span,
          kind: TermKind::Call(
            Box::new(Term { span: path.span, kind: TermKind::Path(path) }),
            vec![Term { span: o.span, kind: TermKind::Ref(Box::new(o)) }],
          ),
        }))
      }
      TermKind::WhileLet(pat, value, body) => {
        let pat = take(&mut **pat);
        let value = take(&mut **value);
        let body = take(body);
        term.kind = TermKind::Loop(Block {
          span: term.span,
          stmts: vec![Stmt {
            span: term.span,
            kind: StmtKind::Term(
              Term {
                span: term.span,
                kind: TermKind::Match(
                  Box::new(value),
                  vec![
                    (pat, Term { span: body.span, kind: TermKind::Block(body) }),
                    (
                      Term { span: Span::NONE, kind: TermKind::Hole },
                      Term { span: Span::NONE, kind: TermKind::Break },
                    ),
                  ],
                ),
              },
              true,
            ),
          }],
        });
      }
      _ => {}
    }
    self._visit_term(term)
  }
}
