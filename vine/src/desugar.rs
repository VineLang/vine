use std::mem::take;

use crate::{
  ast::{Block, Stmt, StmtKind, Term, TermKind},
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
        args.insert(0, Term::new_ref(receiver));
        *term = Term { kind: TermKind::Call(Box::new(Term::new_path(path)), args) }
      }
      TermKind::Field(receiver, p) => {
        let o = take(&mut **receiver);
        let p = take(p);
        *term = Term::new_deref(Term {
          kind: TermKind::Call(Box::new(Term::new_path(p)), vec![Term::new_ref(o)]),
        })
      }
      TermKind::WhileLet(pat, value, body) => {
        let pat = take(&mut **pat);
        let value = take(&mut **value);
        let body = take(body);
        *term = Term {
          kind: TermKind::Loop(Block {
            stmts: vec![Stmt {
              kind: StmtKind::Term(
                Term {
                  kind: TermKind::Match(
                    Box::new(value),
                    vec![
                      (pat, Term { kind: TermKind::Block(body) }),
                      (Term { kind: TermKind::Hole }, Term { kind: TermKind::Break }),
                    ],
                  ),
                },
                true,
              ),
            }],
          }),
        }
      }
      _ => {}
    }
    self._visit_term(term)
  }
}
