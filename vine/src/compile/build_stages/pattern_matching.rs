use std::{collections::HashMap, ops::Range};

use crate::{
  compile::{Local, StageId},
  resolve::NodeId,
};

use super::{Compiler, Expr, Node, Pat, PatKind, Result, Step};

impl Compiler<'_> {
  pub(super) fn lower_match(&mut self, value: &Expr, arms: &[(Pat, Expr)]) -> Result {
    let value = self.lower_expr_value(value);
    let initial = self.new_match_var(false);
    self.set_local_to(initial.local, value);
    let mut arm_counts = vec![0; arms.len()];
    let rows = arms
      .iter()
      .enumerate()
      .map(|(i, arm)| Row {
        cells: vec![Cell { var: initial, pattern: &arm.0 }],
        body: Body { aliases: vec![], arm: i },
      })
      .collect();
    let tree = self.build_decision_tree(rows, &mut arm_counts);

    let result = self.new_local();

    self.new_fork(|self_| {
      let arms = arms
        .iter()
        .zip(arm_counts)
        .map(|((_, body), count)| {
          if count <= 1 {
            Arm::Unique(body)
          } else {
            let i = self_.new_interface();
            Arm::Shared(self_.new_stage(i, |self_, _| {
              let r = self_.lower_expr_value(body);
              self_.set_local_to(result, r);
              true
            }))
          }
        })
        .collect::<Vec<_>>();

      self_.lower_decision_tree(&arms, result, tree);
    });

    let r = self.net.new_wire();
    self.cur.steps.push_back(Step::Move(result, r.0));

    Result::Value(r.1)
  }

  fn lower_decision_tree(&mut self, arms: &[Arm], result: Local, tree: DecisionTree) -> bool {
    match tree {
      DecisionTree::Leaf(body) => {
        for (a, b) in body.aliases {
          let x = self.net.new_wire();
          self.cur.steps.push_back(Step::Move(b.local, x.0));
          self.set_local_to(a, x.1);
          if b.is_place {
            let y = self.fin_move_local(a);
            self.set_local_to(b.local, y);
          }
        }
        match arms[body.arm] {
          Arm::Shared(stage) => {
            self.goto(stage);
            false
          }
          Arm::Unique(expr) => {
            let r = self.lower_expr_value(expr);
            self.set_local_to(result, r);
            true
          }
        }
      }
      DecisionTree::Missing => false,
      DecisionTree::Tuple(v, t, next) => {
        let a = self.tuple(t.clone(), Self::set_local);
        self.cur.steps.push_back(Step::Move(v.local, a));
        if v.is_place {
          let a = self.tuple(t, Self::fin_move_local);
          self.set_local_to(v.local, a);
        }
        self.lower_decision_tree(arms, result, *next)
      }
      DecisionTree::Deref(r, v, next) => {
        let v0 = self.net.new_wire();
        let v2 = self.net.new_wire();
        let r0 = self.new_comb("ref", v0.0, v2.0);
        self.cur.steps.push_back(Step::Move(r.local, r0));
        self.set_local_to(v.local, v0.1);
        if r.is_place {
          let v1 = self.fin_move_local(v.local);
          let r1 = self.new_comb("ref", v1, v2.1);
          self.set_local_to(r.local, r1);
        } else {
          self.cur.fin.push(Step::Move(v.local, v2.1));
        }
        self.lower_decision_tree(arms, result, *next)
      }
      DecisionTree::Switch(var, adt, cases, fallback) => {
        let adt = self.nodes[adt].adt.as_ref().unwrap();

        let fallback_needs_var = fallback.as_ref().is_some_and(|x| x.0);
        let mut fallback = fallback.map(|x| x.1);
        let fallback_stage =
          if fallback.is_some() && cases.iter().filter(|x| x.body.is_none()).count() > 1 {
            fallback.take().map(|tree| {
              let i = self.new_interface();
              self.new_stage(i, |self_, _| self_.lower_decision_tree(arms, result, *tree))
            })
          } else {
            None
          };

        let i = self.new_interface();
        let val = self.net.new_wire();
        self.cur.steps.push_back(Step::Move(var.local, val.0));
        let stage =
          self.apply_combs("enum", val.1, cases.into_iter().enumerate(), |self_, (v, case)| {
            let s = self_.new_stage(i, |self_, _| {
              let root = self_.net.new_wire();
              let inner = self_.apply_combs("enum", root.0, case.vars.clone(), Self::set_local);
              let should_fallback = case.body.is_none();
              let mut end = false;
              if let Some(tree) = case.body {
                end = self_.lower_decision_tree(arms, result, tree);
              }
              if var.is_place || should_fallback && fallback_needs_var {
                let r = self_.make_enum(adt, v, case.vars.clone(), Self::fin_move_local);
                self_.set_local_to(var.local, r);
              }
              self_.cur.header = Some((root.1, inner));
              if should_fallback {
                if let Some(stage) = fallback_stage {
                  self_.goto(stage);
                } else if let Some(tree) = fallback.take() {
                  end = self_.lower_decision_tree(arms, result, *tree);
                }
              }
              end
            });
            self_.stage_port(s)
          });

        self.cur.steps.push_back(Step::Call(i, stage));

        false
      }
    }
  }

  fn new_match_var(&mut self, is_place: bool) -> MatchVar {
    MatchVar { local: self.new_local(), is_place }
  }
}

#[derive(Debug)]
enum Arm<'t> {
  Shared(StageId),
  Unique(&'t Expr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct MatchVar {
  local: Local,
  is_place: bool,
}

#[derive(Debug)]
enum DecisionTree {
  Leaf(Body),
  Missing,
  Switch(MatchVar, NodeId, Vec<Case>, Option<(bool, Box<DecisionTree>)>),
  Tuple(MatchVar, Range<Local>, Box<DecisionTree>),
  Deref(MatchVar, MatchVar, Box<DecisionTree>),
}

#[derive(Debug)]
struct Case {
  vars: Range<Local>,
  body: Option<DecisionTree>,
}

#[derive(Debug, Clone)]
struct Row<'t> {
  cells: Vec<Cell<'t>>,
  body: Body,
}

#[derive(Debug, Clone)]
struct Cell<'t> {
  var: MatchVar,
  pattern: &'t Pat,
}

#[derive(Debug, Clone)]
struct Body {
  aliases: Vec<(Local, MatchVar)>,
  arm: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PatternType {
  Tuple(usize),
  Adt(NodeId),
  Ref,
  Move,
}

impl Compiler<'_> {
  fn build_decision_tree(&mut self, mut rows: Vec<Row>, arm_counts: &mut [usize]) -> DecisionTree {
    if rows.is_empty() {
      return DecisionTree::Missing;
    }

    for row in &mut rows {
      row.eliminate_wildcard_cells()
    }

    if rows.first().is_some_and(|r| r.cells.is_empty()) {
      let body = rows.swap_remove(0).body;
      arm_counts[body.arm] += 1;
      return DecisionTree::Leaf(body);
    }

    let (var, ty) = self.next_var(&rows);

    match ty {
      PatternType::Tuple(len) => {
        for row in &mut rows {
          if let Some(expr) = row.remove_column(var) {
            let PatKind::Tuple(children) = &expr.kind else { unreachable!() };
            row.cells.extend(children.iter().enumerate().map(|(i, p)| Cell {
              var: MatchVar { local: self.local_count + i, is_place: var.is_place },
              pattern: p,
            }))
          }
        }
        let range = self.local_count..(self.local_count + len);
        self.local_count += len;

        DecisionTree::Tuple(var, range, Box::new(self.build_decision_tree(rows, arm_counts)))
      }
      PatternType::Ref => {
        let new_var = self.new_match_var(true);
        unwrap_pattern_one(&mut rows, var, new_var, |t| match &t.kind {
          PatKind::Ref(x) => x,
          _ => unreachable!(),
        });

        DecisionTree::Deref(var, new_var, Box::new(self.build_decision_tree(rows, arm_counts)))
      }
      PatternType::Move => {
        let new_var = MatchVar { local: var.local, is_place: false };
        unwrap_pattern_one(&mut rows, var, new_var, |t| match &t.kind {
          PatKind::Move(x) => x,
          _ => unreachable!(),
        });

        self.build_decision_tree(rows, arm_counts)
      }
      PatternType::Adt(adt_id) => {
        let adt = self.nodes[adt_id].adt.as_ref().unwrap();
        let mut variants = vec![vec![]; adt.variants.len()];
        let mut wildcard = vec![];

        for mut row in rows.into_iter() {
          let bucket = if let Some(pat) = row.remove_column(var) {
            let PatKind::Adt(v, children) = &pat.kind else { unreachable!() };
            let v = v.resolved.unwrap();
            let children = children.as_deref().unwrap_or(&[]);
            let v = self.nodes[v].variant.as_ref().unwrap();
            assert_eq!(v.fields.len(), children.len());
            row.cells.extend(children.iter().enumerate().map(|(i, p)| Cell {
              var: MatchVar { local: self.local_count + i, is_place: var.is_place },
              pattern: p,
            }));
            &mut variants[v.variant]
          } else {
            &mut wildcard
          };
          bucket.push(row);
        }

        let max_local_count = adt
          .variants
          .iter()
          .map(|x| self.nodes[*x].variant.as_ref().unwrap().fields.len())
          .max()
          .unwrap_or(0);

        let base = self.local_count;
        self.local_count += max_local_count;

        let cases = variants
          .into_iter()
          .enumerate()
          .map(|(v, mut rows)| {
            let field_count = self.nodes[adt.variants[v]].variant.as_ref().unwrap().fields.len();
            let vars = base..(base + field_count);
            Case {
              vars,
              body: (!rows.is_empty()).then(|| {
                rows.extend_from_slice(&wildcard);
                self.build_decision_tree(rows, arm_counts)
              }),
            }
          })
          .collect::<Vec<_>>();

        if cases.len() == 1 {
          return if let Case { vars, body: Some(body) } = { cases }.pop().unwrap() {
            DecisionTree::Tuple(var, vars, Box::new(body))
          } else {
            self.local_count -= max_local_count;
            self.build_decision_tree(wildcard, arm_counts)
          };
        }

        let fallback =
          (!wildcard.is_empty() && cases.iter().any(|x| x.body.is_none())).then(|| {
            (
              var.is_place
                || wildcard.iter().any(|x| x.body.aliases.iter().any(|x| x.1.local == var.local)),
              Box::new(self.build_decision_tree(wildcard, arm_counts)),
            )
          });

        DecisionTree::Switch(var, adt_id, cases, fallback)
      }
    }
  }

  fn next_var(&self, rows: &Vec<Row>) -> (MatchVar, PatternType) {
    let mut refs = HashMap::<MatchVar, (usize, Option<PatternType>)>::new();

    for row in rows {
      for cell in &row.cells {
        let ty = cell.pat_type(self.nodes);
        let entry = refs.entry(cell.var).or_insert((0, Some(ty)));
        entry.0 += 1;
        if entry.1 != Some(ty) {
          entry.1 = None;
        }
      }
    }

    refs
      .into_iter()
      .filter_map(|(i, (r, ty))| Some(((i, ty?), r)))
      .max_by_key(|&((i, _), r)| (r, i.local))
      .unwrap()
      .0
  }
}

fn unwrap_pattern_one(
  rows: &mut Vec<Row<'_>>,
  var: MatchVar,
  new_var: MatchVar,
  f: impl Fn(&Pat) -> &Pat,
) {
  for row in rows {
    if let Some(cell) = row.get_column_mut(var) {
      cell.var = new_var;
      cell.pattern = f(cell.pattern);
    }
  }
}

impl<'t> Row<'t> {
  fn eliminate_wildcard_cells(&mut self) {
    self.cells.retain(|cell| match cell.pattern.kind {
      PatKind::Local(l) => {
        self.body.aliases.push((l, cell.var));
        false
      }
      PatKind::Hole => false,
      _ => true,
    });
  }

  fn remove_column(&mut self, var: MatchVar) -> Option<&'t Pat> {
    let i = self.get_column_idx(var)?;
    Some(self.cells.remove(i).pattern)
  }

  fn get_column_mut(&mut self, var: MatchVar) -> Option<&mut Cell<'t>> {
    let i = self.get_column_idx(var)?;
    Some(&mut self.cells[i])
  }

  fn get_column_idx(&mut self, var: MatchVar) -> Option<usize> {
    Some(self.cells.iter().enumerate().find(|(_, c)| c.var == var)?.0)
  }
}

impl Cell<'_> {
  fn pat_type(&self, nodes: &[Node]) -> PatternType {
    match &self.pattern.kind {
      PatKind::Adt(p, _) => {
        PatternType::Adt(nodes[p.resolved.unwrap()].variant.as_ref().unwrap().adt)
      }
      PatKind::Tuple(e) => PatternType::Tuple(e.len()),
      PatKind::Ref(_) => PatternType::Ref,
      PatKind::Move(_) => PatternType::Move,
      _ => unreachable!(),
    }
  }
}
