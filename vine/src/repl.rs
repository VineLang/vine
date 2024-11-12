use std::{
  collections::{BTreeMap, HashMap},
  fmt::{self, Display},
  mem::replace,
  path::PathBuf,
};

use ivm::{
  ext::ExtVal,
  port::{Port, PortRef, Tag},
  IVM,
};
use ivy::{ast::Tree, host::Host};
use vine_util::{
  idx::{Counter, IdxVec, IntMap, RangeExt},
  parser::{Parser, ParserState},
};

use crate::{
  ast::{Block, Expr, ExprKind, Ident, Local, Span, Stmt},
  checker::{self, Checker, CheckerState, Type},
  core::Core,
  desugar::Desugar,
  diag::Diag,
  emitter::Emitter,
  loader::Loader,
  parser::VineParser,
  resolver::{DefId, Resolver},
  visit::VisitMut,
};

pub struct Repl<'core, 'ctx, 'ivm> {
  host: &'ivm mut Host<'ivm>,
  ivm: &'ctx mut IVM<'ivm>,
  core: &'core Core<'core>,
  loader: Loader<'core>,
  resolver: Resolver<'core>,
  repl_mod: DefId,
  line: usize,
  vars: HashMap<Ident<'core>, Var<'ivm>>,
  locals: BTreeMap<Local, Ident<'core>>,
  local_count: Counter<Local>,
  checker_state: CheckerState,
}

struct Var<'ivm> {
  local: Local,
  value: Port<'ivm>,
}

impl<'core, 'ctx, 'ivm> Repl<'core, 'ctx, 'ivm> {
  pub fn new(
    mut host: &'ivm mut Host<'ivm>,
    ivm: &'ctx mut IVM<'ivm>,
    core: &'core Core<'core>,
    libs: Vec<PathBuf>,
  ) -> Result<Self, String> {
    let mut loader = Loader::new(core);
    for lib in libs {
      loader.load_mod(lib);
    }

    core.bail()?;

    let mut resolver = Resolver::new(core);
    resolver.build_graph(loader.finish());
    resolver.resolve_imports();
    resolver.resolve_defs();

    let repl_mod = resolver.get_or_insert_child(DefId::ROOT, core.ident("repl"), DefId::ROOT).id;

    let mut checker = Checker::new(core, &mut resolver);
    checker.check_defs();

    core.bail()?;

    Desugar.visit(resolver.defs.values_mut());

    let mut emitter = Emitter::new(&resolver);
    emitter.emit_all();
    host.insert_nets(&emitter.nets);

    let io = core.ident("io");
    let vars = HashMap::from([(io, Var { local: Local(0), value: Port::new_ext_val(ExtVal::IO) })]);
    let locals = BTreeMap::from([(Local(0), io)]);

    let checker_state = CheckerState {
      vars: IdxVec::from([Ok(Type::IO)]),
      locals: IntMap::from_iter([(Local(0), checker::Var(0))]),
      dyn_fns: IntMap::default(),
    };

    Ok(Repl {
      host,
      ivm,
      core,
      loader,
      resolver,
      repl_mod,
      line: 0,
      vars,
      locals,
      local_count: Counter(Local(1)),
      checker_state,
    })
  }

  pub fn exec(&mut self, line: &str) -> Result<Option<String>, String> {
    let mut stmts = match self.parse_line(line) {
      Ok(stmts) => stmts,
      Err(diag) => {
        self.core.report(diag);
        self.core.bail()?;
        unreachable!()
      }
    };

    self.loader.load_deps(".".as_ref(), &mut stmts);

    if let Err(e) = self.core.bail() {
      self.loader.finish();
      return Err(e);
    }

    let new_defs = self.resolver.defs.next_index();
    let new_uses = self.resolver.use_id.peek_next();
    self.resolver.build_mod(DefId::ROOT, self.loader.finish(), DefId::ROOT);
    self.resolver.extract_subitems(self.repl_mod, &mut stmts);
    let new_defs = new_defs..self.resolver.defs.next_index();

    self.resolver._resolve_defs(new_defs.clone());
    let binds =
      self.resolver.resolve_custom(self.repl_mod, &self.locals, &mut self.local_count, &mut stmts);

    self.resolver._resolve_imports(new_defs.iter().chain([self.repl_mod]));

    if let Err(e) = self.core.bail() {
      self.resolver.revert(new_defs.start, new_uses);
      return Err(e);
    }

    for (ident, local) in binds {
      let var =
        self.vars.entry(ident).or_insert(Var { local: Local(usize::MAX), value: Port::ERASE });
      if var.local != local {
        self.locals.remove(&var.local);
        self.locals.insert(local, ident);
        var.local = local;
        self.ivm.link(replace(&mut var.value, Port::ERASE), Port::ERASE);
      }
    }

    let mut block = Block { span: Span::NONE, stmts };

    let mut checker = Checker::new(self.core, &mut self.resolver);
    checker._check_defs(new_defs.clone());
    let state = checker._check_custom(self.checker_state.clone(), &mut block);
    self.core.bail()?;
    self.checker_state = state;

    Desugar.visit(&mut block);
    Desugar.visit(self.resolver.defs.slice_mut(new_defs.clone()));

    let mut emitter = Emitter::new(&self.resolver);
    for def in self.resolver.defs.slice(new_defs.clone()) {
      emitter.emit_def(def);
    }

    let line = self.line;
    self.line += 1;

    let name = format!("::repl::{line}");

    emitter.emit_root_expr(
      &mut self.local_count,
      name.clone(),
      &Expr { span: Span::NONE, kind: ExprKind::Block(block) },
      self.vars.values().map(|v| v.local),
    );

    self.host.insert_nets(&emitter.nets);

    let w = self.ivm.new_wire();
    let root = w.0;

    let mut cur = w.1;

    let label = self.host.label_to_u16("x");
    for var in self.locals.values() {
      let var = self.vars.get_mut(var).unwrap();
      let n = unsafe { self.ivm.new_node(Tag::Comb, label) };
      let m = unsafe { self.ivm.new_node(Tag::Comb, label) };
      self.ivm.link_wire(cur, n.0);
      self.ivm.link_wire(n.2, m.0);
      let value = replace(&mut var.value, Port::new_wire(m.1));
      self.ivm.link_wire(n.1, value);
      cur = m.2;
    }

    let out = cur;

    self.ivm.execute(&self.host.get(&name).unwrap().instructions, Port::new_wire(root));
    self.ivm.normalize();

    let tree = self.host.read(self.ivm, &PortRef::new_wire(&out));
    let output = (tree != Tree::Erase).then(|| show(&tree));
    self.ivm.link_wire(out, Port::ERASE);
    self.ivm.normalize();

    Ok(output)
  }

  fn parse_line(&mut self, line: &str) -> Result<Vec<Stmt<'core>>, Diag<'core>> {
    let file = self.loader.add_file("input".to_string(), line);
    let mut parser = VineParser { core: self.core, state: ParserState::new(line), file };
    parser.bump()?;
    let mut stmts = Vec::new();
    while parser.state.token.is_some() {
      stmts.push(parser.parse_stmt()?);
    }
    Ok(stmts)
  }
}

fn show(tree: &Tree) -> String {
  if let Tree::Comb(c, l, r) = tree {
    if c == "tup" {
      'list: {
        let Tree::U32(len) = **l else { break 'list };
        let Tree::Comb(c, l, r) = &**r else { break 'list };
        let "tup" = &**c else { break 'list };
        let mut cur = &**l;
        let mut children = vec![];
        for _ in 0..len {
          let Tree::Comb(c, l, r) = cur else { break 'list };
          let "tup" = &**c else { break 'list };
          children.push(l);
          cur = r;
        }
        if &**r != cur || !matches!(cur, Tree::Var(_)) {
          break 'list;
        }
        let is_str = children.iter().all(
          |x| matches!(***x, Tree::U32(n) if char::from_u32(n).is_some_and(|x| x == '\n' || !x.is_control())),
        );
        if is_str {
          let str = children
            .into_iter()
            .map(|x| {
              let Tree::U32(n) = **x else { unreachable!() };
              char::from_u32(n).unwrap()
            })
            .collect::<String>();
          return format!("{str:?}");
        } else {
          return format!(
            "[{}]",
            children.into_iter().map(|x| show(x)).collect::<Vec<_>>().join(", ")
          );
        }
      }
      let mut children = vec![&**l];
      let mut cur = &**r;
      while let Tree::Comb(c, l, r) = cur {
        if c != "tup" {
          break;
        }
        children.push(l);
        cur = r;
      }
      children.push(cur);
      return format!("({})", children.into_iter().map(show).collect::<Vec<_>>().join(", "));
    }
  }
  match tree {
    Tree::Erase => "()".into(),
    Tree::U32(n) => format!("{n}"),
    Tree::F32(n) => format!("{n:?}"),
    Tree::Var(v) if v == "#io" => "#io".into(),
    Tree::Global(p) => p.clone(),
    tree => format!("#ivy({tree})"),
  }
}

impl Display for Repl<'_, '_, '_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for ident in self.locals.values() {
      let var = &self.vars[ident];
      let value = self.host.read(self.ivm, &var.value);
      if value != Tree::Erase {
        writeln!(f, "{} = {}", ident.0 .0, show(&value))?;
      }
    }
    Ok(())
  }
}
