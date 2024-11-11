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
  interner::StringInterner,
  parser::{Parser, ParserState},
};

use crate::{
  ast::{Block, Expr, ExprKind, Ident, Span},
  checker::{Checker, CheckerState, Type},
  desugar::Desugar,
  emitter::Emitter,
  loader::Loader,
  parser::VineParser,
  resolve::{DefId, Resolver},
  visit::VisitMut,
};

pub struct Repl<'ctx, 'ivm> {
  host: &'ivm mut Host<'ivm>,
  ivm: &'ctx mut IVM<'ivm>,
  interner: &'ctx StringInterner<'static>,
  loader: Loader<'ctx>,
  resolver: Resolver,
  repl_mod: DefId,
  line: usize,
  vars: HashMap<Ident, Var<'ivm>>,
  locals: BTreeMap<usize, Ident>,
  local_count: usize,
  checker_state: CheckerState,
}

struct Var<'ivm> {
  local: usize,
  value: Port<'ivm>,
}

impl<'ctx, 'ivm> Repl<'ctx, 'ivm> {
  pub fn new(
    mut host: &'ivm mut Host<'ivm>,
    ivm: &'ctx mut IVM<'ivm>,
    interner: &'ctx StringInterner<'static>,
    libs: Vec<PathBuf>,
  ) -> Result<Self, String> {
    let mut loader = Loader::new(interner);
    for lib in libs {
      loader.load_mod(lib);
    }

    loader.diags.report(&loader.files)?;

    let mut resolver = Resolver::default();
    resolver.build_graph(loader.finish());
    resolver.resolve_imports();
    resolver.resolve_defs();

    resolver.diags.report(&loader.files)?;

    let repl_mod = resolver.get_or_insert_child(0, Ident(interner.intern("repl")), 0).id;

    let mut checker = Checker::new(&mut resolver);
    checker.check_defs();
    checker.diags.report(&loader.files)?;

    Desugar.visit(&mut resolver.defs);

    let mut emitter = Emitter::new(&resolver);
    emitter.emit_all();
    host.insert_nets(&emitter.nets);

    let io = Ident(interner.intern("io"));
    let vars = HashMap::from([(io, Var { local: 0, value: Port::new_ext_val(ExtVal::IO) })]);
    let locals = BTreeMap::from([(0, io)]);

    let checker_state = CheckerState {
      vars: vec![Ok(Type::IO)],
      locals: HashMap::from([(0, 0)]),
      dyn_fns: HashMap::new(),
    };

    Ok(Repl {
      host,
      ivm,
      interner,
      loader,
      resolver,
      repl_mod,
      line: 0,
      vars,
      locals,
      local_count: 1,
      checker_state,
    })
  }

  pub fn exec(&mut self, line: &str) -> Result<Option<String>, String> {
    let file = self.loader.add_file("input".to_string(), line);
    let mut parser = VineParser { interner: self.interner, state: ParserState::new(line), file };
    parser.bump().map_err(|d| d.report(&self.loader.files))?;
    let mut stmts = Vec::new();
    while parser.state.token.is_some() {
      stmts.push(parser.parse_stmt().map_err(|d| d.report(&self.loader.files))?);
    }

    self.loader.load_deps(".".as_ref(), &mut stmts);

    if let Err(e) = self.loader.diags.report(&self.loader.files) {
      self.loader.finish();
      return Err(e);
    }

    let new_defs = self.resolver.defs.len();
    let new_uses = self.resolver.next_use_id;
    self.resolver.build_mod(0, self.loader.finish(), 0);
    self.resolver.extract_subitems(self.repl_mod, &mut stmts);
    let new_defs = new_defs..self.resolver.defs.len();

    self.resolver._resolve_defs(new_defs.clone());
    let binds =
      self.resolver.resolve_custom(self.repl_mod, &self.locals, &mut self.local_count, &mut stmts);

    self.resolver._resolve_imports(new_defs.clone().chain([self.repl_mod]));

    if let Err(e) = self.resolver.diags.report(&self.loader.files) {
      self.resolver.revert(new_defs.start, new_uses);
      return Err(e);
    }

    for (ident, local) in binds {
      let var = self.vars.entry(ident).or_insert(Var { local: usize::MAX, value: Port::ERASE });
      if var.local != local {
        self.locals.remove(&var.local);
        self.locals.insert(local, ident);
        var.local = local;
        self.ivm.link(replace(&mut var.value, Port::ERASE), Port::ERASE);
      }
    }

    let mut block = Block { span: Span::NONE, stmts };

    let mut checker = Checker::new(&mut self.resolver);
    checker._check_defs(new_defs.clone());
    let state = checker._check_custom(self.checker_state.clone(), &mut block);
    checker.diags.report(&self.loader.files)?;
    self.checker_state = state;

    Desugar.visit(&mut block);
    Desugar.visit(&mut self.resolver.defs[new_defs.clone()]);

    let mut emitter = Emitter::new(&self.resolver);
    for def in &self.resolver.defs[new_defs] {
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

impl Display for Repl<'_, '_> {
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
