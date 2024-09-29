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
  ast::{Block, Ident, Term, TermKind},
  compile::Compiler,
  desugar::Desugar,
  loader::Loader,
  parser::{ParseError, VineParser},
  resolve::{NodeId, Resolver},
  visit::VisitMut,
};

pub struct Repl<'ctx, 'ivm> {
  host: &'ivm mut Host<'ivm>,
  ivm: &'ctx mut IVM<'ivm>,
  interner: &'ctx StringInterner<'static>,
  loader: Loader<'ctx>,
  resolver: Resolver,
  repl_mod: NodeId,
  line: usize,
  vars: HashMap<Ident, Var<'ivm>>,
  locals: BTreeMap<usize, Ident>,
  local_count: usize,
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
  ) -> Self {
    let mut loader = Loader::new(interner);
    for lib in libs {
      loader.load_mod(lib);
    }

    let mut resolver = Resolver::default();
    resolver.build_graph(loader.finish());
    resolver.resolve_terms();

    let repl_mod = resolver.get_or_insert_child(0, Ident(interner.intern("repl"))).id;

    Desugar.visit(&mut resolver.nodes);

    let mut compiler = Compiler::new(&resolver.nodes);
    compiler.compile_all();
    host.insert_nets(&compiler.nets);

    let io = Ident(interner.intern("io"));
    let vars = HashMap::from([(io, Var { local: 0, value: Port::new_ext_val(ExtVal::IO) })]);
    let locals = BTreeMap::from([(0, io)]);

    Repl { host, ivm, interner, loader, resolver, repl_mod, line: 0, vars, locals, local_count: 1 }
  }

  pub fn exec<'src>(&mut self, line: &'src str) -> Result<Option<String>, ParseError<'src>> {
    let mut parser = VineParser { interner: self.interner, state: ParserState::new(line) };
    parser.bump()?;
    let mut stmts = Vec::new();
    while parser.state.token.is_some() {
      stmts.push(parser.parse_stmt()?);
    }

    self.loader.load_deps(".".as_ref(), &mut stmts);

    let new_nodes = self.resolver.nodes.len();
    self.resolver.build_mod(self.loader.finish(), 0);
    self.resolver.define_subitems(self.repl_mod, &mut stmts);
    let new_nodes = new_nodes..self.resolver.nodes.len();

    self.resolver._resolve_terms(new_nodes.clone());
    let binds =
      self.resolver.resolve_custom(self.repl_mod, &self.locals, &mut self.local_count, &mut stmts);

    for (ident, local) in binds {
      let var = self.vars.entry(ident).or_insert(Var { local: usize::MAX, value: Port::ERASE });
      if var.local != local {
        self.locals.remove(&var.local);
        self.locals.insert(local, ident);
        var.local = local;
        self.ivm.link(replace(&mut var.value, Port::ERASE), Port::ERASE);
      }
    }

    Desugar.visit(&mut stmts);
    Desugar.visit(&mut self.resolver.nodes[new_nodes.clone()]);

    let mut compiler = Compiler::new(&self.resolver.nodes);
    for node in &self.resolver.nodes[new_nodes] {
      compiler.compile_node(node);
    }

    let line = self.line;
    self.line += 1;
    let name = format!("::repl::{line}");

    compiler.compile_term(
      &mut self.local_count,
      name.clone(),
      &Term { kind: TermKind::Block(Block { stmts }) },
      self.vars.values().map(|v| v.local),
    );

    self.host.insert_nets(&compiler.nets);

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
        let "fn" = &**c else { break 'list };
        let mut cur = &**r;
        let mut children = vec![];
        for _ in 0..len {
          let Tree::Comb(c, l, r) = cur else { break 'list };
          let "tup" = &**c else { break 'list };
          children.push(l);
          cur = r;
        }
        if &**l != cur || !matches!(cur, Tree::Var(_)) {
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
