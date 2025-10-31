use std::{mem::take, path::PathBuf};

use ivm::{
  port::{Port, PortRef, Tag},
  wire::Wire,
  IVM,
};
use ivy::host::Host;
use vine_util::parser::{Parser, ParserState};

use crate::{
  compiler::{Compiler, Hooks},
  components::{
    charter::{Charter, ExtractItems},
    parser::VineParser,
    resolver::Resolver,
  },
  features::cfg::Config,
  structures::{
    ast::{visit::VisitMut, Block, Ident, Span, Stmt},
    chart::{DefId, GenericsId},
    core::Core,
    diag::Diag,
    resolutions::FragmentId,
    tir::Local,
    types::{Type, TypeKind, Types},
    vir::{Header, Interface, InterfaceKind, Layer, StageId, Step, Transfer, Vir},
  },
  tools::repl::command::{ReplCommand, ReplOption, ReplOptions, HELP},
};

mod command;
mod show_tree;

pub struct Repl<'ctx, 'ivm, 'ext> {
  host: &'ivm mut Host<'ivm>,
  ivm: &'ctx mut IVM<'ivm, 'ext>,
  core: &'static Core,
  compiler: Compiler,
  repl_mod: DefId,
  line: usize,
  scope: Vec<ScopeEntry<'ivm>>,
  types: Types,
  pub options: ReplOptions,
}

struct ScopeEntry<'ivm> {
  name: Ident,
  span: Span,
  ty: Type,
  value: Option<Port<'ivm>>,
  space: Option<Port<'ivm>>,
}

impl<'ctx, 'ivm, 'ext> Repl<'ctx, 'ivm, 'ext> {
  pub fn new(
    mut host: &'ivm mut Host<'ivm>,
    ivm: &'ctx mut IVM<'ivm, 'ext>,
    core: &'static Core,
    config: Config,
    libs: Vec<PathBuf>,
  ) -> Result<Self, Vec<Diag>> {
    let mut compiler = Compiler::new(core, config);
    for lib in libs {
      compiler.loader.load_mod(&lib);
    }

    let mut repl_mod = DefId::default();
    let nets = compiler.compile(InitHooks(&mut repl_mod))?;
    struct InitHooks<'a>(&'a mut DefId);
    impl Hooks for InitHooks<'_> {
      fn chart(&mut self, charter: &mut Charter) {
        *self.0 = charter.chart_child(DefId::ROOT, charter.core.ident("repl"), DefId::ROOT, true);
      }
    }
    host.insert_nets(&nets);

    let mut types = Types::default();
    let mut scope = Vec::new();
    if let Some(io_type) = compiler.chart.builtins.io {
      scope.push(ScopeEntry {
        name: core.ident("io"),
        span: Span::NONE,
        ty: types.new(TypeKind::Opaque(io_type, vec![])),
        value: Some(Port::new_ext_val(host.new_io())),
        space: None,
      });
    }

    let line = 0;
    let options = ReplOptions::default();

    Ok(Repl { host, ivm, core, compiler, repl_mod, line, scope, types, options })
  }

  pub fn exec(&mut self, input: &str) -> Result<(), Vec<Diag>> {
    let (span, command) = match self.parse_input(input) {
      Ok(command) => command,
      Err(diag) => {
        self.core.report(diag);
        self.core.bail()?;
        unreachable!()
      }
    };

    match command {
      ReplCommand::Help => {
        println!("{HELP}");
      }
      ReplCommand::Scope => {
        self.print_scope();
      }
      ReplCommand::Clear(vars) => self.run(span, vec![], vars)?,
      ReplCommand::Set(option) => match option {
        ReplOption::ShowScope(bool) => self.options.show_scope = bool,
      },
      ReplCommand::Run(stmts) => self.run(span, stmts, vec![])?,
    }

    Ok(())
  }

  pub fn run(&mut self, span: Span, stmts: Vec<Stmt>, clear: Vec<Ident>) -> Result<(), Vec<Diag>> {
    let mut block = Block { span, stmts };

    self.compiler.loader.load_deps(".".as_ref(), &mut block);

    let path = self.core.alloc_str(&format!("::repl::{}", self.line));
    let mut fragment = None;
    let mut ty = None;
    let mut bindings = Vec::new();

    let nets = self.compiler.compile(ExecHooks {
      path,
      repl_mod: self.repl_mod,
      scope: &mut self.scope,
      block: &mut block,
      types: &mut self.types,
      fragment: &mut fragment,
      ty: &mut ty,
      bindings: &mut bindings,
      clear,
    })?;

    let fragment = fragment.unwrap();
    let ty = ty.unwrap();

    self.line += 1;

    struct ExecHooks<'ivm, 'a> {
      path: &'static str,
      repl_mod: DefId,
      scope: &'a mut Vec<ScopeEntry<'ivm>>,
      block: &'a mut Block,
      types: &'a mut Types,
      fragment: &'a mut Option<FragmentId>,
      ty: &'a mut Option<Type>,
      bindings: &'a mut Vec<(Local, Ident, Span, Type)>,
      clear: Vec<Ident>,
    }

    impl Hooks for ExecHooks<'_, '_> {
      fn chart(&mut self, charter: &mut Charter<'_>) {
        let mut extractor = ExtractItems::default();
        extractor.visit(&mut *self.block);
        for item in extractor.items {
          charter.chart_item(self.repl_mod, item, self.repl_mod, GenericsId::NONE);
        }
      }

      fn resolve(&mut self, resolver: &mut Resolver<'_>) {
        let (fragment, ty, mut bindings) = resolver._resolve_repl(
          self.block.span,
          self.path,
          self.repl_mod,
          self.types.clone(),
          self.scope.iter().map(|entry| (entry.name.clone(), entry.span, entry.ty)),
          self.block,
        );
        *self.fragment = Some(fragment);
        *self.ty = Some(ty);
        if !self.clear.is_empty() {
          bindings.retain_mut(|b| !self.clear.contains(&b.1));
        }
        *self.bindings = bindings;
      }

      fn distill(&mut self, fragment_id: FragmentId, vir: &mut Vir) {
        let id = self.fragment.unwrap();
        if id == fragment_id {
          let layer = vir.layers.next_index();
          _ = vir.layers.push(Layer { id: layer, parent: None, stages: vec![] });
          let locals = self.bindings.iter().map(|b| b.0).collect();
          let interface = vir.interfaces.next_index();
          _ = vir.interfaces.push(Interface::new(interface, layer, InterfaceKind::Inspect(locals)));
          let stage = &mut vir.stages[StageId(0)];
          let wire = stage.new_wire(Span::NONE, self.types.nil());
          stage.steps.push(Step::Transfer(Transfer { interface, data: Some(wire.neg) }));
          let Header::Entry(mut ports) = take(&mut stage.header) else { unreachable!() };
          ports.push(wire.pos);

          for (i, entry) in self.scope.iter().enumerate().rev() {
            stage.declarations.push(Local(i));
            if entry.space.is_some() {
              ports.push(stage.local_read(Local(i), entry.span, entry.ty));
            }
            stage.local_barrier(Local(i));
            if entry.value.is_some() {
              ports.push(stage.local_write(Local(i), entry.span, entry.ty));
            }
          }

          ports.reverse();
          stage.header = Header::Entry(ports);
        }
      }
    }

    self.host.insert_nets(&nets);

    let w = self.ivm.new_wire();
    let root = w.0;

    fn make_node<'ivm>(
      ivm: &mut IVM<'ivm, '_>,
      label: u16,
      wire: Wire<'ivm>,
    ) -> (Wire<'ivm>, Wire<'ivm>) {
      let node = unsafe { ivm.new_node(Tag::Comb, label) };
      ivm.link_wire(wire, node.0);
      (node.1, node.2)
    }

    let label_x = Host::label_to_u16("x", &mut self.host.comb_labels);
    let label_ref = Host::label_to_u16("ref", &mut self.host.comb_labels);

    let mut cur = w.1;

    if self.core.debug {
      let label_dbg = Host::label_to_u16("dbg", &mut self.host.comb_labels);
      let label_tup = Host::label_to_u16("tup", &mut self.host.comb_labels);

      let dbg;
      (dbg, cur) = make_node(self.ivm, label_dbg, cur);
      let (dbg_in, dbg_out) = make_node(self.ivm, label_ref, dbg);
      self.ivm.link_wire(dbg_out, Port::ERASE);
      let (io, dbg_in) = make_node(self.ivm, label_tup, dbg_in);
      self.ivm.link_wire(io, Port::new_ext_val(self.host.new_io()));
      let (len, dbg_in) = make_node(self.ivm, label_tup, dbg_in);
      self.ivm.link_wire(len, Port::new_ext_val(self.host.new_n32(0)));
      let (buf, end) = make_node(self.ivm, label_tup, dbg_in);
      self.ivm.link_wire_wire(buf, end);
    }

    let mut wire;
    for entry in take(&mut self.scope) {
      if let Some(port) = entry.value {
        (wire, cur) = make_node(self.ivm, label_x, cur);
        self.ivm.link_wire(wire, port);
      }
      if let Some(port) = entry.space {
        (wire, cur) = make_node(self.ivm, label_x, cur);
        self.ivm.link_wire(wire, port);
      }
    }

    let (binds, cur) = make_node(self.ivm, label_x, cur);
    let (result, destroy) = make_node(self.ivm, label_ref, cur);

    let mut binds = Some(binds);
    let vir = &self.compiler.vir[fragment];
    self.types = vir.types.clone();
    let wires = &vir.interfaces.last().unwrap().wires;
    self.scope = Vec::from_iter(bindings.into_iter().map(|(local, name, span, ty)| {
      let (value, _, space) = wires.get(&local).copied().unwrap_or_default();
      let value = value.then(|| {
        let (a, b) = make_node(self.ivm, label_x, binds.take().unwrap());
        binds = Some(b);
        Port::new_wire(a)
      });
      let space = space.then(|| {
        let (a, b) = make_node(self.ivm, label_x, binds.take().unwrap());
        binds = Some(b);
        Port::new_wire(a)
      });
      ScopeEntry { name, span, ty, value, space }
    }));
    self.ivm.link_wire(binds.unwrap(), Port::ERASE);

    self.ivm.execute(&self.host.get(path).unwrap().instructions, Port::new_wire(root));
    self.ivm.normalize();

    let tree = self.host.read(self.ivm, &PortRef::new_wire(&result));
    let output = self.show_tree(ty, &tree);
    self.ivm.link_wire_wire(result, destroy);
    self.ivm.normalize();

    if output != "()" {
      println!("{output}");
    }

    Ok(())
  }

  fn parse_input(&mut self, line: &str) -> Result<(Span, ReplCommand), Diag> {
    let file = self.compiler.loader.add_file(None, "input".into(), line);
    let mut parser = VineParser { core: self.core, state: ParserState::new(line), file };
    parser.bump()?;
    let span = parser.start_span();
    let command = parser.parse_repl_command()?;
    let span = parser.end_span(span);
    Ok((span, command))
  }

  pub fn print_scope(&mut self) {
    let mut reader = self.host.reader(self.ivm);
    let trees = Vec::from_iter(self.scope.iter().map(|entry| {
      (
        entry.value.as_ref().map(|p| reader.read_port(p)),
        entry.space.as_ref().map(|p| reader.read_port(p)),
      )
    }));
    for (i, (value, space)) in (0..self.scope.len()).zip(trees) {
      let entry = &self.scope[i];
      let ident = entry.name.0.clone();
      let ty = entry.ty;
      let value = value.map(|tree| self.show_tree(ty, &tree));
      let space = space.map(|tree| self.show_tree(ty.inverse(), &tree));
      let ty = self.types.show(&self.compiler.chart, ty);
      match (value, space) {
        (None, None) => println!("let {ident}: {ty};"),
        (Some(value), None) => println!("let {ident}: {ty} = {value};"),
        (None, Some(space)) => println!("let ~{ident}: ~{ty} = {space};"),
        (Some(value), Some(space)) => println!("let &{ident}: &{ty} = &({value}; ~{space});"),
      }
    }
  }
}
