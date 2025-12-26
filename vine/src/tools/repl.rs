use std::{mem::take, path::PathBuf};

use ivm::{
  IVM,
  port::{Port, Tag},
  wire::Wire,
};
use ivy::host::Host;

use crate::{
  compiler::{Compiler, Hooks},
  components::{
    charter::{Charter, ExtractItems},
    lexer::Lexer,
    parser::Parser,
    resolver::Resolver,
  },
  structures::{
    ast::{Block, Ident, Span, Stmt, visit::VisitMut},
    chart::{DefId, GenericsId, VisId},
    diag::{Diag, ErrorGuaranteed},
    resolutions::FragmentId,
    tir::Local,
    types::{Type, TypeKind, Types},
    vir::{Header, Interface, InterfaceId, InterfaceKind, Layer, StageId, Step, Transfer, Vir},
  },
  tools::repl::command::{HELP, ReplCommand, ReplOption, ReplOptions},
};

mod command;
mod show;

pub struct Repl<'ctx, 'ivm, 'ext, 'comp> {
  host: &'ivm mut Host<'ivm>,
  ivm: &'ctx mut IVM<'ivm, 'ext>,
  pub compiler: &'comp mut Compiler,
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

impl<'ctx, 'ivm, 'ext, 'comp> Repl<'ctx, 'ivm, 'ext, 'comp> {
  pub fn new(
    mut host: &'ivm mut Host<'ivm>,
    ivm: &'ctx mut IVM<'ivm, 'ext>,
    compiler: &'comp mut Compiler,
    libs: Vec<PathBuf>,
  ) -> Result<Self, ErrorGuaranteed> {
    for lib in libs {
      compiler.loader.load_mod(&lib, &mut compiler.diags);
    }

    let mut repl_mod = DefId::default();
    let nets = compiler.compile(InitHooks(&mut repl_mod))?;
    struct InitHooks<'a>(&'a mut DefId);
    impl Hooks for InitHooks<'_> {
      fn chart(&mut self, charter: &mut Charter) {
        *self.0 = charter.new_def(Ident("repl".into()), "<repl>".into(), None);
      }
    }
    host.insert_nets(&nets);

    let mut types = Types::default();
    let mut scope = Vec::new();
    if let Some(io_type) = compiler.chart.builtins.io {
      scope.push(ScopeEntry {
        name: Ident("io".into()),
        span: Span::NONE,
        ty: types.new(TypeKind::Opaque(io_type, vec![])),
        value: Some(Port::new_ext_val(host.new_io())),
        space: None,
      });
    }

    let line = 0;
    let options = ReplOptions::default();

    Ok(Repl { host, ivm, compiler, repl_mod, line, scope, types, options })
  }

  pub fn exec(&mut self, input: &str) -> Result<(), ErrorGuaranteed> {
    let (span, command) = match self.parse_input(input) {
      Ok(command) => command,
      Err(diag) => {
        self.compiler.diags.error(diag);
        self.compiler.diags.bail()?;
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

  pub fn run(
    &mut self,
    span: Span,
    stmts: Vec<Stmt>,
    clear: Vec<Ident>,
  ) -> Result<(), ErrorGuaranteed> {
    let mut block = Block { span, stmts };

    self.compiler.loader.load_deps(".".as_ref(), &mut block, &mut self.compiler.diags);

    let path = format!(":repl::{}", self.line);
    let mut fragment = None;
    let mut ty = None;
    let mut bindings = Vec::new();
    let mut interface = None;
    let mut repl_mod = self.repl_mod;

    let hooks = ExecHooks {
      path: path.clone(),
      repl_mod: &mut repl_mod,
      scope: &mut self.scope,
      block: &mut block,
      types: &mut self.types,
      fragment: &mut fragment,
      ty: &mut ty,
      bindings: &mut bindings,
      interface: &mut interface,
      clear,
      extracted_items: false,
    };
    let nets = self.compiler.compile(hooks)?;

    let fragment = fragment.unwrap();
    let ty = ty.unwrap();
    self.repl_mod = repl_mod;

    self.line += 1;

    struct ExecHooks<'ivm, 'a> {
      path: String,
      repl_mod: &'a mut DefId,
      scope: &'a mut Vec<ScopeEntry<'ivm>>,
      block: &'a mut Block,
      types: &'a mut Types,
      fragment: &'a mut Option<FragmentId>,
      ty: &'a mut Option<Type>,
      bindings: &'a mut Vec<(Local, Ident, Span, Type)>,
      interface: &'a mut Option<InterfaceId>,
      clear: Vec<Ident>,
      extracted_items: bool,
    }

    impl Hooks for ExecHooks<'_, '_> {
      fn chart(&mut self, charter: &mut Charter<'_>) {
        let mut extractor = ExtractItems::default();
        extractor.visit(&mut *self.block);
        if !extractor.items.is_empty() {
          for item in extractor.items {
            *self.repl_mod =
              charter.new_def(Ident("repl".into()), "<repl>".into(), Some(*self.repl_mod));
            charter.chart_item(VisId::Def(*self.repl_mod), item, *self.repl_mod, GenericsId::NONE);
            self.extracted_items = true;
          }
        }
      }

      fn resolve(&mut self, resolver: &mut Resolver<'_>) {
        let (fragment, ty, mut bindings) = resolver._resolve_repl(
          self.block.span,
          self.path.clone(),
          *self.repl_mod,
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
          *self.interface = Some(interface);
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

    if self.compiler.debug {
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
    let wires = &vir.interfaces[interface.unwrap()].wires;
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

    self.ivm.execute(&self.host.get(&path).unwrap().instructions, Port::new_wire(root));
    self.ivm.normalize();

    let mut result = Port::new_wire(result);
    let output = self.show(ty, &mut result);
    self.ivm.link_wire(destroy, result);
    self.ivm.normalize();

    if output != "()" {
      println!("{output}");
    }

    Ok(())
  }

  fn parse_input(&mut self, line: &str) -> Result<(Span, ReplCommand), Diag> {
    let file = self.compiler.loader.add_file(None, "input".into(), line.into());
    let mut parser = Parser::new(Lexer::new(file, line))?;
    let span = parser.start_span();
    let command = parser.parse_repl_command()?;
    let span = parser.end_span(span);
    Ok((span, command))
  }

  pub fn print_scope(&mut self) {
    for i in 0..self.scope.len() {
      let entry = &mut self.scope[i];
      let ident = entry.name.0.clone();
      let ty = entry.ty;
      let mut value = entry.value.take();
      let mut space = entry.space.take();
      let value_str = value.as_mut().map(|port| self.show(ty, port));
      let space_str = space.as_mut().map(|port| self.show(ty.inverse(), port));
      let entry = &mut self.scope[i];
      entry.value = value;
      entry.space = space;
      let ty = self.types.show(&self.compiler.chart, ty);
      match (value_str, space_str) {
        (None, None) => println!("let {ident}: {ty};"),
        (Some(value), None) => println!("let {ident}: {ty} = {value};"),
        (None, Some(space)) => println!("let ~{ident}: ~{ty} = {space};"),
        (Some(value), Some(space)) => println!("let &{ident}: &{ty} = &({value}; ~{space});"),
      }
    }
  }
}
