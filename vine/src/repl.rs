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
  analyzer::{analyze, usage::Usage},
  ast::{Block, Builtin, Ident, Local, Span, Stmt},
  checker::{self, Checker, CheckerState, Type},
  core::Core,
  diag::Diag,
  distiller::Distiller,
  emitter::Emitter,
  loader::Loader,
  normalizer::normalize,
  parser::VineParser,
  resolver::{DefId, Resolver},
  specializer::specialize,
  vir::{InterfaceId, StageId},
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
  checker_state: CheckerState<'core>,
}

#[derive(Debug)]
struct Var<'ivm> {
  local: Local,
  value: Port<'ivm>,
  space: Port<'ivm>,
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

    let specializations = specialize(&mut resolver);
    let mut distiller = Distiller::new(&resolver);
    let mut emitter = Emitter::new(&resolver);
    for (def_id, def) in &resolver.defs {
      if let Some(vir) = distiller.distill(def) {
        let mut vir = normalize(&vir);
        analyze(&mut vir);
        emitter.emit_vir(def.canonical.to_string(), &vir, &specializations[def_id]);
      } else {
        emitter.emit_ivy(def);
      }
    }
    host.insert_nets(&emitter.nets);

    let io = core.ident("io");
    let vars = HashMap::from([(
      io,
      Var { local: Local(0), value: Port::new_ext_val(ExtVal::IO), space: Port::ERASE },
    )]);
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
      if self.vars.get(&ident).is_none_or(|v| v.local != local) {
        let wire = self.ivm.new_wire();
        let old = self.vars.insert(
          ident,
          Var { local, value: Port::new_wire(wire.0), space: Port::new_wire(wire.1) },
        );
        self.locals.insert(local, ident);
        if let Some(old) = old {
          self.locals.remove(&old.local);
          self.ivm.link(old.value, old.space);
        }
      }
    }

    let mut block = Block { span: Span::NONE, stmts };

    let mut checker = Checker::new(self.core, &mut self.resolver);
    checker._check_defs(new_defs.clone());
    let (mut ty, state) =
      checker._check_custom(self.repl_mod, self.checker_state.clone(), &mut block);
    self.core.bail()?;
    self.checker_state = state;

    let mut distiller = Distiller::new(&self.resolver);
    let mut emitter = Emitter::new(&self.resolver);
    for def in self.resolver.defs.slice(new_defs.clone()) {
      if let Some(vir) = distiller.distill(def) {
        let mut vir = normalize(&vir);
        analyze(&mut vir);
        // emitter.emit_vir(def.canonical.to_string(), &vir);
      } else {
        emitter.emit_ivy(def);
      }
    }

    let line = self.line;
    self.line += 1;

    let name = format!("::repl::{line}");

    let mut vir = distiller.distill_root(self.local_count, &block, Distiller::distill_block);
    vir.stages[StageId(0)].declarations.retain(|l| !self.locals.contains_key(l));
    vir.globals.extend(self.vars.values().map(|v| (v.local, Usage::Mut)));
    let mut vir = normalize(&vir);
    analyze(&mut vir);
    for var in self.vars.values() {
      vir.interfaces[InterfaceId(0)].wires.insert(var.local, (Usage::Mut, Usage::Mut));
    }
    // emitter.emit_vir(name.clone(), &vir);

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
      self.ivm.link_wire(n.1, m.0);
      let value = replace(&mut var.value, Port::new_wire(m.2));
      self.ivm.link_wire(m.1, value);
      cur = n.2;
    }

    let out = cur;

    self.ivm.execute(&self.host.get(&name).unwrap().instructions, Port::new_wire(root));
    self.ivm.normalize();

    let tree = self.host.read(self.ivm, &PortRef::new_wire(&out));
    let output = (tree != Tree::Erase).then(|| self.show(&mut ty, &tree));
    self.ivm.link_wire(out, Port::ERASE);
    self.ivm.normalize();

    Ok(output)
  }

  fn parse_line(&mut self, line: &str) -> Result<Vec<Stmt<'core>>, Diag<'core>> {
    let file = self.loader.add_file(None, "input".into(), line);
    let mut parser = VineParser { core: self.core, state: ParserState::new(line), file };
    parser.bump()?;
    let mut stmts = Vec::new();
    while parser.state.token.is_some() {
      stmts.push(parser.parse_stmt()?);
    }
    Ok(stmts)
  }

  fn show(&self, ty: &mut Type<'core>, tree: &Tree) -> String {
    self._show(ty, tree).unwrap_or_else(|| format!("#ivy({})", tree))
  }

  fn _show(&self, ty: &mut Type<'core>, tree: &Tree) -> Option<String> {
    self.checker_state.try_concretize(ty);
    Some(match (ty, tree) {
      (_, Tree::Global(g)) => g.clone(),
      (Type::Bool, Tree::N32(0)) => "false".into(),
      (Type::Bool, Tree::N32(1)) => "true".into(),
      (Type::N32, Tree::N32(n)) => format!("{n}"),
      (Type::F32, Tree::F32(n)) => format!("{n:?}"),
      (Type::Char, Tree::N32(n)) => format!("{:?}", char::try_from(*n).ok()?),
      (Type::IO, Tree::Var(v)) if v == "#io" => "#io".into(),
      (Type::Tuple(tys), _) if tys.is_empty() => "()".into(),
      (Type::Tuple(tys), _) if tys.len() == 1 => format!("({},)", self.show(&mut tys[0], tree)),
      (Type::Tuple(tys), _) if !tys.is_empty() => {
        format!("({})", self.read_tuple(tys, tree)?.join(", "))
      }
      (Type::Object(tys), _) if tys.is_empty() => "{}".into(),
      (Type::Object(tys), _) => {
        let values = self.read_tuple(tys.values_mut(), tree)?;
        format!(
          "{{ {} }}",
          tys.keys().zip(values).map(|(k, v)| format!("{k}: {v}")).collect::<Vec<_>>().join(", ")
        )
      }
      (Type::Adt(def, args), tree)
        if self.resolver.builtins.get(&Builtin::List) == Some(def)
          || self.resolver.builtins.get(&Builtin::String) == Some(def) =>
      {
        let Tree::Comb(c, l, r) = tree else { None? };
        let "tup" = &**c else { None? };
        let Tree::N32(len) = **l else { None? };
        let Tree::Comb(c, l, r) = &**r else { None? };
        let "tup" = &**c else { None? };
        let mut cur = &**l;
        let mut children = vec![];
        for _ in 0..len {
          let Tree::Comb(c, l, r) = cur else { None? };
          let "tup" = &**c else { None? };
          children.push(l);
          cur = r;
        }
        if &**r != cur || !matches!(cur, Tree::Var(_)) {
          None?
        }
        if self.resolver.builtins.get(&Builtin::String) == Some(def) {
          let str = children
            .into_iter()
            .map(|x| {
              let Tree::N32(n) = **x else { Err(())? };
              char::from_u32(n).ok_or(())
            })
            .collect::<Result<String, ()>>()
            .ok()?;
          format!("{str:?}")
        } else {
          let [arg] = &mut **args else { None? };
          self.checker_state.try_concretize(arg);
          format!(
            "[{}]",
            children.into_iter().map(|x| self.show(arg, x)).collect::<Vec<_>>().join(", ")
          )
        }
      }
      (Type::Adt(def, args), tree) => {
        let adt = &self.resolver.defs[*def];
        let adt_def = adt.adt_def.as_ref().unwrap();
        if adt_def.variants.len() == 1 {
          let variant = adt_def.variants[0];
          let variant = &self.resolver.defs[variant];
          let variant_def = variant.variant_def.as_ref().unwrap();
          let mut fields = variant_def
            .field_types
            .as_ref()
            .unwrap()
            .iter()
            .map(|x| x.instantiate(args))
            .collect::<Vec<_>>();
          let name = *variant.canonical.segments.last().unwrap();
          if fields.is_empty() {
            name.to_string()
          } else {
            format!("{name}({})", self.read_tuple(&mut fields, tree)?.join(", "))
          }
        } else {
          let variant_count = adt_def.variants.len();
          let mut active_variant = None;
          let mut tree = tree;
          for i in 0..variant_count {
            let Tree::Comb(c, l, r) = tree else { None? };
            let "enum" = &**c else { None? };
            if **l != Tree::Erase {
              if active_variant.is_some() {
                None?
              }
              active_variant = Some((i, &**l));
            }
            tree = r;
          }
          let end = tree;
          if !matches!(end, Tree::Var(_)) {
            None?
          }
          let (variant_i, mut tree) = active_variant?;
          let variant = adt_def.variants[variant_i];
          let variant = &self.resolver.defs[variant];
          let variant_def = variant.variant_def.as_ref().unwrap();
          let name = *variant.canonical.segments.last().unwrap();
          let field_types = variant_def
            .field_types
            .as_ref()
            .unwrap()
            .iter()
            .map(|x| x.instantiate(args))
            .collect::<Vec<_>>();
          let mut fields = Vec::new();
          for mut field in field_types {
            let Tree::Comb(c, l, r) = tree else { None? };
            let "enum" = &**c else { None? };
            fields.push(self.show(&mut field, l));
            tree = r;
          }
          if tree != end {
            None?
          }
          if fields.is_empty() {
            name.to_string()
          } else {
            format!("{name}({})", fields.join(", "))
          }
        }
      }
      (_, Tree::Erase) => "~_".into(),
      _ => None?,
    })
  }

  fn read_tuple<'a>(
    &self,
    tys: impl IntoIterator<Item = &'a mut Type<'core>, IntoIter: DoubleEndedIterator>,
    tree: &Tree,
  ) -> Option<Vec<String>>
  where
    'core: 'a,
  {
    let mut tys = tys.into_iter();
    let mut tup = Vec::new();
    let mut tree = tree;
    let last = tys.next_back().unwrap();
    for ty in tys {
      let Tree::Comb(l, a, b) = tree else { None? };
      let "tup" = &**l else { None? };
      tup.push(self.show(ty, a));
      tree = b;
    }
    tup.push(self.show(last, tree));
    Some(tup)
  }
}

impl Display for Repl<'_, '_, '_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (local, ident) in &self.locals {
      let var = &self.vars[ident];
      let value = self.host.read(self.ivm, &var.value);
      if value != Tree::Erase {
        writeln!(
          f,
          "{} = {}",
          ident.0 .0,
          self.show(&mut Type::Var(self.checker_state.locals[local]), &value)
        )?;
      }
      let space = self.host.read(self.ivm, &var.space);
      if space != Tree::Erase {
        writeln!(
          f,
          "~{} = {}",
          ident.0 .0,
          self.show(
            &mut Type::Inverse(Box::new(Type::Var(self.checker_state.locals[local]))),
            &space
          )
        )?;
      }
    }
    Ok(())
  }
}
