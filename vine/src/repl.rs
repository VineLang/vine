use std::{
  collections::{BTreeMap, HashMap},
  fmt::{self, Display},
  mem::{replace, take},
  path::PathBuf,
};

use ivm::{
  port::{Port, PortRef, Tag},
  IVM,
};
use ivy::{ast::Tree, host::Host};
use vine_util::{
  idx::{Counter, IdxVec, IntMap},
  parser::{Parser, ParserState},
};

use crate::{
  analyzer::{analyze, usage::Usage},
  ast::{Block, Ident, Local, Span, Stmt},
  chart::{DefId, ValueDefId, VariantId},
  charter::{Charter, ExtractItems},
  checker::{self, Checker, Type},
  compiler::{Compiler, Hooks},
  core::Core,
  diag::Diag,
  distiller::Distiller,
  emitter::Emitter,
  normalizer::normalize,
  parser::VineParser,
  resolver::Resolver,
  specializer::{RelId, Spec, SpecId, Specializer},
  unifier::Unifier,
  vir::{InterfaceId, StageId},
  visit::VisitMut,
};

pub struct Repl<'core, 'ctx, 'ivm, 'ext> {
  host: &'ivm mut Host<'ivm>,
  ivm: &'ctx mut IVM<'ivm, 'ext>,
  core: &'core Core<'core>,
  compiler: Compiler<'core>,
  repl_mod: DefId,
  line: usize,
  vars: HashMap<Ident<'core>, Var<'ivm>>,
  locals: BTreeMap<Local, Ident<'core>>,
  local_count: Counter<Local>,
  unifier: Unifier<'core>,
  local_types: IntMap<Local, checker::Var>,
}

#[derive(Debug)]
struct Var<'ivm> {
  local: Local,
  value: Port<'ivm>,
  space: Port<'ivm>,
}

impl<'core, 'ctx, 'ivm, 'ext> Repl<'core, 'ctx, 'ivm, 'ext> {
  pub fn new(
    mut host: &'ivm mut Host<'ivm>,
    ivm: &'ctx mut IVM<'ivm, 'ext>,
    core: &'core Core<'core>,
    libs: Vec<PathBuf>,
  ) -> Result<Self, String> {
    let mut compiler = Compiler::new(core);
    for lib in libs {
      compiler.loader.load_mod(lib);
    }

    let mut repl_mod = DefId::default();
    let nets = compiler.compile(InitHooks(&mut repl_mod))?;
    struct InitHooks<'a>(&'a mut DefId);
    impl Hooks<'_> for InitHooks<'_> {
      fn chart(&mut self, charter: &mut Charter) {
        *self.0 = charter.chart_child(DefId::ROOT, charter.core.ident("repl"), DefId::ROOT, true);
      }
    }
    host.insert_nets(&nets);

    let io = core.ident("io");
    let vars = HashMap::from([(
      io,
      Var { local: Local(0), value: Port::new_ext_val(host.new_io()), space: Port::ERASE },
    )]);
    let locals = BTreeMap::from([(Local(0), io)]);
    let mut unifier = Unifier::new(core);
    let var = unifier._new_var(Span::NONE);
    _ = unifier.unify(&mut Type::Var(var), &mut Type::IO);
    let local_types = IntMap::from_iter([(Local(0), var)]);

    Ok(Repl {
      host,
      ivm,
      core,
      compiler,
      repl_mod,
      line: 0,
      vars,
      locals,
      local_count: Counter(Local(1)),
      unifier,
      local_types,
    })
  }

  pub fn exec(&mut self, line: &str) -> Result<Option<String>, String> {
    let stmts = match self.parse_line(line) {
      Ok(stmts) => stmts,
      Err(diag) => {
        self.core.report(diag);
        self.core.bail()?;
        unreachable!()
      }
    };
    let mut block = Block { span: Span::NONE, stmts };

    self.compiler.loader.load_deps(".".as_ref(), &mut block);

    let mut ty = Type::NIL;
    let mut name = String::new();

    let nets = self.compiler.compile(ExecHooks {
      repl_mod: self.repl_mod,
      vars: &mut self.vars,
      ivm: self.ivm,
      locals: &mut self.locals,
      block: &mut block,
      unifier: &mut self.unifier,
      local_types: &mut self.local_types,
      ty: &mut ty,
      local_count: &mut self.local_count,
      line: &mut self.line,
      rels: IdxVec::new(),
      name: &mut name,
    })?;

    struct ExecHooks<'core, 'ivm, 'ext, 'a> {
      repl_mod: DefId,
      vars: &'a mut HashMap<Ident<'core>, Var<'ivm>>,
      ivm: &'a mut IVM<'ivm, 'ext>,
      locals: &'a mut BTreeMap<Local, Ident<'core>>,
      block: &'a mut Block<'core>,
      unifier: &'a mut Unifier<'core>,
      local_types: &'a mut IntMap<Local, checker::Var>,
      ty: &'a mut Type<'core>,
      local_count: &'a mut Counter<Local>,
      line: &'a mut usize,
      rels: IdxVec<RelId, SpecId>,
      name: &'a mut String,
    }

    impl<'core> Hooks<'core> for ExecHooks<'core, '_, '_, '_> {
      fn chart(&mut self, charter: &mut Charter<'core, '_>) {
        let mut extractor = ExtractItems::default();
        extractor.visit(&mut *self.block);
        for item in extractor.items {
          charter.chart_item(self.repl_mod, item, self.repl_mod);
        }
      }

      fn resolve(&mut self, resolver: &mut Resolver<'core, '_>) {
        let binds = resolver.resolve_custom(
          self.repl_mod,
          self.locals,
          self.local_count,
          &mut self.block.stmts,
        );
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
      }

      fn check(&mut self, checker: &mut Checker<'core, '_>) {
        *self.ty = checker._check_custom(self.repl_mod, self.unifier, self.local_types, self.block);
      }

      fn specialize(&mut self, specializer: &mut Specializer<'core, '_>) {
        self.rels = specializer._specialize_custom(&mut *self.block);
      }

      fn emit(&mut self, distiller: &mut Distiller<'core, '_>, emitter: &mut Emitter<'core, '_>) {
        let line = *self.line;
        *self.line += 1;

        *self.name = format!("::repl::{line}");

        let mut vir = distiller.distill_root(*self.local_count, |distiller, stage, local| {
          let result = distiller.distill_block(stage, self.block);
          stage.set_local_to(local, result);
        });
        vir.stages[StageId(0)].declarations.retain(|l| !self.locals.contains_key(l));
        vir.globals.extend(self.vars.values().map(|v| (v.local, Usage::Mut)));
        let mut vir = normalize(&vir);
        analyze(&mut vir);
        for var in self.vars.values() {
          vir.interfaces[InterfaceId(0)].wires.insert(var.local, (Usage::Mut, Usage::Mut));
        }
        emitter.emit_vir(
          &vir,
          self.name,
          &Spec { value: ValueDefId(0), index: 0, singular: true, rels: take(&mut self.rels) },
        );
      }
    }

    self.host.insert_nets(&nets);

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
    let file = self.compiler.loader.add_file(None, "input".into(), line);
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
    self.unifier.try_concretize(ty);
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
        if self.compiler.chart.builtins.list == Some(*def)
          || self.compiler.chart.builtins.string == Some(*def) =>
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
        if self.compiler.chart.builtins.string == Some(*def) {
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
          self.unifier.try_concretize(arg);
          format!(
            "[{}]",
            children.into_iter().map(|x| self.show(arg, x)).collect::<Vec<_>>().join(", ")
          )
        }
      }
      (Type::Adt(adt_id, args), tree) => {
        let adt = &self.compiler.chart.adts[*adt_id];
        if adt.variants.len() == 1 {
          let mut fields = self.compiler.types.adt_types[*adt_id][VariantId(0)]
            .iter()
            .map(|x| x.instantiate(args))
            .collect::<Vec<_>>();
          let name = adt.variants[VariantId(0)].name;
          if fields.is_empty() {
            name.to_string()
          } else {
            format!("{name}({})", self.read_tuple(&mut fields, tree)?.join(", "))
          }
        } else {
          let variant_count = adt.variants.len();
          let mut active_variant = None;
          let mut tree = tree;
          for i in 0..variant_count {
            let Tree::Comb(c, l, r) = tree else { None? };
            let "enum" = &**c else { None? };
            if **l != Tree::Erase {
              if active_variant.is_some() {
                None?
              }
              active_variant = Some((VariantId(i), &**l));
            }
            tree = r;
          }
          let end = tree;
          if !matches!(end, Tree::Var(_)) {
            None?
          }
          let (variant_id, mut tree) = active_variant?;
          let variant = &adt.variants[variant_id];
          let name = variant.name;
          let field_types = self.compiler.types.adt_types[*adt_id][variant_id]
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

impl Display for Repl<'_, '_, '_, '_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (local, ident) in &self.locals {
      let var = &self.vars[ident];
      let value = self.host.read(self.ivm, &var.value);
      if value != Tree::Erase {
        writeln!(
          f,
          "{} = {}",
          ident.0 .0,
          self.show(&mut Type::Var(self.local_types[local]), &value)
        )?;
      }
      let space = self.host.read(self.ivm, &var.space);
      if space != Tree::Erase {
        writeln!(
          f,
          "~{} = {}",
          ident.0 .0,
          self.show(&mut Type::Inverse(Box::new(Type::Var(self.local_types[local]))), &space)
        )?;
      }
    }
    Ok(())
  }
}
