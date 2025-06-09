use std::{
  collections::{BTreeMap, HashMap},
  fmt::Write,
  mem::replace,
  path::PathBuf,
};

use ivm::{
  port::{Port, PortRef, Tag},
  IVM,
};
use ivy::{ast::Tree, host::Host};
use vine_util::{
  idx::Counter,
  parser::{Parser, ParserState},
};

use crate::{
  analyzer::{analyze, usage::Usage},
  ast::{Block, Ident, Local, Span, Stmt},
  chart::{ConcreteConstId, DefId, VariantId},
  charter::{Charter, ExtractItems},
  checker::Checker,
  compiler::{Compiler, Hooks},
  core::Core,
  diag::Diag,
  distiller::Distiller,
  emitter::Emitter,
  normalizer::normalize,
  parser::VineParser,
  specializer::{Spec, SpecRels, Specializer, TemplateId},
  types::{Type, TypeKind, Types},
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
  locals: BTreeMap<Local, (Ident<'core>, Type)>,
  local_count: Counter<Local>,
  types: Types<'core>,
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
    let mut types = Types::default();
    let io_type = if let Some(io) = compiler.chart.builtins.io {
      types.new(TypeKind::Opaque(io, vec![]))
    } else {
      types.nil()
    };
    let locals = BTreeMap::from([(Local(0), (io, io_type))]);

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
      types,
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

    let mut ty = self.types.nil();
    let mut name = String::new();

    let nets = self.compiler.compile(ExecHooks {
      repl_mod: self.repl_mod,
      vars: &mut self.vars,
      ivm: self.ivm,
      locals: &mut self.locals,
      block: &mut block,
      types: &mut self.types,
      ty: &mut ty,
      local_count: &mut self.local_count,
      line: &mut self.line,
      rels: None,
      name: &mut name,
    })?;

    struct ExecHooks<'core, 'ivm, 'ext, 'a> {
      repl_mod: DefId,
      vars: &'a mut HashMap<Ident<'core>, Var<'ivm>>,
      ivm: &'a mut IVM<'ivm, 'ext>,
      locals: &'a mut BTreeMap<Local, (Ident<'core>, Type)>,
      block: &'a mut Block<'core>,
      types: &'a mut Types<'core>,
      ty: &'a mut Type,
      local_count: &'a mut Counter<Local>,
      line: &'a mut usize,
      rels: Option<SpecRels>,
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

      fn check(&mut self, checker: &mut Checker<'core, '_>) {
        let (ty, binds) = checker._check_custom(
          self.repl_mod,
          self.types,
          self.locals,
          self.local_count,
          self.block,
        );
        *self.ty = ty;
        for (ident, local, ty) in binds {
          if self.vars.get(&ident).is_none_or(|v| v.local != local) {
            let wire = self.ivm.new_wire();
            let old = self.vars.insert(
              ident,
              Var { local, value: Port::new_wire(wire.0), space: Port::new_wire(wire.1) },
            );
            self.locals.insert(local, (ident, ty));
            if let Some(old) = old {
              self.locals.remove(&old.local);
              self.ivm.link(old.value, old.space);
            }
          }
        }
      }

      fn specialize(&mut self, specializer: &mut Specializer<'core, '_>) {
        self.rels = Some(specializer._specialize_custom(&mut *self.block));
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
          &Spec {
            def: DefId(0),
            template: TemplateId::Const(ConcreteConstId(0)),
            index: 0,
            singular: true,
            rels: self.rels.take().unwrap(),
          },
        );
      }
    }

    self.host.insert_nets(&nets);

    let w = self.ivm.new_wire();
    let root = w.0;

    let mut cur = w.1;

    let label = self.host.label_to_u16("x");
    for (local, _) in self.locals.values() {
      let var = self.vars.get_mut(local).unwrap();
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
    let output = self.show(ty, &tree);
    self.ivm.link_wire(out, Port::ERASE);
    self.ivm.normalize();

    Ok((output != "()").then_some(output))
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

  fn show(&mut self, ty: Type, tree: &Tree) -> String {
    self._show(ty, tree).unwrap_or_else(|| format!("#ivy({})", tree))
  }

  fn _show(&mut self, ty: Type, tree: &Tree) -> Option<String> {
    let builtins = &self.compiler.chart.builtins;
    let (inv, kind) = self.types.kind(ty)?;
    if inv.0 {
      return None;
    }
    Some(match (kind, tree) {
      (_, Tree::Global(g)) => g.clone(),
      (TypeKind::Opaque(id, _), Tree::N32(0)) if builtins.bool == Some(*id) => "false".into(),
      (TypeKind::Opaque(id, _), Tree::N32(1)) if builtins.bool == Some(*id) => "true".into(),
      (TypeKind::Opaque(id, _), Tree::N32(n)) if builtins.n32 == Some(*id) => {
        format!("{n}")
      }
      (TypeKind::Opaque(id, _), Tree::N32(n)) if builtins.i32 == Some(*id) => {
        format!("{:+}", *n as i32)
      }
      (TypeKind::Opaque(id, _), Tree::F32(n)) if builtins.f32 == Some(*id) => {
        format!("{n:?}")
      }
      (TypeKind::Opaque(id, _), Tree::N32(n)) if builtins.char == Some(*id) => {
        format!("{:?}", char::try_from(*n).ok()?)
      }
      (TypeKind::Opaque(id, _), Tree::Var(v)) if builtins.io == Some(*id) && v == "#io" => {
        "#io".into()
      }
      (TypeKind::Tuple(tys), _) if tys.is_empty() => "()".into(),
      (TypeKind::Tuple(tys), _) if tys.len() == 1 => format!("({},)", self.show(tys[0], tree)),
      (TypeKind::Tuple(tys), _) if !tys.is_empty() => {
        format!("({})", self.read_tuple(tys.clone(), tree)?.join(", "))
      }
      (TypeKind::Object(tys), _) if tys.is_empty() => "{}".into(),
      (TypeKind::Object(tys), _) => {
        let tys = tys.clone();
        let values = self.read_tuple(tys.values().copied(), tree)?;
        format!(
          "{{ {} }}",
          tys.keys().zip(values).map(|(k, v)| format!("{k}: {v}")).collect::<Vec<_>>().join(", ")
        )
      }
      (TypeKind::Struct(def, args), tree)
        if builtins.list == Some(*def) || builtins.string == Some(*def) =>
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
        if builtins.string == Some(*def) {
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
          let [arg] = **args else { None? };
          format!(
            "[{}]",
            children.into_iter().map(|x| self.show(arg, x)).collect::<Vec<_>>().join(", ")
          )
        }
      }
      (TypeKind::Struct(struct_id, args), tree) => {
        let name = self.compiler.chart.structs[*struct_id].name;
        let args = args.clone();
        let data = self.types.import(&self.compiler.sigs.structs[*struct_id], Some(&args)).data;
        let data = self.show(data, tree);
        if data.starts_with("(") {
          format!("{name}{}", data)
        } else {
          format!("{name}({})", data)
        }
      }
      (TypeKind::Enum(enum_id, args), tree) => {
        let enum_def = &self.compiler.chart.enums[*enum_id];
        let variant_count = enum_def.variants.len();
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
        let variant = &enum_def.variants[variant_id];
        let name = variant.name;
        let enum_id = *enum_id;
        let args = args.clone();
        let data =
          self.types.import_with(&self.compiler.sigs.enums[enum_id], Some(&args), |t, sig| {
            Some(t.transfer(&sig.variant_data[variant_id]?))
          });
        let data = if let Some(ty) = data {
          let Tree::Comb(c, l, r) = tree else { None? };
          let "enum" = &**c else { None? };
          tree = r;
          Some(self.show(ty, l))
        } else {
          None
        };

        if tree != end {
          None?
        }
        if let Some(data) = data {
          if data.starts_with("(") {
            format!("{name}{}", data)
          } else {
            format!("{name}({})", data)
          }
        } else {
          name.to_string()
        }
      }
      (_, Tree::Erase) => "~_".into(),
      _ => None?,
    })
  }

  fn read_tuple<'a>(
    &mut self,
    tys: impl IntoIterator<Item = Type, IntoIter: DoubleEndedIterator>,
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

  pub fn format(&mut self) -> String {
    let mut str = String::new();
    for local in self.locals.keys().copied().collect::<Vec<_>>() {
      let (ident, ty) = self.locals[&local];
      let var = &self.vars[&ident];
      let value = self.host.read(self.ivm, &var.value);
      let space = self.host.read(self.ivm, &var.space);
      if value != Tree::Erase {
        writeln!(str, "{} = {}", ident.0 .0, self.show(ty, &value)).unwrap();
      }
      if space != Tree::Erase {
        writeln!(str, "~{} = {}", ident.0 .0, self.show(ty.inverse(), &space)).unwrap();
      }
    }
    str
  }
}
