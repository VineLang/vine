use std::{
  env::current_dir,
  fs, io,
  mem::take,
  path::{Path, PathBuf},
  str,
};

use crate::{
  components::parser::VineParser,
  structures::{
    ast::{
      visit::{VisitMut, Visitee},
      Attr, AttrKind, Generics, Ident, Item, ItemKind, ModItem, ModKind, Span, Vis,
    },
    core::{Core, FileId},
    diag::{Diag, FileInfo},
  },
};

pub struct Loader<'core> {
  core: &'core Core<'core>,
  cwd: PathBuf,
  root: Vec<Item<'core>>,
}

impl<'core> Loader<'core> {
  pub fn new(core: &'core Core<'core>) -> Self {
    Self { core, cwd: current_dir().unwrap(), root: Vec::new() }
  }

  pub fn finish(&mut self) -> ModKind<'core> {
    ModKind::Loaded(Span::NONE, take(&mut self.root))
  }

  pub fn load_main_mod(&mut self, path: &Path) {
    self.load_mod(path);
    self.root.last_mut().unwrap().attrs.push(Attr { span: Span::NONE, kind: AttrKind::Main });
  }

  pub fn load_mod(&mut self, path: &Path) {
    let module = Item {
      span: Span::NONE,
      vis: Vis::Public,
      attrs: Vec::new(),
      kind: ItemKind::Mod(ModItem {
        name: self.auto_mod_name(path),
        generics: Generics::empty(Span::NONE),
        kind: self.load_file(None, ModSpec::Explicit(path), Span::NONE),
      }),
    };
    self.root.push(module);
  }

  fn auto_mod_name(&self, path: &Path) -> Ident<'core> {
    self.core.ident(str::from_utf8(path.file_stem().unwrap().as_encoded_bytes()).unwrap())
  }

  pub(crate) fn add_file(&mut self, path: Option<PathBuf>, name: String, src: &str) -> FileId {
    self.core.files.borrow_mut().push(FileInfo::new(path, name, src))
  }

  fn load_file(
    &mut self,
    base: Option<&Path>,
    spec: ModSpec<'core, '_>,
    span: Span,
  ) -> ModKind<'core> {
    match self._load_file(base, spec, span) {
      Ok(mod_) => mod_,
      Err(diag) => ModKind::Error(self.core.report(diag)),
    }
  }

  fn _load_file(
    &mut self,
    base: Option<&Path>,
    spec: ModSpec<'core, '_>,
    span: Span,
  ) -> Result<ModKind<'core>, Diag<'core>> {
    let ((src, file), path) = match (base, spec) {
      (None, ModSpec::Explicit(path)) => {
        let mut path = path.to_owned();
        (self.read_file(&mut path, span)?, path)
      }
      (Some(base), ModSpec::Explicit(path)) => {
        let mut path = base.parent().unwrap().join(path);
        (self.read_file(&mut path, span)?, path)
      }
      (Some(base), ModSpec::Implicit(name)) => {
        if !implicit_path_valid(base) {
          Err(Diag::DisallowedImplicitMod { span })?
        }
        let name = name.0 .0;
        let mut path_1 = base.parent().unwrap().join(format!("{name}.vi"));
        let mut path_2 = base.parent().unwrap().join(format!("{name}/{name}.vi"));
        match (self.read_file(&mut path_1, span), self.read_file(&mut path_2, span)) {
          (Ok(_), Ok(_)) => Err(Diag::AmbiguousImplicitMod { span, name })?,
          (Ok(info), Err(_)) => (info, path_1),
          (Err(_), Ok(info)) => (info, path_2),
          (Err(diag_1), Err(diag_2)) => {
            let err = self.core.report(diag_1);
            self.core.report(diag_2);
            Err(err)?
          }
        }
      }
      (None, ModSpec::Implicit(_)) => unreachable!(),
    };

    let mut items = VineParser::parse(self.core, &src, file)?;
    self.load_deps(&path, &mut items);
    Ok(ModKind::Loaded(span, items))
  }

  fn read_file(&mut self, path: &mut PathBuf, span: Span) -> Result<(String, FileId), Diag<'core>> {
    self._read_file(path).map_err(|err| Diag::FsError {
      span,
      path: path.strip_prefix(&self.cwd).unwrap_or(&*path).to_owned(),
      err,
    })
  }

  fn _read_file(&mut self, path: &mut PathBuf) -> Result<(String, FileId), io::Error> {
    let src = fs::read_to_string(&*path)?;
    *path = path.canonicalize()?;
    let name = path.strip_prefix(&self.cwd).unwrap_or(path).display().to_string();
    let file = self.add_file(Some(path.clone()), name, &src);
    Ok((src, file))
  }

  pub(crate) fn load_deps<'t>(&mut self, base: &Path, visitee: impl Visitee<'core, 't>) {
    LoadDeps { loader: self, base }.visit(visitee);
  }
}

struct LoadDeps<'core, 'a> {
  loader: &'a mut Loader<'core>,
  base: &'a Path,
}

impl<'core> VisitMut<'core, '_> for LoadDeps<'core, '_> {
  fn visit_item<'a>(&'a mut self, item: &mut Item<'core>) {
    if let ItemKind::Mod(module) = &mut item.kind {
      if let ModKind::Unloaded(_, path) = &mut module.kind {
        module.kind = self.loader.load_file(
          Some(self.base),
          match &*path {
            Some(path) => ModSpec::Explicit(path.as_ref()),
            None => ModSpec::Implicit(module.name),
          },
          item.span,
        );
        return;
      }
    }
    self._visit_item(item);
  }
}

enum ModSpec<'core, 'a> {
  Explicit(&'a Path),
  Implicit(Ident<'core>),
}

fn implicit_path_valid(path: &Path) -> bool {
  let Some(file_name) = path.file_name() else { return false };
  let Some(file_name) = file_name.to_str() else { return false };
  let Some(file_name) = file_name.strip_suffix(".vi") else { return false };
  let Some(dir) = path.parent() else { return false };
  let Some(dir_name) = dir.file_name() else { return false };
  let Some(dir_name) = dir_name.to_str() else { return false };
  file_name == dir_name
}
