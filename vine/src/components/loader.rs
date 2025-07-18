use std::{
  env::current_dir,
  fs,
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
    core::Core,
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

  pub fn load_main_mod(&mut self, path: impl Into<PathBuf>) {
    self.load_mod(path);
    self.root.last_mut().unwrap().attrs.push(Attr { span: Span::NONE, kind: AttrKind::Main });
  }

  pub fn load_mod(&mut self, path: impl Into<PathBuf>) {
    let path = path.into();
    let module = Item {
      span: Span::NONE,
      vis: Vis::Public,
      attrs: Vec::new(),
      kind: ItemKind::Mod(ModItem {
        name: self.auto_mod_name(&path),
        generics: Generics::empty(Span::NONE),
        kind: self.load_file(path, Span::NONE),
      }),
    };
    self.root.push(module);
  }

  fn auto_mod_name(&self, path: &Path) -> Ident<'core> {
    self.core.ident(str::from_utf8(path.file_stem().unwrap().as_encoded_bytes()).unwrap())
  }

  pub(crate) fn add_file(&mut self, path: Option<PathBuf>, name: String, src: &str) -> usize {
    let mut files = self.core.files.borrow_mut();
    let file = files.len();
    files.push(FileInfo::new(path, name, src));
    file
  }

  fn load_file(&mut self, path: PathBuf, span: Span) -> ModKind<'core> {
    match self._load_file(path, span) {
      Ok(items) => ModKind::Loaded(span, items),
      Err(diag) => ModKind::Error(self.core.report(diag)),
    }
  }

  fn _load_file(&mut self, mut path: PathBuf, span: Span) -> Result<Vec<Item<'core>>, Diag<'core>> {
    let fs_err = |err| Diag::FsError {
      span,
      path: path.strip_prefix(&self.cwd).unwrap_or(&path).to_owned(),
      err,
    };
    let src = fs::read_to_string(&path).map_err(fs_err)?;
    path = path.canonicalize().map_err(fs_err)?;
    let name = path.strip_prefix(&self.cwd).unwrap_or(&path).display().to_string();
    let file = self.add_file(Some(path.clone()), name, &src);
    let mut items = VineParser::parse(self.core, &src, file)?;
    path.pop();
    for item in &mut items {
      self.load_deps(&path, item);
    }
    Ok(items)
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
        module.kind = self.loader.load_file(self.base.join(path), item.span);
        return;
      }
    }
    self._visit_item(item);
  }
}
