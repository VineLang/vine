use std::{
  env::current_dir,
  fs, io,
  mem::take,
  path::{Path, PathBuf},
  str,
};

use vine_util::{idx::IdxVec, new_idx};

use crate::{
  components::parser::VineParser,
  structures::{
    ast::{
      Ident, Item, ItemKind, ModKind, Span,
      visit::{VisitMut, Visitee},
    },
    checkpoint::Checkpoint,
    diag::{Diag, Diags, FileInfo},
  },
};

new_idx!(pub FileId);

pub struct Loader {
  cwd: PathBuf,
  modules: Vec<Module>,
  pub files: IdxVec<FileId, FileInfo>,
}

pub struct Module {
  pub name: Ident,
  pub main: bool,
  pub kind: ModKind,
}

impl Default for Loader {
  fn default() -> Self {
    Self { cwd: current_dir().unwrap(), modules: Vec::new(), files: Default::default() }
  }
}

impl Loader {
  pub fn finish(&mut self) -> Vec<Module> {
    take(&mut self.modules)
  }

  pub fn load_main_mod(&mut self, path: &Path, diags: &mut Diags) {
    self._load_mod(path, diags, true);
  }

  pub fn load_mod(&mut self, path: &Path, diags: &mut Diags) {
    self._load_mod(path, diags, false);
  }

  fn _load_mod(&mut self, path: &Path, diags: &mut Diags, main: bool) {
    let name = self.auto_mod_name(path);
    let kind = self.load_file(None, ModSpec::Explicit(path), Span::NONE, diags);
    self.modules.push(Module { name, main, kind });
  }

  fn auto_mod_name(&self, path: &Path) -> Ident {
    Ident(str::from_utf8(path.file_stem().unwrap().as_encoded_bytes()).unwrap().into())
  }

  pub(crate) fn add_file(&mut self, path: Option<PathBuf>, name: String, src: String) -> FileId {
    self.files.push(FileInfo::new(path, name, src))
  }

  fn load_file(
    &mut self,
    base: Option<&Path>,
    spec: ModSpec<'_>,
    span: Span,
    diags: &mut Diags,
  ) -> ModKind {
    match self._load_file(base, spec, span, diags) {
      Ok(mod_) => mod_,
      Err(diag) => ModKind::Error(diags.error(diag)),
    }
  }

  fn _load_file(
    &mut self,
    base: Option<&Path>,
    spec: ModSpec<'_>,
    span: Span,
    diags: &mut Diags,
  ) -> Result<ModKind, Diag> {
    let (file, path) = match (base, spec) {
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
        let mut path_1 = base.parent().unwrap().join(format!("{name}.vi"));
        let mut path_2 = base.parent().unwrap().join(format!("{name}/{name}.vi"));
        match (self.read_file(&mut path_1, span), self.read_file(&mut path_2, span)) {
          (Ok(_), Ok(_)) => Err(Diag::AmbiguousImplicitMod { span, name })?,
          (Ok(info), Err(_)) => (info, path_1),
          (Err(_), Ok(info)) => (info, path_2),
          (Err(diag_1), Err(diag_2)) => {
            let err = diags.error(diag_1);
            diags.error(diag_2);
            Err(err)?
          }
        }
      }
      (None, ModSpec::Implicit(_)) => unreachable!(),
    };

    let src = &self.files[file].src;
    let mut items = VineParser::parse(src, file)?;
    let span = Span { file, start: 0, end: src.len() };
    self.load_deps(&path, &mut items, diags);
    Ok(ModKind::Loaded(span, Some(file), items))
  }

  fn read_file(&mut self, path: &mut PathBuf, span: Span) -> Result<FileId, Diag> {
    self._read_file(path).map_err(|err| Diag::FsError {
      span,
      path: path.strip_prefix(&self.cwd).unwrap_or(&*path).to_owned(),
      err,
    })
  }

  fn _read_file(&mut self, path: &mut PathBuf) -> Result<FileId, io::Error> {
    let src = fs::read_to_string(&*path)?;
    *path = path.canonicalize()?;
    let name = path.strip_prefix(&self.cwd).unwrap_or(path).display().to_string();
    Ok(self.add_file(Some(path.clone()), name, src))
  }

  pub(crate) fn load_deps<'t>(
    &mut self,
    base: &Path,
    visitee: impl Visitee<'t>,
    diags: &mut Diags,
  ) {
    LoadDeps { loader: self, base, diags }.visit(visitee);
  }

  pub(crate) fn revert(&mut self, checkpoint: &Checkpoint) {
    self.files.truncate(checkpoint.files.0);
  }
}

struct LoadDeps<'a> {
  loader: &'a mut Loader,
  base: &'a Path,
  diags: &'a mut Diags,
}

impl VisitMut<'_> for LoadDeps<'_> {
  fn visit_item(&mut self, item: &mut Item) {
    if let ItemKind::Mod(module) = &mut item.kind
      && let ModKind::Unloaded(_, path) = &mut module.kind
    {
      module.kind = self.loader.load_file(
        Some(self.base),
        match &*path {
          Some(path) => ModSpec::Explicit(path.as_ref()),
          None => ModSpec::Implicit(module.name.clone()),
        },
        item.name_span,
        self.diags,
      );
      return;
    }
    self._visit_item(item);
  }
}

enum ModSpec<'a> {
  Explicit(&'a Path),
  Implicit(Ident),
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
