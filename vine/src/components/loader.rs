use std::{
  env::current_dir,
  fs,
  path::{Path, PathBuf},
};

use vine_util::{idx::IdxVec, new_idx};

use crate::{
  compiler::Compiler,
  components::parser::Parser,
  structures::{
    ast::{
      Ident, Item, ItemKind, ModKind, Span,
      visit::{VisitMut, Visitee},
    },
    diag::{Diag, Diags, FileInfo},
  },
};

new_idx!(pub FileId);

pub struct Loader<'a, F: FS> {
  fs: &'a mut F,
  file_paths: Option<&'a mut IdxVec<FileId, F::Path>>,
  files: &'a mut IdxVec<FileId, FileInfo>,
  loaded: &'a mut Vec<Module>,
  diags: &'a mut Diags,
}

pub struct Module {
  pub name: Ident,
  pub main: bool,
  pub kind: ModKind,
}

impl<'a, F: FS> Loader<'a, F> {
  pub fn new(
    compiler: &'a mut Compiler,
    fs: &'a mut F,
    file_paths: Option<&'a mut IdxVec<FileId, F::Path>>,
  ) -> Self {
    Loader {
      fs,
      file_paths,
      files: &mut compiler.files,
      loaded: &mut compiler.loaded,
      diags: &mut compiler.diags,
    }
  }

  pub fn load_main_mod(&mut self, name: Ident, path: F::Path) {
    self._load_mod(name, path, true);
  }

  pub fn load_mod(&mut self, name: Ident, path: F::Path) {
    self._load_mod(name, path, false);
  }

  fn _load_mod(&mut self, name: Ident, path: F::Path, main: bool) {
    let kind = match self.fs.kind(&path) {
      Some(EntryKind::File) => self.load_file(Span::NONE, path, None),
      Some(EntryKind::Dir) => self.load_dir(Span::NONE, &name, path),
      None => {
        let path = self.fs.display(&path);
        self.error_mod(Diag::CannotRead { span: Span::NONE, path })
      }
    };
    self.loaded.push(Module { name, main, kind });
  }

  fn load_file(&mut self, span: Span, path: F::Path, base: Option<&F::Path>) -> ModKind {
    let Some(src) = self.fs.read_file(&path) else {
      let diag = Diag::CannotRead { span, path: self.fs.display(&path) };
      return self.error_mod(diag);
    };
    let file = self.files.next_index();
    let parse_result = Parser::parse(file, &src);
    let span = Span { file, start: 0, end: src.len() };
    self.files.push_to(file, FileInfo::new(self.fs.display(&path), src));
    if let Some(file_paths) = &mut self.file_paths {
      file_paths.push_to(file, path);
    }
    let mut items = match parse_result {
      Ok(items) => items,
      Err(diag) => return self.error_mod(diag),
    };
    self.load_children(base, &mut items);
    ModKind::Loaded(span, Some(file), items)
  }

  fn error_mod(&mut self, diag: Diag) -> ModKind {
    ModKind::Error(self.diags.error(diag))
  }

  fn load_dir(&mut self, span: Span, name: &Ident, path: F::Path) -> ModKind {
    let file_path = self.fs.child_file(&path, name);
    self.load_file(span, file_path, Some(&path))
  }

  fn load_child(&mut self, span: Span, base: Option<&F::Path>, name: &Ident) -> ModKind {
    let Some(base) = base else { return self.error_mod(Diag::InvalidSubmodule { span }) };
    let file_path = self.fs.child_file(base, name);
    let dir_path = self.fs.child_dir(base, name);
    let file_valid = self.fs.kind(&file_path) == Some(EntryKind::File);
    let dir_valid = self.fs.kind(&dir_path) == Some(EntryKind::Dir);
    match (file_valid, dir_valid) {
      (true, false) => self.load_file(span, file_path, None),
      (false, true) => self.load_dir(span, name, dir_path),
      (true, true) => {
        let diag = Diag::AmbiguousSubmodule {
          span,
          file_path: self.fs.display(&file_path),
          dir_path: self.fs.display(&dir_path),
        };
        self.error_mod(diag)
      }
      (false, false) => {
        let diag = Diag::MissingSubmodule {
          span,
          file_path: self.fs.display(&file_path),
          dir_path: self.fs.display(&dir_path),
        };
        self.error_mod(diag)
      }
    }
  }

  fn load_children<'t>(&mut self, base: Option<&F::Path>, visitee: impl Visitee<'t>) {
    LoadChildren { loader: self, base }.visit(visitee);
  }
}

struct LoadChildren<'a, 'b, F: FS> {
  loader: &'b mut Loader<'a, F>,
  base: Option<&'b F::Path>,
}

impl<F: FS> VisitMut<'_> for LoadChildren<'_, '_, F> {
  fn visit_item(&mut self, item: &mut Item) {
    if let ItemKind::Mod(module) = &mut item.kind
      && let ModKind::Unloaded = &mut module.kind
    {
      module.kind = self.loader.load_child(item.span, self.base, &module.name);
    } else {
      self._visit_item(item);
    }
  }
}

pub trait FS {
  type Path;

  fn display(&mut self, path: &Self::Path) -> String;

  fn kind(&mut self, path: &Self::Path) -> Option<EntryKind>;

  fn child_dir(&mut self, path: &Self::Path, name: &Ident) -> Self::Path;
  fn child_file(&mut self, path: &Self::Path, name: &Ident) -> Self::Path;

  fn read_file(&mut self, path: &Self::Path) -> Option<String>;
}

#[derive(Clone)]
pub struct RealFS {
  pub cwd: PathBuf,
}

impl FS for RealFS {
  type Path = PathBuf;

  fn display(&mut self, path: &Self::Path) -> String {
    path.strip_prefix(&self.cwd).unwrap_or(path).display().to_string()
  }

  fn kind(&mut self, path: &Self::Path) -> Option<EntryKind> {
    let file_type = fs::metadata(path).ok()?.file_type();
    if file_type.is_file() {
      Some(EntryKind::File)
    } else if file_type.is_dir() {
      Some(EntryKind::Dir)
    } else {
      None
    }
  }

  fn child_dir(&mut self, path: &Self::Path, name: &Ident) -> Self::Path {
    path.join(&name.0)
  }

  fn child_file(&mut self, path: &Self::Path, name: &Ident) -> Self::Path {
    let mut path = path.join(&name.0);
    path.as_mut_os_string().push(".vi");
    path
  }

  fn read_file(&mut self, path: &Self::Path) -> Option<String> {
    fs::read_to_string(path).ok()
  }
}

impl RealFS {
  pub fn detect_name(path: &Path) -> Option<Ident> {
    let path = path.file_name()?.to_str()?;
    let path = path.strip_suffix(".vi").unwrap_or(path);
    Ident::new(path)
  }
}

impl Default for RealFS {
  fn default() -> Self {
    RealFS { cwd: current_dir().unwrap() }
  }
}

#[derive(PartialEq, Eq)]
pub enum EntryKind {
  File,
  Dir,
}
