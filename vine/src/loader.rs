use std::{
  env::current_dir,
  fs,
  mem::take,
  path::{Path, PathBuf},
  str,
};

use vine_util::interner::StringInterner;

use crate::{
  ast::{self, ConstItem, Ident, Item, ItemKind, ModItem, ModKind, Span, Term, TermKind},
  diag::{Diag, DiagGroup, FileInfo},
  parser::VineParser,
  visit::{VisitMut, Visitee},
};

pub struct Loader<'ctx> {
  cwd: PathBuf,
  interner: &'ctx StringInterner<'static>,
  root: Vec<Item>,
  pub files: Vec<FileInfo>,
  pub diags: DiagGroup,
}

impl<'ctx> Loader<'ctx> {
  pub fn new(interner: &'ctx StringInterner<'static>) -> Self {
    Self {
      cwd: current_dir().unwrap(),
      interner,
      root: Vec::new(),
      files: Vec::new(),
      diags: DiagGroup::default(),
    }
  }

  pub fn finish(&mut self) -> ModKind {
    ModKind::Loaded(take(&mut self.root))
  }

  pub fn load_main_mod(&mut self, path: impl Into<PathBuf>) {
    let path = path.into();
    let main = Ident(self.interner.intern("main"));
    self.root.push(Item {
      span: Span::NONE,
      kind: ItemKind::Const(ConstItem {
        name: main,
        value: Term {
          span: Span::NONE,
          kind: TermKind::Path(ast::Path {
            span: Span::NONE,
            segments: vec![self.auto_mod_name(&path), main],
            absolute: true,
            resolved: None,
          }),
        },
      }),
    });
    self.load_mod(path)
  }

  pub fn load_mod(&mut self, path: impl Into<PathBuf>) {
    let path = path.into();
    let module = Item {
      span: Span::NONE,
      kind: ItemKind::Mod(ModItem {
        name: self.auto_mod_name(&path),
        kind: self.load_file(path, Span::NONE),
      }),
    };
    self.root.push(module);
  }

  fn auto_mod_name(&self, path: &Path) -> Ident {
    Ident(
      self.interner.intern(str::from_utf8(path.file_stem().unwrap().as_encoded_bytes()).unwrap()),
    )
  }

  pub(crate) fn add_file(&mut self, name: String, src: &str) -> usize {
    let file = self.files.len();
    self.files.push(FileInfo::new(name, src));
    file
  }

  fn load_file(&mut self, path: PathBuf, span: Span) -> ModKind {
    match self._load_file(path, span) {
      Ok(items) => ModKind::Loaded(items),
      Err(diag) => ModKind::Error(self.diags.add(diag)),
    }
  }

  fn _load_file(&mut self, mut path: PathBuf, span: Span) -> Result<Vec<Item>, Diag> {
    let fs_err = |path: &mut PathBuf, err| {
      let path = take(path);
      Diag::FsError { span, path, err }
    };
    let src = fs::read_to_string(&path).map_err(|err| fs_err(&mut path, err))?;
    path = path.canonicalize().map_err(|err| fs_err(&mut path, err))?;
    let name = path.strip_prefix(&self.cwd).unwrap_or(&path).display().to_string();
    let file = self.add_file(name, &src);
    let mut items = VineParser::parse(self.interner, &src, file)?;
    path.pop();
    for item in &mut items {
      self.load_deps(&path, item);
    }
    Ok(items)
  }

  pub(crate) fn load_deps<'t>(&mut self, base: &Path, visitee: &'t mut impl Visitee<'t>) {
    LoadDeps { loader: self, base }.visit(visitee);
  }
}

struct LoadDeps<'a, 'ctx> {
  loader: &'a mut Loader<'ctx>,
  base: &'a Path,
}

impl VisitMut<'_> for LoadDeps<'_, '_> {
  fn visit_item(&mut self, item: &'_ mut Item) {
    if let ItemKind::Mod(module) = &mut item.kind {
      if let ModKind::Unloaded(path) = &mut module.kind {
        module.kind = self.loader.load_file(self.base.join(path), item.span);
        return;
      }
    }
    self._visit_item(item);
  }
}
