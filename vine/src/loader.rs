use std::{
  fs,
  path::{Path, PathBuf},
  str,
};

use vine_util::interner::StringInterner;

use crate::{
  ast::{self, ConstItem, Ident, Item, ItemKind, ModItem, ModKind, Term},
  parser::VineParser,
};

pub struct Loader<'ctx> {
  interner: &'ctx StringInterner<'static>,
  root: Vec<Item>,
}

impl<'ctx> Loader<'ctx> {
  pub fn new(interner: &'ctx StringInterner<'static>) -> Self {
    Self { interner, root: Vec::new() }
  }

  pub fn finish(self) -> ModKind {
    ModKind::Loaded(self.root)
  }

  pub fn load_main_mod(&mut self, path: impl Into<PathBuf>) {
    let path = path.into();
    let main = Ident(self.interner.intern("main"));
    self.root.push(Item {
      kind: ItemKind::Const(ConstItem {
        name: main,
        value: Term::new_path(ast::Path {
          segments: vec![self.auto_mod_name(&path), main],
          absolute: true,
          resolved: None,
        }),
      }),
    });
    self.load_mod(path)
  }

  pub fn load_mod(&mut self, path: impl Into<PathBuf>) {
    let path = path.into();
    let module = Item {
      kind: ItemKind::Mod(ModItem {
        name: self.auto_mod_name(&path),
        kind: ModKind::Loaded(self.load_file(path)),
      }),
    };
    self.root.push(module);
  }

  fn auto_mod_name(&self, path: &Path) -> Ident {
    Ident(
      self.interner.intern(str::from_utf8(path.file_stem().unwrap().as_encoded_bytes()).unwrap()),
    )
  }

  fn load_file(&mut self, mut path: PathBuf) -> Vec<Item> {
    let src = fs::read_to_string(&path).unwrap();
    let mut items = VineParser::parse(self.interner, &src).unwrap();
    path.pop();
    for item in &mut items {
      self.load_item(&path, item);
    }
    items
  }

  fn load_item(&mut self, base: &Path, item: &mut Item) {
    if let ItemKind::Mod(module) = &mut item.kind {
      match &mut module.kind {
        ModKind::Unloaded(path) => {
          module.kind = ModKind::Loaded(self.load_file(base.join(path)));
        }
        ModKind::Loaded(items) => {
          for item in items {
            self.load_item(base, item);
          }
        }
      }
    }
  }
}
