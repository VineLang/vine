use std::{fmt::Write, fs, path::Path};

use crate::{
  compiler::Compiler,
  components::loader::FileId,
  structures::{
    ast::Span,
    chart::{DefId, MemberKind, VisId},
  },
};

#[allow(warnings)]
pub fn document(compiler: &Compiler, path: &Path) -> Result<(), ()> {
  let Some(id) = compiler.chart.main_mod else { return Err(()) };
  let documenter = Documenter { compiler };
  let tree = documenter.build_tree(id);
  documenter.output_tree(&tree, path, None);
  Ok(())
}

struct Documenter<'a> {
  compiler: &'a Compiler,
}

struct Tree<'a> {
  id: String,
  src_file: Option<FileId>,
  src: Option<Span>,
  name: &'a str,
  path: &'a str,
  sigs: Vec<&'a String>,
  docs: Vec<&'a String>,
  children: Vec<Tree<'a>>,
}

impl<'a> Documenter<'a> {
  fn build_tree(&self, id: DefId) -> Tree<'a> {
    let def = &self.compiler.chart.defs[id];
    let mut tree = Tree {
      id: def.path.strip_prefix("#").unwrap().split("::").collect::<Vec<_>>().join("-"),
      src_file: def.file,
      src: def.spans.first().copied(),
      name: &def.name.0,
      path: &def.path,
      sigs: Vec::new(),
      docs: Vec::new(),
      children: Vec::new(),
    };
    for span in &def.spans {
      if let Some(hover) = self.compiler.annotations.hovers.get(span) {
        tree.sigs.extend(&hover.signatures);
        tree.docs.extend(&hover.docs);
      }
    }
    for member in &def.named_members {
      if matches!(member.vis, VisId::Pub)
        && let MemberKind::Child(child) = member.kind
      {
        tree.children.push(self.build_tree(child));
      }
    }
    tree
  }

  fn output_tree(&self, tree: &Tree, path: &Path, parent: Option<(&str, &str)>) -> String {
    let id = &tree.id;
    let name = tree.name;
    let mut str = "#import \"/lib.typ\": *\n\n".to_owned();

    let src = self.source_link(tree);
    if let Some((parent_id, parent_path)) = parent {
      writeln!(str, "#let title = `{name}`").unwrap();
      writeln!(str, "=== @{parent_id}[`{parent_path}`]").unwrap();
    } else {
      writeln!(str, "#let title = `#{name}`").unwrap();
    }
    writeln!(str, "= #title{src} <{id}>\n").unwrap();

    self.write_header(tree, &mut str);

    let mut child_paths = Vec::new();
    let dir = path.join(name);
    for child in &tree.children {
      if child.children.is_empty() && child.sigs.is_empty() && child.docs.is_empty() {
        continue;
      }
      let src = self.source_link(child);
      if child.children.is_empty() && child.src_file.is_none() {
        writeln!(str, "\n== #link(\"#{}\")[`{}`]{src} <{}>\n", child.id, child.name, child.id)
          .unwrap();
      } else {
        child_paths.push(self.output_tree(child, &dir, Some((id, tree.path))));
        writeln!(str, "\n== @{}[`{}`]{src}\n", child.id, child.name).unwrap();
      }
      self.write_header(child, &mut str);
    }

    if !child_paths.is_empty() {
      str += "#let children = (\n";
      for path in &child_paths {
        writeln!(str, "  \"{path}\",").unwrap();
      }
      str += ")";
    }

    let file =
      if child_paths.is_empty() { format!("{name}.typ",) } else { format!("{name}/{name}.typ",) };

    let path = path.join(&file);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, str).unwrap();

    file
  }

  fn source_link(&self, tree: &Tree) -> String {
    if let Some(file) = tree.src_file {
      let file = &self.compiler.loader.files[file].name;
      format!(" #src(\"{}\")", file)
    } else if let Some(span) = tree.src {
      let pos = self.compiler.loader.files[span.file].get_pos(span.start);
      format!(" #src(\"{}\", line: {})", pos.file, pos.line + 1)
    } else {
      String::new()
    }
  }

  fn write_header(&self, tree: &Tree, str: &mut String) {
    if !tree.sigs.is_empty() {
      *str += "```vi\n";
      for sig in &tree.sigs {
        *str += sig;
        *str += "\n";
      }
      *str += "```\n";
    }

    for doc in &tree.docs {
      let doc = doc.strip_prefix("///").unwrap();
      let doc = doc.strip_prefix(" ").unwrap_or(doc);
      *str += doc;
      *str += "\n";
    }
  }
}
