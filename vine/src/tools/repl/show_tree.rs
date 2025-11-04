use ivy::ast::Tree;

use crate::structures::{
  chart::VariantId,
  types::{Type, TypeKind},
};

use super::Repl;

impl<'ctx, 'ivm, 'ext, 'comp> Repl<'ctx, 'ivm, 'ext, 'comp> {
  pub(super) fn show_tree(&mut self, ty: Type, tree: &Tree) -> String {
    self._show(ty, tree).unwrap_or_else(|| format!("#ivy({tree})"))
  }

  fn _show(&mut self, ty: Type, tree: &Tree) -> Option<String> {
    if let Tree::Global(g) = tree {
      return Some(g.clone());
    }
    let builtins = &self.compiler.chart.builtins;
    let (inv, kind) = self.types.kind(ty)?;
    if inv.0 {
      return None;
    }
    Some(match (kind, tree) {
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
      (TypeKind::Tuple(tys), _) if tys.len() == 1 => format!("({},)", self.show_tree(tys[0], tree)),
      (TypeKind::Tuple(tys), _) if !tys.is_empty() => {
        format!("({})", self.show_tuple(tys.clone(), tree)?.join(", "))
      }
      (TypeKind::Object(tys), _) if tys.is_empty() => "{}".into(),
      (TypeKind::Object(tys), _) => {
        let tys = tys.clone();
        let values = self.show_tuple(tys.values().copied(), tree)?;
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
            children.into_iter().map(|x| self.show_tree(arg, x)).collect::<Vec<_>>().join(", ")
          )
        }
      }
      (TypeKind::Struct(struct_id, args), tree) => {
        let name = self.compiler.chart.structs[*struct_id].name.clone();
        let args = args.clone();
        let data = self.types.import(&self.compiler.sigs.structs[*struct_id], Some(&args)).data;
        let data = self.show_tree(data, tree);
        if data.starts_with("(") {
          format!("{name}{data}")
        } else {
          format!("{name}({data})")
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
        let name = variant.name.clone();
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
          Some(self.show_tree(ty, l))
        } else {
          None
        };

        if tree != end {
          None?
        }
        if let Some(data) = data {
          if data.starts_with("(") {
            format!("{name}{data}")
          } else {
            format!("{name}({data})")
          }
        } else {
          name.to_string()
        }
      }
      (TypeKind::Ref(inner), Tree::Comb(label, value, space)) if label == "ref" => {
        let inner = *inner;
        format!("&({}; ~{})", self.show_tree(inner, value), self.show_tree(inner.inverse(), space))
      }
      (_, Tree::Erase) => "unsafe::eraser".into(),
      _ => None?,
    })
  }

  fn show_tuple(
    &mut self,
    tys: impl IntoIterator<Item = Type, IntoIter: DoubleEndedIterator>,
    tree: &Tree,
  ) -> Option<Vec<String>> {
    let mut tys = tys.into_iter();
    let mut tup = Vec::new();
    let mut tree = tree;
    let last = tys.next_back().unwrap();
    for ty in tys {
      let Tree::Comb(l, a, b) = tree else { None? };
      let "tup" = &**l else { None? };
      tup.push(self.show_tree(ty, a));
      tree = b;
    }
    tup.push(self.show_tree(last, tree));
    Some(tup)
  }
}
