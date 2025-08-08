use std::mem::take;

use ivy::ast::Tree;
use vine_util::parser::Parser;

use crate::{
  components::{
    charter::Charter, distiller::Distiller, emitter::Emitter, lexer::Token, parser::VineParser,
    resolver::Resolver,
  },
  structures::{
    ast::{ConstItem, Path, Span},
    chart::{ConcreteConstDef, ConcreteConstId, ConstId, DefId, DefValueKind, GenericsId},
    diag::Diag,
    resolutions::ConstRelId,
    signatures::ConstSig,
    tir::{TirExpr, TirExprKind},
    types::{Type, TypeCtx},
    vir::{Port, Stage, Step},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_const_item(&mut self) -> Result<ConstItem<'core>, Diag<'core>> {
    self.expect(Token::Const)?;
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    self.expect(Token::Colon)?;
    let ty = self.parse_ty()?;
    let value = self.eat_then(Token::Eq, Self::parse_expr)?;
    self.expect(Token::Semi)?;
    Ok(ConstItem { name, generics, ty, value })
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_const_item(&self, c: &ConstItem<'core>) -> Doc<'src> {
    Doc::concat([
      Doc("const "),
      Doc(c.name),
      self.fmt_generic_params(&c.generics),
      Doc(": "),
      self.fmt_ty(&c.ty),
      match &c.value {
        Some(v) => Doc::concat([Doc(" = "), self.fmt_expr(v)]),
        None => Doc(""),
      },
      Doc(";"),
    ])
  }
}

impl<'core> Charter<'core, '_> {
  pub(crate) fn chart_const(
    &mut self,
    parent: DefId,
    parent_generics: GenericsId,
    span: Span,
    vis: DefId,
    member_vis: DefId,
    const_item: ConstItem<'core>,
  ) -> DefId {
    let def = self.chart_child(parent, const_item.name, member_vis, true);
    let generics = self.chart_generics(def, parent_generics, const_item.generics, true);
    let value = self.ensure_implemented(span, const_item.value);
    let const_id = self.chart.concrete_consts.push(ConcreteConstDef {
      span,
      def,
      generics,
      ty: const_item.ty,
      value,
    });
    self.define_value(span, def, vis, DefValueKind::Const(ConstId::Concrete(const_id)));
    def
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_const_sig(&mut self, const_id: ConcreteConstId) {
    let const_def = &self.chart.concrete_consts[const_id];
    self.initialize(const_def.def, const_def.generics);
    let ty = self.resolve_ty(&const_def.ty, false);
    let types = take(&mut self.types);
    self.sigs.concrete_consts.push_to(const_id, TypeCtx { types, inner: ConstSig { ty } });
  }

  pub(crate) fn resolve_const_def(&mut self, const_id: ConcreteConstId) {
    let const_def = &self.chart.concrete_consts[const_id];
    self.initialize(const_def.def, const_def.generics);
    let ty = self.types.import(&self.sigs.concrete_consts[const_id], None).ty;
    let root = self.resolve_expr_type(&const_def.value, ty);
    let fragment =
      self.finish_fragment(const_def.span, self.chart.defs[const_def.def].path, root, false);
    let fragment_id = self.fragments.push(fragment);
    self.resolutions.consts.push_to(const_id, fragment_id);
  }

  pub(crate) fn resolve_expr_path_const(
    &mut self,
    span: Span,
    path: &Path<'core>,
    const_id: ConstId,
  ) -> Result<TirExpr, Diag<'core>> {
    let (type_params, impl_params) =
      self.resolve_generics(path, self.chart.const_generics(const_id), true);
    let ty = self.types.import(self.sigs.const_sig(const_id), Some(&type_params)).ty;
    let rel = self.rels.consts.push((const_id, impl_params));
    Ok(TirExpr::new(span, ty, TirExprKind::Const(rel)))
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_expr_value_const(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    id: ConstRelId,
  ) -> Port {
    let wire = stage.new_wire(span, ty);
    stage.steps.push(Step::Const(span, id, wire.neg));
    wire.pos
  }
}

impl<'core> Emitter<'core, '_> {
  pub(crate) fn emit_const(&mut self, span: &Span, rel: &ConstRelId, out: &Port) {
    let rel = self.emit_const_rel(*rel);
    let out = self.emit_port(out);
    if self.core.debug {
      let dbg = self.tap_debug_call(*span);
      self.pairs.push((rel, Tree::n_ary("dbg", [dbg, out])));
    } else {
      self.pairs.push((rel, out));
    }
  }
}
