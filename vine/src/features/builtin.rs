use vine_util::idx::IntMap;

use crate::{
  components::{charter::Charter, parser::VineParser},
  structures::{
    ast::{BinaryOp, ComparisonOp},
    chart::{
      DefId, DefImplKind, DefTraitKind, DefTypeKind, DefValueKind, EnumId, FnId, ImplId,
      OpaqueTypeId, StructId, TraitId, WithVis,
    },
    diag::Diag,
  },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Builtin {
  Bool,
  N32,
  I32,
  F32,
  Char,
  IO,
  Prelude,
  List,
  String,
  Option,
  Result,
  BinaryOp(BinaryOp),
  Neg,
  Not,
  BoolNot,
  ComparisonOp(ComparisonOp),
  Cast,
  Fork,
  Drop,
  Duplicate,
  Erase,
  Range,
  BoundUnbounded,
  BoundInclusive,
  BoundExclusive,
  Advance,
  Tuple,
  Object,
}

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_builtin(&mut self) -> Result<Builtin, Diag<'core>> {
    let span = self.start_span();
    let builtin = self.parse_string()?;
    let span = self.end_span(span);
    Ok(match &*builtin {
      "Bool" => Builtin::Bool,
      "N32" => Builtin::N32,
      "I32" => Builtin::I32,
      "F32" => Builtin::F32,
      "Char" => Builtin::Char,
      "IO" => Builtin::IO,
      "List" => Builtin::List,
      "String" => Builtin::String,
      "Option" => Builtin::Option,
      "Result" => Builtin::Result,
      "prelude" => Builtin::Prelude,
      "neg" => Builtin::Neg,
      "not" => Builtin::Not,
      "bool_not" => Builtin::BoolNot,
      "cast" => Builtin::Cast,
      "add" => Builtin::BinaryOp(BinaryOp::Add),
      "sub" => Builtin::BinaryOp(BinaryOp::Sub),
      "mul" => Builtin::BinaryOp(BinaryOp::Mul),
      "div" => Builtin::BinaryOp(BinaryOp::Div),
      "rem" => Builtin::BinaryOp(BinaryOp::Rem),
      "and" => Builtin::BinaryOp(BinaryOp::BitAnd),
      "or" => Builtin::BinaryOp(BinaryOp::BitOr),
      "xor" => Builtin::BinaryOp(BinaryOp::BitXor),
      "shl" => Builtin::BinaryOp(BinaryOp::Shl),
      "shr" => Builtin::BinaryOp(BinaryOp::Shr),
      "concat" => Builtin::BinaryOp(BinaryOp::Concat),
      "pow" => Builtin::BinaryOp(BinaryOp::Pow),
      "eq" => Builtin::ComparisonOp(ComparisonOp::Eq),
      "ne" => Builtin::ComparisonOp(ComparisonOp::Ne),
      "lt" => Builtin::ComparisonOp(ComparisonOp::Lt),
      "gt" => Builtin::ComparisonOp(ComparisonOp::Gt),
      "le" => Builtin::ComparisonOp(ComparisonOp::Le),
      "ge" => Builtin::ComparisonOp(ComparisonOp::Ge),
      "Fork" => Builtin::Fork,
      "Drop" => Builtin::Drop,
      "duplicate" => Builtin::Duplicate,
      "erase" => Builtin::Erase,
      "Range" => Builtin::Range,
      "BoundUnbounded" => Builtin::BoundUnbounded,
      "BoundInclusive" => Builtin::BoundInclusive,
      "BoundExclusive" => Builtin::BoundExclusive,
      "advance" => Builtin::Advance,
      "Tuple" => Builtin::Tuple,
      "Object" => Builtin::Object,
      _ => Err(Diag::BadBuiltin { span })?,
    })
  }
}

#[derive(Debug, Default)]
pub struct Builtins {
  pub prelude: Option<DefId>,

  pub bool: Option<OpaqueTypeId>,
  pub n32: Option<OpaqueTypeId>,
  pub i32: Option<OpaqueTypeId>,
  pub f32: Option<OpaqueTypeId>,
  pub char: Option<OpaqueTypeId>,
  pub io: Option<OpaqueTypeId>,

  pub list: Option<StructId>,
  pub string: Option<StructId>,
  pub option: Option<EnumId>,
  pub result: Option<EnumId>,

  pub neg: Option<FnId>,
  pub not: Option<FnId>,
  pub cast: Option<FnId>,
  pub binary_ops: IntMap<BinaryOp, Option<FnId>>,
  pub comparison_ops: IntMap<ComparisonOp, Option<FnId>>,
  pub bool_not: Option<ImplId>,

  pub fork: Option<TraitId>,
  pub drop: Option<TraitId>,
  pub duplicate: Option<ImplId>,
  pub erase: Option<ImplId>,

  pub range: Option<StructId>,
  pub bound_exclusive: Option<StructId>,
  pub bound_inclusive: Option<StructId>,
  pub bound_unbounded: Option<StructId>,

  pub advance: Option<FnId>,

  pub tuple: Option<TraitId>,
  pub object: Option<TraitId>,
}

impl<'core> Charter<'core, '_> {
  pub(crate) fn chart_builtin(&mut self, def_id: Option<DefId>, builtin: Builtin) -> bool {
    let Some(def_id) = def_id else { return false };
    let def = &mut self.chart.defs[def_id];

    fn set<T>(builtin: &mut Option<T>, got: Option<T>) -> bool {
      if builtin.is_none() && got.is_some() {
        *builtin = got;
        true
      } else {
        false
      }
    }
    let opaque_type_id = match def.type_kind {
      Some(WithVis { kind: DefTypeKind::Opaque(id), .. }) => Some(id),
      _ => None,
    };
    let struct_id = match def.type_kind {
      Some(WithVis { kind: DefTypeKind::Struct(id), .. }) => Some(id),
      _ => None,
    };
    let enum_id = match def.type_kind {
      Some(WithVis { kind: DefTypeKind::Enum(id), .. }) => Some(id),
      _ => None,
    };
    let fn_id = match def.value_kind {
      Some(WithVis { kind: DefValueKind::Fn(kind), .. }) => Some(kind),
      _ => None,
    };
    let trait_id = match def.trait_kind {
      Some(WithVis { kind: DefTraitKind::Trait(id), .. }) => Some(id),
      _ => None,
    };
    let impl_id = match def.impl_kind {
      Some(WithVis { kind: DefImplKind::Impl(id), .. }) => Some(id),
      _ => None,
    };

    let builtins = &mut self.chart.builtins;
    match builtin {
      Builtin::Prelude => set(&mut builtins.prelude, Some(def_id)),
      Builtin::Bool => set(&mut builtins.bool, opaque_type_id),
      Builtin::N32 => set(&mut builtins.n32, opaque_type_id),
      Builtin::I32 => set(&mut builtins.i32, opaque_type_id),
      Builtin::F32 => set(&mut builtins.f32, opaque_type_id),
      Builtin::Char => set(&mut builtins.char, opaque_type_id),
      Builtin::IO => set(&mut builtins.io, opaque_type_id),
      Builtin::List => set(&mut builtins.list, struct_id),
      Builtin::String => set(&mut builtins.string, struct_id),
      Builtin::Option => set(&mut builtins.option, enum_id),
      Builtin::Result => set(&mut builtins.result, enum_id),
      Builtin::Neg => set(&mut builtins.neg, fn_id),
      Builtin::Not => set(&mut builtins.not, fn_id),
      Builtin::Cast => set(&mut builtins.cast, fn_id),
      Builtin::Fork => set(&mut builtins.fork, trait_id),
      Builtin::Drop => set(&mut builtins.drop, trait_id),
      Builtin::Duplicate => set(&mut builtins.duplicate, impl_id),
      Builtin::Erase => set(&mut builtins.erase, impl_id),
      Builtin::BoolNot => set(&mut builtins.bool_not, impl_id),
      Builtin::BinaryOp(op) => set(builtins.binary_ops.entry(op).or_default(), fn_id),
      Builtin::ComparisonOp(op) => set(builtins.comparison_ops.entry(op).or_default(), fn_id),
      Builtin::Range => set(&mut builtins.range, struct_id),
      Builtin::BoundUnbounded => set(&mut builtins.bound_unbounded, struct_id),
      Builtin::BoundExclusive => set(&mut builtins.bound_exclusive, struct_id),
      Builtin::BoundInclusive => set(&mut builtins.bound_inclusive, struct_id),
      Builtin::Advance => set(&mut builtins.advance, fn_id),
      Builtin::Tuple => set(&mut builtins.tuple, trait_id),
      Builtin::Object => set(&mut builtins.object, trait_id),
    }
  }
}
