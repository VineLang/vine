use vine_util::idx::IntMap;

use crate::{
  components::charter::{ChartedItem, Charter},
  structures::{
    ast::{BinaryOp, ComparisonOp},
    chart::{DefId, EnumId, FnId, ImplId, OpaqueTypeId, StructId, TraitId},
  },
};

#[derive(Debug, Default)]
pub struct Builtins {
  pub prelude: Option<DefId>,

  pub bool: Option<OpaqueTypeId>,
  pub n32: Option<OpaqueTypeId>,
  pub i32: Option<OpaqueTypeId>,
  pub f32: Option<OpaqueTypeId>,
  pub f64: Option<OpaqueTypeId>,
  pub nat: Option<StructId>,
  pub char: Option<OpaqueTypeId>,
  pub io: Option<OpaqueTypeId>,

  pub list: Option<StructId>,
  pub string: Option<StructId>,
  pub option: Option<EnumId>,
  pub result: Option<EnumId>,

  pub pos: Option<FnId>,
  pub neg: Option<FnId>,
  pub not: Option<FnId>,
  pub cast: Option<FnId>,
  pub binary_ops: IntMap<BinaryOp, Option<FnId>>,
  pub comparison_ops: IntMap<ComparisonOp, Option<FnId>>,
  pub bool_not: Option<ImplId>,

  pub fn_: Option<TraitId>,
  pub fork: Option<TraitId>,
  pub drop: Option<TraitId>,
  pub duplicate: Option<ImplId>,
  pub erase: Option<ImplId>,

  pub index: Option<TraitId>,
  pub index_value: Option<TraitId>,
  pub index_space: Option<TraitId>,
  pub index_place: Option<TraitId>,

  pub range: Option<StructId>,
  pub bound_exclusive: Option<StructId>,
  pub bound_inclusive: Option<StructId>,
  pub bound_unbounded: Option<StructId>,

  pub advance: Option<FnId>,
  pub iter: Option<FnId>,

  pub tuple: Option<TraitId>,
  pub object: Option<TraitId>,
  pub struct_: Option<TraitId>,
  pub enum_: Option<TraitId>,
  pub variant: Option<EnumId>,
  pub if_const: Option<TraitId>,
  pub opaque: Option<TraitId>,
  pub default: Option<TraitId>,

  pub debug_state: Option<FnId>,

  pub show: Option<TraitId>,
  pub repl_show: Option<FnId>,
}

impl Charter<'_> {
  pub(crate) fn chart_builtin(&mut self, item: ChartedItem, builtin: String) -> bool {
    fn set<T>(builtin: &mut Option<T>, got: Option<T>) -> bool {
      if builtin.is_none() && got.is_some() {
        *builtin = got;
        true
      } else {
        false
      }
    }

    fn binary_op(builtins: &mut Builtins, op: BinaryOp, fn_id: Option<FnId>) -> bool {
      set(builtins.binary_ops.entry(op).or_default(), fn_id)
    }

    fn comparison_op(builtins: &mut Builtins, op: ComparisonOp, fn_id: Option<FnId>) -> bool {
      set(builtins.comparison_ops.entry(op).or_default(), fn_id)
    }

    let opaque_type_id = if let ChartedItem::OpaqueType(_, id) = item { Some(id) } else { None };
    let struct_id = if let ChartedItem::Struct(_, id) = item { Some(id) } else { None };
    let enum_id = if let ChartedItem::Enum(_, id) = item { Some(id) } else { None };
    let fn_id = if let ChartedItem::Fn(_, id) = item { Some(id) } else { None };
    let trait_id = if let ChartedItem::Trait(_, id) = item { Some(id) } else { None };
    let impl_id = if let ChartedItem::Impl(_, id) = item { Some(id) } else { None };

    let b = &mut self.chart.builtins;
    match &*builtin {
      "prelude" => set(&mut b.prelude, item.def()),
      "Bool" => set(&mut b.bool, opaque_type_id),
      "N32" => set(&mut b.n32, opaque_type_id),
      "I32" => set(&mut b.i32, opaque_type_id),
      "F32" => set(&mut b.f32, opaque_type_id),
      "F64" => set(&mut b.f64, opaque_type_id),
      "Nat" => set(&mut b.nat, struct_id),
      "Char" => set(&mut b.char, opaque_type_id),
      "IO" => set(&mut b.io, opaque_type_id),
      "List" => set(&mut b.list, struct_id),
      "String" => set(&mut b.string, struct_id),
      "Option" => set(&mut b.option, enum_id),
      "Result" => set(&mut b.result, enum_id),
      "pos" => set(&mut b.pos, fn_id),
      "neg" => set(&mut b.neg, fn_id),
      "not" => set(&mut b.not, fn_id),
      "cast" => set(&mut b.cast, fn_id),
      "Fn" => set(&mut b.fn_, trait_id),
      "Fork" => set(&mut b.fork, trait_id),
      "Drop" => set(&mut b.drop, trait_id),
      "duplicate" => set(&mut b.duplicate, impl_id),
      "erase" => set(&mut b.erase, impl_id),
      "bool_not" => set(&mut b.bool_not, impl_id),
      "Index" => set(&mut b.index, trait_id),
      "IndexValue" => set(&mut b.index_value, trait_id),
      "IndexSpace" => set(&mut b.index_space, trait_id),
      "IndexPlace" => set(&mut b.index_place, trait_id),
      "Range" => set(&mut b.range, struct_id),
      "BoundUnbounded" => set(&mut b.bound_unbounded, struct_id),
      "BoundExclusive" => set(&mut b.bound_exclusive, struct_id),
      "BoundInclusive" => set(&mut b.bound_inclusive, struct_id),
      "advance" => set(&mut b.advance, fn_id),
      "iter" => set(&mut b.iter, fn_id),
      "Tuple" => set(&mut b.tuple, trait_id),
      "Object" => set(&mut b.object, trait_id),
      "Struct" => set(&mut b.struct_, trait_id),
      "Enum" => set(&mut b.enum_, trait_id),
      "Variant" => set(&mut b.variant, enum_id),
      "IfConst" => set(&mut b.if_const, trait_id),
      "Opaque" => set(&mut b.opaque, trait_id),
      "Default" => set(&mut b.default, trait_id),
      "debug_state" => set(&mut b.debug_state, fn_id),
      "Show" => set(&mut b.show, trait_id),
      "repl_show" => set(&mut b.repl_show, fn_id),
      "add" => binary_op(b, BinaryOp::Add, fn_id),
      "sub" => binary_op(b, BinaryOp::Sub, fn_id),
      "mul" => binary_op(b, BinaryOp::Mul, fn_id),
      "div" => binary_op(b, BinaryOp::Div, fn_id),
      "rem" => binary_op(b, BinaryOp::Rem, fn_id),
      "and" => binary_op(b, BinaryOp::BitAnd, fn_id),
      "or" => binary_op(b, BinaryOp::BitOr, fn_id),
      "xor" => binary_op(b, BinaryOp::BitXor, fn_id),
      "shl" => binary_op(b, BinaryOp::Shl, fn_id),
      "shr" => binary_op(b, BinaryOp::Shr, fn_id),
      "concat" => binary_op(b, BinaryOp::Concat, fn_id),
      "pow" => binary_op(b, BinaryOp::Pow, fn_id),
      "eq" => comparison_op(b, ComparisonOp::Eq, fn_id),
      "ne" => comparison_op(b, ComparisonOp::Ne, fn_id),
      "lt" => comparison_op(b, ComparisonOp::Lt, fn_id),
      "gt" => comparison_op(b, ComparisonOp::Gt, fn_id),
      "le" => comparison_op(b, ComparisonOp::Le, fn_id),
      "ge" => comparison_op(b, ComparisonOp::Ge, fn_id),
      _ => false,
    }
  }
}
