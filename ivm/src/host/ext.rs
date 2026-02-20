use std::{
  any::TypeId,
  marker::PhantomData,
  mem::{ManuallyDrop, transmute_copy},
};

use ivy::name::{NameId, Table};
use vine_util::register::Register;

pub mod common;

use crate::{
  host::{
    Host,
    ext::common::{Pair, Ref},
  },
  runtime::{
    Runtime as Rt,
    ext::{Boxed, ExtFnMerge, ExtFnSplit, ExtTy, ExtTyCast, ExtVal},
    port::Port,
    wire::Wire,
  },
};

pub struct HostTable<'ivm, 'r> {
  host: &'r mut Host<'ivm>,
  table: &'r mut Table,
}

impl<'ivm> Host<'ivm> {
  pub fn register<'r>(
    &'r mut self,
    table: &'r mut Table,
    value: impl Register<HostTable<'ivm, 'r>>,
  ) {
    value.register(&mut HostTable { host: self, table });
  }

  pub fn get_ext_ty<T: ExtTyRegister<'ivm>>(&self) -> Option<ExtTy<'ivm, T>> {
    let id = *self.ext_types_lookup.get(&TypeId::of::<T::With<'static>>())?;
    Some(unsafe { ExtTy::new_unchecked(id) })
  }

  pub fn register_ext_ty<T: ExtTyRegister<'ivm>>(&mut self) -> ExtTy<'ivm, T> {
    let id = *self
      .ext_types_lookup
      .entry(TypeId::of::<T::With<'static>>())
      .or_insert_with(|| self.extrinsics.new_ext_ty::<T>().id());
    unsafe { ExtTy::new_unchecked(id) }
  }

  pub fn register_ext_fn<const I: usize, const O: usize, W>(
    &mut self,
    table: &mut Table,
    path: &str,
    f: impl ExtFn<'ivm, I, O, W>,
  ) {
    let name = table.add_path_name(path);
    let f = f.register(self, table);
    let pair = self.register_ext_ty();
    let ref_ = self.register_ext_ty();
    match (I, O) {
      (0, _) => unreachable!(),
      (2, 1) => {
        self.register_ext_merge(name, move |rt, a, b, c| f(rt, cast_array([a, b]), cast_array([c])))
      }
      (1, 2) => {
        self.register_ext_split(name, move |rt, a, b, c| f(rt, cast_array([a]), cast_array([b, c])))
      }
      (2.., 0 | 1) => self.register_ext_merge(name, move |rt, mut args, last, out| {
        let mut inputs = [const { None }; I];
        inputs[I - 1] = Some(last);
        for i in (1..(I - 1)).rev() {
          let Some(Pair(a, b)) = pair.unwrap(rt, args) else { return error(rt, [out]) };
          args = a;
          inputs[i] = Some(b);
        }
        inputs[0] = Some(args);
        let inputs = inputs.map(|x| x.unwrap());
        let outputs = if O == 1 {
          cast_array([out])
        } else {
          rt.link_wire(out, Port::ERASE);
          cast_array([])
        };
        f(rt, inputs, outputs);
      }),
      (1, _) | (_, 2..) => self.register_ext_split(name, move |rt, mut args, out1, out2| {
        let mut inputs = [const { None }; I];
        let mut outputs = [const { None }; O];
        match O {
          0 => {
            rt.link_wire(out1, Port::ERASE);
            rt.link_wire(out2, Port::ERASE);
          }
          1 => {
            outputs[0] = Some(out1);
            rt.link_wire(out2, Port::ERASE);
          }
          2.. => {
            outputs[O - 1] = Some(out2);
            outputs[O - 2] = Some(out1);
            for i in (0..(O - 2)).rev() {
              let Some(Ref(a, b)) = ref_.unwrap(rt, args) else {
                return error(rt, outputs.into_iter().flatten());
              };
              args = a;
              outputs[i] = Some(b);
            }
          }
        }
        let outputs = outputs.map(|x| x.unwrap());
        for i in (1..I).rev() {
          let Some(Pair(a, b)) = pair.unwrap(rt, args) else { return error(rt, outputs) };
          args = a;
          inputs[i] = Some(b);
        }
        inputs[0] = Some(args);
        let inputs = inputs.map(|x| x.unwrap());
        f(rt, inputs, outputs)
      }),
    }
  }

  fn register_ext_merge(&mut self, name: NameId, f: impl ExtFnMerge<'ivm>) {
    let f = self.extrinsics.new_ext_fn_merge(f);
    let old = self.ext_merge_lookup.insert(name, f);
    assert!(old.is_none());
  }

  fn register_ext_split(&mut self, name: NameId, f: impl ExtFnSplit<'ivm>) {
    let f = self.extrinsics.new_ext_fn_split(f);
    let old = self.ext_split_lookup.insert(name, f);
    assert!(old.is_none());
  }
}

#[allow(nonstandard_style)]
pub fn ExtFn<'ivm, 'r, const I: usize, const O: usize, W, F: ExtFn<'ivm, I, O, W>>(
  path: &str,
  f: F,
) -> impl Register<HostTable<'ivm, 'r>> {
  pub struct _ExtFn<'p, const I: usize, const O: usize, W, F> {
    path: &'p str,
    f: F,
    phantom: PhantomData<([(); I], [(); O], W)>,
  }

  impl<'r, 'p, 'ivm, const I: usize, const O: usize, W, F: ExtFn<'ivm, I, O, W>>
    Register<HostTable<'ivm, 'r>> for _ExtFn<'p, I, O, W, F>
  {
    fn register(self, registry: &mut HostTable<'ivm, 'r>) {
      registry.host.register_ext_fn(registry.table, self.path, self.f);
    }
  }

  _ExtFn { path, f, phantom: PhantomData }
}

pub trait Live<'ivm>: 'ivm + Send + Sync {}
impl<'ivm, T: 'ivm + Send + Sync> Live<'ivm> for T {}

pub trait ExtTyRegister<'ivm>: 'ivm + ExtTyCast<'ivm> {
  type With<'x>: ExtTyRegister<'x, With<'ivm> = Self>;
}

pub trait ExtTyBoxed<'ivm>: 'ivm {
  type With<'x>: ExtTyBoxed<'x, With<'ivm> = Self>;
}

impl<'ivm, T: ExtTyBoxed<'ivm>> ExtTyRegister<'ivm> for Boxed<T> {
  type With<'x> = Boxed<T::With<'x>>;
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Invalid;

pub trait ExtInput<'ivm, W>: Sized {
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm, Self, W> + Live<'ivm> + Fn(&mut Rt<'ivm, '_>, ExtVal<'ivm>) -> Result<Self, Invalid>;
}

pub trait ExtOutput<'ivm, W>: Sized {
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm, Self, W> + Live<'ivm> + Fn(&mut Rt<'ivm, '_>, Self) -> ExtVal<'ivm>;
}

pub struct FromRegister;

impl<'ivm, T: ExtTyRegister<'ivm>> ExtInput<'ivm, FromRegister> for T {
  fn register(
    host: &mut Host<'ivm>,
    _: &mut Table,
  ) -> impl use<'ivm, T> + Live<'ivm> + Fn(&mut Rt<'ivm, '_>, ExtVal<'ivm>) -> Result<Self, Invalid>
  {
    let ty = host.register_ext_ty();
    move |rt, value| ty.unwrap(rt, value).ok_or(Invalid)
  }
}

impl<'ivm, T: ExtTyRegister<'ivm>> ExtOutput<'ivm, FromRegister> for T {
  fn register(
    host: &mut Host<'ivm>,
    _: &mut Table,
  ) -> impl use<'ivm, T> + Live<'ivm> + Fn(&mut Rt<'ivm, '_>, Self) -> ExtVal<'ivm> {
    let ty = host.register_ext_ty();
    move |rt, value| ty.wrap(rt, value)
  }
}

pub struct FromBoxed;

impl<'ivm, T: ExtTyBoxed<'ivm>> ExtInput<'ivm, FromBoxed> for T {
  fn register(
    host: &mut Host<'ivm>,
    _: &mut Table,
  ) -> impl use<'ivm, T> + Live<'ivm> + Fn(&mut Rt<'ivm, '_>, ExtVal<'ivm>) -> Result<Self, Invalid>
  {
    let ty = host.register_ext_ty::<Boxed<T>>();
    move |rt, value| Ok(ty.unwrap(rt, value).ok_or(Invalid)?.into_inner())
  }
}

impl<'ivm, T: ExtTyBoxed<'ivm>> ExtOutput<'ivm, FromBoxed> for T {
  fn register(
    host: &mut Host<'ivm>,
    _: &mut Table,
  ) -> impl use<'ivm, T> + Live<'ivm> + Fn(&mut Rt<'ivm, '_>, Self) -> ExtVal<'ivm> {
    let ty = host.register_ext_ty();
    move |rt, value| ty.wrap(rt, Boxed::new(value))
  }
}

impl<'ivm> ExtInput<'ivm, ()> for ExtVal<'ivm> {
  fn register(
    _: &mut Host<'ivm>,
    _: &mut Table,
  ) -> impl use<'ivm> + Live<'ivm> + Fn(&mut Rt<'ivm, '_>, ExtVal<'ivm>) -> Result<Self, Invalid>
  {
    |_, value| Ok(value)
  }
}

impl<'ivm> ExtOutput<'ivm, ()> for ExtVal<'ivm> {
  fn register(
    _: &mut Host<'ivm>,
    _: &mut Table,
  ) -> impl use<'ivm> + Live<'ivm> + Fn(&mut Rt<'ivm, '_>, Self) -> ExtVal<'ivm> {
    |_, value| value
  }
}

impl<'ivm, const N: usize, T: ExtOutputs<'ivm, N, W>, W> ExtOutputs<'ivm, N, Result<W, Invalid>>
  for Result<T, Invalid>
{
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm, N, T, W> + Live<'ivm> + Fn(&mut Rt<'ivm, '_>, Self, [Wire<'ivm>; N]) {
    let handle_outputs = T::register(host, table);
    move |rt, result, wires| {
      let Ok(outputs) = result else { return error(rt, wires) };
      handle_outputs(rt, outputs, wires);
    }
  }
}

pub trait ExtFn<'ivm, const I: usize, const O: usize, W> {
  fn register(
    self,
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm, Self, I, O, W>
  + Live<'ivm>
  + Fn(&mut Rt<'ivm, '_>, [ExtVal<'ivm>; I], [Wire<'ivm>; O]);
}

pub struct ExtFnManual;

impl<
  'ivm,
  const I: usize,
  const O: usize,
  F: Live<'ivm> + Fn(&mut Rt<'ivm, '_>, [ExtVal<'ivm>; I], [Wire<'ivm>; O]),
> ExtFn<'ivm, I, O, ExtFnManual> for F
{
  fn register(
    self,
    _: &mut Host<'ivm>,
    _: &mut Table,
  ) -> impl use<'ivm, F, I, O> + Live<'ivm> + Fn(&mut Rt<'ivm, '_>, [ExtVal<'ivm>; I], [Wire<'ivm>; O])
  {
    self
  }
}

pub struct ExtFnIn<I, IW>(PhantomData<(I, IW)>);

impl<
  'ivm,
  const IN: usize,
  const ON: usize,
  I: ExtInputs<'ivm, IN, IW>,
  IW,
  F: Live<'ivm> + Fn(&mut Rt<'ivm, '_>, I, [Wire<'ivm>; ON]),
> ExtFn<'ivm, IN, ON, ExtFnIn<I, IW>> for F
{
  fn register(
    self,
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm, IN, ON, I, F, IW>
  + Live<'ivm>
  + Fn(&mut Rt<'ivm, '_>, [ExtVal<'ivm>; IN], [Wire<'ivm>; ON]) {
    let handle_inputs = I::register(host, table);
    move |rt, inputs, wires| {
      let Ok(inputs) = handle_inputs(rt, inputs) else { return error(rt, wires) };
      self(rt, inputs, wires);
    }
  }
}

pub struct ExtFnRtInOut<I, IW, O, OW>(PhantomData<(I, IW, O, OW)>);

impl<
  'ivm,
  const IN: usize,
  const ON: usize,
  I: ExtInputs<'ivm, IN, IW>,
  IW,
  O: ExtOutputs<'ivm, ON, OW>,
  OW,
  F: Live<'ivm> + Fn(&mut Rt<'ivm, '_>, I) -> O,
> ExtFn<'ivm, IN, ON, ExtFnRtInOut<I, IW, O, OW>> for F
{
  fn register(
    self,
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm, IN, ON, I, O, F, IW, OW>
  + Live<'ivm>
  + Fn(&mut Rt<'ivm, '_>, [ExtVal<'ivm>; IN], [Wire<'ivm>; ON]) {
    let handle_inputs = I::register(host, table);
    let handle_outputs = O::register(host, table);
    move |rt, inputs, wires| {
      let Ok(inputs) = handle_inputs(rt, inputs) else { return error(rt, wires) };
      let outputs = self(rt, inputs);
      handle_outputs(rt, outputs, wires);
    }
  }
}

pub struct ExtFnInOut<I, IW, O, OW>(PhantomData<(I, IW, O, OW)>);

impl<
  'ivm,
  const N: usize,
  const M: usize,
  I: ExtInputs<'ivm, N, IW>,
  IW,
  O: ExtOutputs<'ivm, M, OW>,
  OW,
  F: Live<'ivm> + Fn(I) -> O,
> ExtFn<'ivm, N, M, ExtFnInOut<I, IW, O, OW>> for F
{
  fn register(
    self,
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm, N, M, I, O, F, IW, OW>
  + Live<'ivm>
  + Fn(&mut Rt<'ivm, '_>, [ExtVal<'ivm>; N], [Wire<'ivm>; M]) {
    let handle_inputs = I::register(host, table);
    let handle_outputs = O::register(host, table);
    move |rt, inputs, wires| {
      let Ok(inputs) = handle_inputs(rt, inputs) else { return error(rt, wires) };
      let outputs = self(inputs);
      handle_outputs(rt, outputs, wires);
    }
  }
}

pub struct WithHost<W>(PhantomData<W>);

impl<
  'ivm,
  const I: usize,
  const O: usize,
  F: FnOnce(&mut Host<'ivm>, &mut Table) -> G,
  G: ExtFn<'ivm, I, O, W>,
  W,
> ExtFn<'ivm, I, O, WithHost<W>> for F
{
  fn register(
    self,
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm, I, O, F, G, W>
  + Live<'ivm>
  + Fn(&mut Rt<'ivm, '_>, [ExtVal<'ivm>; I], [Wire<'ivm>; O]) {
    self(host, table).register(host, table)
  }
}

pub trait ExtInputs<'ivm, const N: usize, W>: Sized {
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm, N, Self, W>
  + Live<'ivm>
  + Fn(&mut Rt<'ivm, '_>, [ExtVal<'ivm>; N]) -> Result<Self, Invalid>;
}

pub trait ExtOutputs<'ivm, const N: usize, W>: Sized {
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm, N, Self, W> + Live<'ivm> + Fn(&mut Rt<'ivm, '_>, Self, [Wire<'ivm>; N]);
}

macro_rules! impl_variadic {
  ($N:literal $($t:ident $t_:ident $T:ident $T_:ident)*) => {
    #[allow(warnings)]
    impl<'ivm, $($T: ExtInput<'ivm, $T_>, $T_,)*> ExtInputs<'ivm, $N, Compound<($($T_),*)>> for ($($T),*) {
      fn register(
        host: &mut Host<'ivm>,
        table: &mut Table,
      ) -> impl use<'ivm, $($T, $T_,)*> + Live<'ivm> + Fn(&mut Rt<'ivm, '_>, [ExtVal<'ivm>; $N]) -> Result<Self, Invalid> {
        $(let $T = $T::register(host, table);)*
        move |rt, [$($t),*]| {
          $(let $t = $T(rt, $t)?;)*
          Ok(($($t),*))
        }
      }
    }

    #[allow(warnings)]
    impl<'ivm, $($T: ExtOutput<'ivm, $T_>, $T_,)*> ExtOutputs<'ivm, $N, Compound<($($T_),*)>> for ($($T),*) {
      fn register(
        host: &mut Host<'ivm>,
        table: &mut Table,
      ) -> impl use<'ivm, $($T, $T_,)*> + Live<'ivm> + Fn(&mut Rt<'ivm, '_>, Self, [Wire<'ivm>; $N]) {
        $(let $T = $T::register(host, table);)*
        move |rt, ($($t),*), [$($t_),*]| {
          $(let $t = $T(rt, $t);)*
          $(rt.link_wire($t_, Port::new_ext_val($t));)*
        }
      }
    }
  };
}

pub struct Compound<T>(PhantomData<T>);

impl_variadic!(0);
impl_variadic!(1 a a_ A A_);
impl_variadic!(2 a a_ A A_ b b_ B B_);
impl_variadic!(3 a a_ A A_ b b_ B B_ c c_ C C_);
impl_variadic!(4 a a_ A A_ b b_ B B_ c c_ C C_ d d_ D D_);
impl_variadic!(5 a a_ A A_ b b_ B B_ c c_ C C_ d d_ D D_ e e_ E E_);
impl_variadic!(6 a a_ A A_ b b_ B B_ c c_ C C_ d d_ D D_ e e_ E E_ f f_ F F_);
impl_variadic!(7 a a_ A A_ b b_ B B_ c c_ C C_ d d_ D D_ e e_ E E_ f f_ F F_ g g_ G G_);

fn cast_array<T, const N: usize, const M: usize>(arr: [T; N]) -> [T; M] {
  assert_eq!(N, M);
  unsafe { transmute_copy(&ManuallyDrop::new(arr)) }
}

pub fn error<'ivm>(rt: &mut Rt<'ivm, '_>, wires: impl IntoIterator<Item = Wire<'ivm>>) {
  wires.into_iter().for_each(|w| rt.link_wire(w, Port::ERASE));
  rt.flags.ext_generic = true;
}
