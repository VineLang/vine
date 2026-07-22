use core::slice;
use std::{
  collections::{HashMap, hash_map::Entry},
  hash::Hash,
};

use vine_util::{idx::Counter, nat::Nat};

use crate::{
  name::{Name, NameId, PathId, Table},
  net::{FlatNet, FlatNode, Wire, WireLinks},
};

pub const MAGIC_NUMBER: &[u8; 4] = b"\0.iv";
const VERSION: usize = 0;

pub fn is_binary(data: &[u8]) -> bool {
  data.starts_with(MAGIC_NUMBER)
}

pub fn encode_binary(table: &Table, nets: &mut HashMap<NameId, FlatNet>) -> Vec<u8> {
  let mut encoder = Encoder {
    table,
    path_ids: Default::default(),
    name_ids: Default::default(),
    wires: Default::default(),
    output: Default::default(),
  };
  encoder.output.extend_from_slice(MAGIC_NUMBER);
  encoder.write(VERSION);
  encoder.encode_nets(nets);
  encoder.output
}

pub fn decode_binary(
  table: &mut Table,
  data: &[u8],
) -> Result<HashMap<NameId, FlatNet>, DecodeError> {
  if !data.starts_with(MAGIC_NUMBER) {
    Err(DecodeError::InvalidHeader)?;
  }
  let mut decoder = Decoder {
    table,
    path_ids: Default::default(),
    name_ids: Default::default(),
    wires: Default::default(),
    input: data[MAGIC_NUMBER.len()..].iter(),
  };
  if decoder.read()? != VERSION {
    Err(DecodeError::InvalidHeader)?;
  }
  let nets = decoder.decode_nets()?;
  if decoder.input.next().is_some() {
    Err(DecodeError::ExpectedEof)?;
  }
  Ok(nets)
}

struct Encoder<'a> {
  table: &'a Table,
  path_ids: EncodeIds<PathId>,
  name_ids: EncodeIds<NameId>,
  wires: EncodeIds<Wire>,
  output: Vec<u8>,
}

impl Encoder<'_> {
  fn encode_nets(&mut self, nets: &mut HashMap<NameId, FlatNet>) {
    let mut nets = nets.iter_mut().map(|(&name, net)| (name, net)).collect::<Vec<_>>();
    nets.sort_by_key(|&(name, _)| name);
    self.write(nets.len());
    for (name, net) in nets {
      self.encode_name_id(name);
      self.encode_net(net);
    }
  }

  fn encode_net(&mut self, net: &mut FlatNet) {
    net.resolve_links();
    self.write(net.free.len());
    for &wire in &net.free {
      self.encode_wire(wire);
    }
    self.write(net.nodes.len());
    for node in &net.nodes {
      self.encode_node(node);
    }
    self.wires.clear();
  }

  fn encode_node(&mut self, node: &FlatNode) {
    self.encode_name(&node.name);
    self.encode_wire(node.pri);
    self.write(node.aux.len());
    for &aux in &node.aux {
      self.encode_wire(aux);
    }
  }

  fn encode_wire(&mut self, wire: Wire) {
    let id = self.wires.get(wire).0;
    self.write(id);
  }

  fn encode_name_id(&mut self, name_id: NameId) {
    let (id, fresh) = self.name_ids.get(name_id);
    self.write(id);
    if fresh {
      self.encode_name(self.table.name(name_id));
    }
  }

  fn encode_name(&mut self, name: &Name) {
    self.encode_path(name.path);
    self.write(name.children.len());
    for &child in &name.children {
      self.encode_name_id(child);
    }
    self.write_nat(&name.payload);
  }

  fn encode_path(&mut self, path_id: PathId) {
    let (id, fresh) = self.path_ids.get(path_id);
    self.write(id);
    if fresh {
      let path = self.table.path(path_id);
      self.write(path.len());
      self.output.extend_from_slice(path.as_bytes());
    }
  }

  fn write(&mut self, mut word: usize) {
    while word >= 0x80 {
      self.output.push(word as u8 | 0x80);
      word >>= 7;
    }
    self.output.push(word as u8);
  }

  fn write_nat(&mut self, nat: &Nat) {
    let mut bits = 0;
    let mut data = 0u64;
    for part in &nat.0 {
      while bits >= 7 {
        self.output.push(data as u8 | 0x80);
        bits -= 7;
        data >>= 7;
      }
      data |= (*part as u64) << bits;
      bits += 32;
    }
    while data >= 0x80 {
      self.output.push(data as u8 | 0x80);
      data >>= 7;
    }
    self.output.push(data as u8);
  }
}

#[derive(Default)]
struct EncodeIds<T> {
  lookup: HashMap<T, usize>,
}

impl<T: Hash + Eq> EncodeIds<T> {
  fn clear(&mut self) {
    self.lookup.clear();
  }

  fn get(&mut self, value: T) -> (usize, bool) {
    let next = self.lookup.len();
    match self.lookup.entry(value) {
      Entry::Occupied(e) => (*e.get(), false),
      Entry::Vacant(e) => (*e.insert(next), true),
    }
  }
}

struct Decoder<'a> {
  table: &'a mut Table,
  path_ids: DecodeIds<PathId>,
  name_ids: DecodeIds<NameId>,
  wires: DecodeIds<()>,
  input: slice::Iter<'a, u8>,
}

#[derive(Debug)]
pub enum DecodeError {
  InvalidHeader,
  UnexpectedEof,
  InvalidId,
  InvalidUtf8,
  InvalidInt,
  InvalidWires,
  DuplicateNet,
  ExpectedEof,
}

impl Decoder<'_> {
  fn decode_nets(&mut self) -> Result<HashMap<NameId, FlatNet>, DecodeError> {
    let mut nets = HashMap::new();
    for _ in 0..self.read()? {
      let name = self.decode_name_id()?;
      let net = self.decode_net()?;
      let old = nets.insert(name, net);
      if old.is_some() {
        Err(DecodeError::DuplicateNet)?;
      }
    }
    Ok(nets)
  }

  fn decode_net(&mut self) -> Result<FlatNet, DecodeError> {
    let free = self.decode_list(Self::decode_wire)?;
    let nodes = self.decode_list(Self::decode_node)?;
    let wires = self.wires.lookup.len();
    if !self.wires.lookup.iter().all(|x| x.is_none()) {
      Err(DecodeError::InvalidWires)?;
    }
    self.wires.reset();
    Ok(FlatNet { wires: Counter(Wire(wires)), free, nodes, links: WireLinks::default() })
  }

  fn decode_node(&mut self) -> Result<FlatNode, DecodeError> {
    let name = self.decode_name()?;
    let pri = self.decode_wire()?;
    let aux = self.decode_list(Self::decode_wire)?;
    Ok(FlatNode { name, pri, aux })
  }

  fn decode_wire(&mut self) -> Result<Wire, DecodeError> {
    let id = self.read()?;
    match self.wires.get(id) {
      None => {
        self.wires.insert(id, ());
      }
      Some(Ok(())) => {
        self.wires.remove(id);
      }
      Some(Err(err)) => {
        Err(err)?;
      }
    }
    Ok(Wire(id))
  }

  fn decode_name_id(&mut self) -> Result<NameId, DecodeError> {
    let id = self.read()?;
    self.name_ids.get(id).unwrap_or_else(|| {
      let name = self.decode_name()?;
      Ok(self.name_ids.insert(id, self.table.add_name(name)))
    })
  }

  fn decode_name(&mut self) -> Result<Name, DecodeError> {
    let path = self.decode_path_id()?;
    let children =
      (0..self.read()?).map(|_| self.decode_name_id()).collect::<Result<Vec<_>, _>>()?;
    let payload = self.read_nat()?;
    Ok(Name { path, children, payload })
  }

  fn decode_path_id(&mut self) -> Result<PathId, DecodeError> {
    let id = self.read()?;
    self.path_ids.get(id).unwrap_or_else(|| {
      let length = self.read()?;
      let input = self.input.as_slice();
      let (bytes, input) = input.split_at_checked(length).ok_or(DecodeError::UnexpectedEof)?;
      self.input = input.iter();
      let path = str::from_utf8(bytes).map_err(|_| DecodeError::InvalidUtf8)?;
      Ok(self.path_ids.insert(id, self.table.add_path(path)))
    })
  }

  fn decode_list<T>(
    &mut self,
    f: impl Fn(&mut Self) -> Result<T, DecodeError>,
  ) -> Result<Vec<T>, DecodeError> {
    (0..self.read()?).map(|_| f(self)).collect()
  }

  fn read(&mut self) -> Result<usize, DecodeError> {
    let mut value = 0;
    let mut bits = 0;
    loop {
      let chunk = self.input.next().ok_or(DecodeError::UnexpectedEof)?;
      value |= ((chunk & 0x7f) as usize) << bits;
      bits += 7;
      if bits >= usize::BITS {
        Err(DecodeError::InvalidInt)?;
      }
      if chunk & 0x80 == 0 {
        break;
      }
    }
    Ok(value)
  }

  fn read_nat(&mut self) -> Result<Nat, DecodeError> {
    let mut bits = 0;
    let mut data = 0u64;
    let mut parts = Vec::new();
    loop {
      if bits > 32 {
        parts.push(data as u32);
        data >>= 32;
        bits -= 32;
      }
      let chunk = self.input.next().ok_or(DecodeError::UnexpectedEof)?;
      data |= ((chunk & 0x7f) as u64) << bits;
      bits += 7;
      if chunk & 0x80 == 0 {
        break;
      }
    }
    while data != 0 {
      parts.push(data as u32);
      data >>= 32;
    }
    Ok(Nat(parts))
  }
}

#[derive(Default)]
struct DecodeIds<T> {
  lookup: Vec<Option<T>>,
}

impl<T: Copy> DecodeIds<T> {
  fn reset(&mut self) {
    self.lookup.clear();
  }

  fn get(&mut self, id: usize) -> Option<Result<T, DecodeError>> {
    if id == self.lookup.len() {
      self.lookup.push(None);
      None
    } else {
      Some(self.lookup.get(id).copied().flatten().ok_or(DecodeError::InvalidId))
    }
  }

  fn insert(&mut self, id: usize, value: T) -> T {
    self.lookup[id] = Some(value);
    value
  }

  fn remove(&mut self, id: usize) {
    self.lookup[id] = None;
  }
}
