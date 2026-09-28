use anyhow::Result;
use blocking::unblock;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::{
  borrow::Cow,
  fmt, fs,
  hash::{Hash, Hasher},
  io::Read,
  path::Path,
};

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Blake3([u8; 32]);

impl fmt::Debug for Blake3 {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(self.encode(&mut [0; _]))
  }
}

impl Serialize for Blake3 {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    if serializer.is_human_readable() {
      self.encode(&mut [0; _]).serialize(serializer)
    } else {
      self.0.serialize(serializer)
    }
  }
}

impl<'de> Deserialize<'de> for Blake3 {
  fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
    if deserializer.is_human_readable() {
      let str = <Cow<str>>::deserialize(deserializer)?;
      Blake3::decode(&str).ok_or_else(|| serde::de::Error::custom("invalid blake3 hash"))
    } else {
      Ok(Blake3(<_>::deserialize(deserializer)?))
    }
  }
}

impl Blake3 {
  pub fn hash(bytes: &[u8]) -> Blake3 {
    Blake3(*blake3::hash(bytes).as_bytes())
  }

  pub fn hash_value<T: Hash>(value: &T) -> Blake3 {
    struct Blake3Hasher(blake3::Hasher);
    impl Hasher for Blake3Hasher {
      fn finish(&self) -> u64 {
        unreachable!()
      }

      fn write(&mut self, bytes: &[u8]) {
        self.0.update(bytes);
      }
    }
    let mut hasher = Blake3Hasher(blake3::Hasher::new());
    value.hash(&mut hasher);
    let output = hasher.0.finalize();
    Blake3(*output.as_bytes())
  }

  pub fn hash_file(path: &Path) -> impl Future<Output = Result<Blake3>> {
    let path = path.to_owned();
    unblock(|| {
      let mut file = fs::File::open(path)?;
      let mut hasher = blake3::Hasher::new();
      let mut buf = Box::new([0; 1 << 12]);
      loop {
        let len = file.read(&mut *buf)?;
        if len == 0 {
          break;
        }
        hasher.update(&buf[0..len]);
      }
      Ok(Blake3(*hasher.finalize().as_bytes()))
    })
  }

  pub fn encode<'a>(&self, buf: &'a mut [u8; 43]) -> &'a str {
    let mut i = 0;
    let mut data = 0u16;
    let mut bits = 0;
    for byte in self.0 {
      data = data.wrapping_shl(8) | byte as u16;
      bits += 8;
      while bits > 6 {
        bits -= 6;
        buf[i] = base64_encode((data >> bits & 0x3f) as u8);
        i += 1;
      }
    }
    assert_eq!(i, 42);
    assert_eq!(bits, 4);
    buf[i] = base64_encode((data << 2 & 0x3f) as u8);
    str::from_utf8(buf).unwrap()
  }

  pub fn decode(str: &str) -> Option<Blake3> {
    if str.len() != 43 {
      return None;
    }
    let mut hash = [0; 32];
    let mut i = 0;
    let mut data = 0u16;
    let mut bits = 0;
    for char in str.bytes() {
      let value = base64_decode(char)?;
      data = data.wrapping_shl(6) | value as u16;
      bits += 6;
      while bits > 8 {
        bits -= 8;
        hash[i] = (data >> bits) as u8;
        i += 1;
      }
    }
    assert_eq!(bits, 2);
    assert_eq!(i, 32);
    if data & 0x3 != 0 {
      return None?;
    }
    Some(Blake3(hash))
  }
}

fn base64_encode(value: u8) -> u8 {
  match value {
    0..26 => b'A' + value,
    26..52 => b'a' + value - 26,
    52..62 => b'0' + value - 52,
    62 => b'-',
    63 => b'_',
    _ => unreachable!(),
  }
}

fn base64_decode(char: u8) -> Option<u8> {
  Some(match char {
    b'A'..=b'Z' => char - b'A',
    b'a'..=b'z' => char - b'a' + 26,
    b'0'..=b'9' => char - b'0' + 52,
    b'-' => 62,
    b'_' => 63,
    _ => None?,
  })
}

#[test]
fn test() {
  let hash = Blake3::hash(&[]);
  let encoded = "rxNJufX5oaagQE3qNtzJSZvLJcmtwRK3zJqTyuQfMmI";
  assert_eq!(hash.encode(&mut [0; _]), encoded);
  assert_eq!(Blake3::decode(encoded), Some(hash));
}
