#![allow(unexpected_cfgs)]

use std::{
  env, fs,
  io::{Read, Write},
  path::{Path, PathBuf},
  process::{Command, Stdio},
  sync::mpsc::channel,
  thread::{self, JoinHandle},
};

use dyntest::{dyntest, DynTester};

#[cfg(not(rust_analyzer))]
dyntest!(tests);

fn tests(t: &mut DynTester) {
  env::set_current_dir("..").unwrap();

  let fib_repl_input = b"1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n21\n100\n999999\n";

  t.group("ivy", |t| {
    test_iv(t, "ivy/examples/cat.iv", b"meow\n", ".txt");
    test_iv(t, "ivy/examples/fib.iv", fib_repl_input, ".txt");
    test_iv(t, "ivy/examples/fizzbuzz.iv", b"", ".txt");
    test_iv(t, "ivy/examples/hihi.iv", b"", ".txt");
  });

  t.group("vine", |t| {
    test_vi(t, "vine/examples/fib_repl.vi", fib_repl_input, ".txt");
    test_vi(t, "vine/examples/fib.vi", b"", ".txt");
    test_vi(t, "vine/examples/fizzbuzz.vi", b"", ".txt");
    test_vi(t, "vine/examples/hello_world.vi", b"", ".txt");
    test_vi(t, "vine/examples/mandelbrot_sixel.vi", b"", ".sixel");
    test_vi(t, "vine/examples/mandelbrot_tga.vi", b"", ".tga");
    test_vi(t, "vine/examples/mandelbrot.vi", b"", ".txt");

    test_vi(t, "tests/programs/loop_vi_loop.vi", b"", ".txt");
    test_vi(t, "tests/programs/maybe_set.vi", b"", ".txt");
    test_vi(t, "tests/programs/move_it_move_it.vi", b"", ".txt");
  });
}

const VINE: &[&str] = &["vine"];
const IVY: &[&str] = &["ivy", "--release"];

fn test_vi(t: &mut DynTester, path: &'static str, input: &'static [u8], output_ext: &'static str) {
  let name = Path::file_stem(path.as_ref()).unwrap().to_str().unwrap();
  t.group(name, |t| {
    let (sender, receiver) = channel();
    t.test("compile", move || {
      let (stdout, stderr) = exec(VINE, &[path, "vine/std/std.vi"], &[]);
      assert!(stderr.is_empty());
      let path = test_snapshot(&["vine", name, "compiled.iv"], &stdout);
      _ = sender.send(path);
    });
    t.test("run", move || {
      let path = receiver.recv().unwrap();
      let path = path.as_os_str().to_str().unwrap();
      run_iv("vine", name, path, input, output_ext);
    });
  });
}

fn test_iv(t: &mut DynTester, path: &'static str, input: &'static [u8], output_ext: &'static str) {
  let name = Path::file_stem(path.as_ref()).unwrap().to_str().unwrap();
  t.test(name, || {
    run_iv("ivy", name, path, input, output_ext);
  });
}

fn run_iv(group: &str, name: &str, path: &str, input: &[u8], output_ext: &str) {
  let (stdout, stderr) = exec(IVY, &[path], input);
  test_snapshot(&[group, name, &format!("output{output_ext}")], &stdout);
  let full_stats = String::from_utf8(stderr).unwrap();
  let stats = full_stats.split_once("\nTime").unwrap().0;
  test_snapshot(&[group, name, "stats.txt"], stats.as_bytes());
  fs::write(get_snapshot_path(&[group, name, "timing.txt"]), full_stats[stats.len()..].as_bytes())
    .unwrap();
}

fn exec(bin: &[&str], args: &[&str], input: &[u8]) -> (Vec<u8>, Vec<u8>) {
  let mut child = Command::new(env!("CARGO"))
    .args(["run", "--quiet", "--bin"])
    .args(bin)
    .arg("--")
    .args(args)
    .stdin(Stdio::piped())
    .stdout(Stdio::piped())
    .stderr(Stdio::piped())
    .spawn()
    .unwrap();

  child.stdin.take().unwrap().write_all(input).unwrap();

  let stdout = parallel_read(child.stdout.take().unwrap());
  let stderr = parallel_read(child.stderr.take().unwrap());

  let status = child.wait().unwrap();
  if !status.success() {
    let err = String::from_utf8(stderr.join().unwrap()).unwrap();
    eprintln!("{err}");
    panic!("{status}");
  }
  assert!(status.success());

  (stdout.join().unwrap(), stderr.join().unwrap())
}

fn test_snapshot(components: &[&str], contents: &[u8]) -> PathBuf {
  let path = get_snapshot_path(components);
  let existing = fs::read(&path).ok();
  if !existing.is_some_and(|x| x == contents) {
    if should_write_snapshot() {
      println!("updating snapshot {:?}", path);
      fs::write(&path, contents).unwrap();
    } else {
      panic!("invalid snapshot {:?}", path);
    }
  }
  path
}

fn get_snapshot_path(components: &[&str]) -> PathBuf {
  let mut path = PathBuf::from("tests/snaps");
  path.extend(components);
  fs::create_dir_all(path.parent().unwrap()).unwrap();
  path
}

fn should_write_snapshot() -> bool {
  std::env::var("SNAP_CHECK").is_err()
}

fn parallel_read(mut read: impl Read + Send + 'static) -> JoinHandle<Vec<u8>> {
  thread::spawn(move || {
    let mut buf = Vec::new();
    read.read_to_end(&mut buf).unwrap();
    buf
  })
}
