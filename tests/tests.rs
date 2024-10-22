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

  let fib_repl_input_iv = b"1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n21\n100\n999999\n";
  let fib_repl_input_vi = b"1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n21\n100\n999999\n\nabc\n";

  let mut ivy_tests = Vec::new();
  t.group("ivy", |t| {
    test_iv(t, "ivy/examples/cat.iv", b"meow\n", ".txt", &mut ivy_tests);
    test_iv(t, "ivy/examples/fib_repl.iv", fib_repl_input_iv, ".txt", &mut ivy_tests);
    test_iv(t, "ivy/examples/fizzbuzz.iv", b"", ".txt", &mut ivy_tests);
    test_iv(t, "ivy/examples/hihi.iv", b"", ".txt", &mut ivy_tests);
  });

  let mut vine_tests = Vec::new();
  t.group("vine", |t| {
    test_vi(t, "vine/examples/fib_repl.vi", fib_repl_input_vi, ".txt", &mut vine_tests);
    test_vi(t, "vine/examples/fib.vi", b"", ".txt", &mut vine_tests);
    test_vi(t, "vine/examples/fizzbuzz.vi", b"", ".txt", &mut vine_tests);
    test_vi(t, "vine/examples/hello_world.vi", b"", ".txt", &mut vine_tests);
    test_vi(t, "vine/examples/mandelbrot_sixel.vi", b"", ".sixel", &mut vine_tests);
    test_vi(t, "vine/examples/mandelbrot_tga.vi", b"", ".tga", &mut vine_tests);
    test_vi(t, "vine/examples/mandelbrot.vi", b"", ".txt", &mut vine_tests);
    test_vi(t, "vine/examples/primes.vi", b"", ".txt", &mut vine_tests);
    test_vi(t, "vine/examples/sum_divisors.vi", b"", ".txt", &mut vine_tests);

    test_vi(t, "tests/programs/basic_diverge.vi", b"", ".txt", &mut vine_tests);
    test_vi(t, "tests/programs/final_countdown.vi", b"", ".txt", &mut vine_tests);
    test_vi(t, "tests/programs/inverse.vi", b"", ".txt", &mut vine_tests);
    test_vi(t, "tests/programs/logic.vi", b"", ".txt", &mut vine_tests);
    test_vi(t, "tests/programs/loop_break_continue.vi", b"", ".txt", &mut vine_tests);
    test_vi(t, "tests/programs/loop_vi_loop.vi", b"", ".txt", &mut vine_tests);
    test_vi(t, "tests/programs/maybe_set.vi", b"", ".txt", &mut vine_tests);
    test_vi(t, "tests/programs/move_it_move_it.vi", b"", ".txt", &mut vine_tests);
    test_vi(t, "tests/programs/option_party.vi", b"", ".txt", &mut vine_tests);
    test_vi(t, "tests/programs/pretty_div.vi", b"", ".txt", &mut vine_tests);
    test_vi(t, "tests/programs/so_random.vi", b"", ".txt", &mut vine_tests);
    test_vi(t, "tests/programs/square_case.vi", b"", ".txt", &mut vine_tests);
  });

  let mut vine_fail_tests = Vec::new();
  t.group("vine_fail", |t| {
    test_vi_fail(t, "tests/programs/fail/hallo_world.vi", &mut vine_fail_tests);
    test_vi_fail(t, "tests/programs/fail/informal.vi", &mut vine_fail_tests);
    test_vi_fail(t, "tests/programs/fail/is_not.vi", &mut vine_fail_tests);
    test_vi_fail(t, "tests/programs/fail/missing_no.vi", &mut vine_fail_tests);
  });

  t.group("remove_orphan_snapshots", |t| {
    t.test("ivy", move || {
      remove_orphan_snapshots("tests/snaps/ivy", &ivy_tests);
    });
    t.test("vine", move || {
      remove_orphan_snapshots("tests/snaps/vine", &vine_tests);
    });
    t.test("vine_fail", move || {
      remove_orphan_snapshots("tests/snaps/vine_fail", &vine_fail_tests);
    });
  });
}

const VINE: &[&str] = &["vine"];
const IVY: &[&str] = &["ivy", "--release"];

fn test_vi(
  t: &mut DynTester,
  path: &'static str,
  input: &'static [u8],
  output_ext: &'static str,
  vine_tests: &mut Vec<String>,
) {
  let name = Path::file_stem(path.as_ref()).unwrap().to_str().unwrap();
  vine_tests.push(name.to_owned());

  t.group(name, |t| {
    let (sender, receiver) = channel();
    t.test("compile", move || {
      let (stdout, stderr) = exec(VINE, &["build", path], &[], true);
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

fn test_vi_fail(t: &mut DynTester, path: &'static str, vine_tests: &mut Vec<String>) {
  let name = Path::file_stem(path.as_ref()).unwrap().to_str().unwrap();
  vine_tests.push(name.to_owned());
  t.test(name, move || {
    let (_, stderr) = exec(VINE, &["build", path], &[], false);
    test_snapshot(&["vine_fail", &format!("{name}.txt")], &stderr);
  });
}

fn test_iv(
  t: &mut DynTester,
  path: &'static str,
  input: &'static [u8],
  output_ext: &'static str,
  ivy_tests: &mut Vec<String>,
) {
  let name = Path::file_stem(path.as_ref()).unwrap().to_str().unwrap();
  ivy_tests.push(name.to_owned());
  t.test(name, || {
    run_iv("ivy", name, path, input, output_ext);
  });
}

fn run_iv(group: &str, name: &str, path: &str, input: &[u8], output_ext: &str) {
  let (stdout, stderr) = exec(IVY, &["run", path], input, true);
  test_snapshot(&[group, name, &format!("output{output_ext}")], &stdout);
  let full_stats = String::from_utf8(stderr).unwrap();
  let stats = full_stats.split_once("\nTime").unwrap().0;
  test_snapshot(&[group, name, "stats.txt"], stats.as_bytes());
  fs::write(get_snapshot_path(&[group, name, "timing.txt"]), full_stats[stats.len()..].as_bytes())
    .unwrap();
}

/// Removes any snapshots that don't have a corresponding test associated with
/// it
fn remove_orphan_snapshots(path: &'static str, names: &[String]) {
  let Ok(entries) = fs::read_dir(path) else {
    println!("Cannot read path: {}", path);
    return;
  };

  for entry in entries {
    let Ok(entry) = entry else {
      continue;
    };

    let path = entry.path();
    let Some(file_name) = path.file_stem() else {
      continue;
    };

    let Some(file_name_str) = file_name.to_str() else {
      continue;
    };

    let file_name = file_name_str.to_owned();
    if !names.contains(&file_name) {
      println!("Removing unlisted snaps: {}", path.display());
      let remove_res =
        if path.is_dir() { fs::remove_dir_all(&path) } else { fs::remove_file(&path) };

      if let Err(err) = remove_res {
        panic!(
          "Unable to remove {}: {} - {}",
          if path.is_dir() { "directory" } else { "file" },
          path.display(),
          err
        )
      }
    }
  }
}

fn exec(bin: &[&str], args: &[&str], input: &[u8], success: bool) -> (Vec<u8>, Vec<u8>) {
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
  if status.success() != success {
    let err = String::from_utf8(stderr.join().unwrap()).unwrap();
    eprintln!("{err}");
    panic!("{status}");
  }

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
