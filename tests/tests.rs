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
  let guessing_game_input = b"a seed\na maximum number\n100\n50\n75\n63\n56\n60\n58\n57";
  let lambda_input = b"(\\2.((2 2) 2))(\\f.\\x.(f(f x)))\n(\\a.\\b.b)((\\x.(x x))(\\x.(x x)))";

  t.group("ivy", |t| {
    test_iv(t, "ivy/examples/cat.iv", b"meow\n", ".txt");
    test_iv(t, "ivy/examples/fib_repl.iv", fib_repl_input_iv, ".txt");
    test_iv(t, "ivy/examples/fizzbuzz.iv", b"", ".txt");
    test_iv(t, "ivy/examples/hihi.iv", b"", ".txt");
  });

  t.group("vine", |t| {
    test_vi(t, "vine/examples/cat.vi", include_bytes!("../vine/examples/cat.vi"), ".txt");
    test_vi(t, "vine/examples/fib_repl.vi", fib_repl_input_vi, ".txt");
    test_vi(t, "vine/examples/fib.vi", b"", ".txt");
    test_vi(t, "vine/examples/fizzbuzz.vi", b"", ".txt");
    test_vi(t, "vine/examples/guessing_game.vi", guessing_game_input, ".txt");
    test_vi(t, "vine/examples/hello_world.vi", b"", ".txt");
    test_vi(t, "vine/examples/mandelbrot_sixel.vi", b"", ".sixel");
    test_vi(t, "vine/examples/mandelbrot_tga.vi", b"", ".tga");
    test_vi(t, "vine/examples/mandelbrot.vi", b"", ".txt");
    test_vi(t, "vine/examples/primeness.vi", b"", ".txt");
    test_vi(t, "vine/examples/stream_primes.vi", b"", ".txt");
    test_vi(t, "vine/examples/sub_min.vi", b"", ".txt");
    test_vi(t, "vine/examples/sum_divisors.vi", b"", ".txt");

    test_vi(t, "tests/programs/array_from_list.vi", b"", ".txt");
    test_vi(t, "tests/programs/array_order.vi", b"", ".txt");
    test_vi(t, "tests/programs/array_to_list.vi", b"", ".txt");
    test_vi(t, "tests/programs/basic_diverge.vi", b"", ".txt");
    test_vi(t, "tests/programs/break_result.vi", b"", ".txt");
    test_vi(t, "tests/programs/classify_primes.vi", b"", ".txt");
    test_vi(t, "tests/programs/cond_diverge.vi", b"", ".txt");
    test_vi(t, "tests/programs/cyclist.vi", b"", ".txt");
    test_vi(t, "tests/programs/final_countdown.vi", b"", ".txt");
    test_vi(t, "tests/programs/find_primes.vi", b"", ".txt");
    test_vi(t, "tests/programs/inverse.vi", b"", ".txt");
    test_vi(t, "tests/programs/lambda.vi", lambda_input, ".txt");
    test_vi(t, "tests/programs/logic.vi", b"", ".txt");
    test_vi(t, "tests/programs/loop_break_continue.vi", b"", ".txt");
    test_vi(t, "tests/programs/loop_vi_loop.vi", b"", ".txt");
    test_vi(t, "tests/programs/map_test.vi", b"", ".txt");
    test_vi(t, "tests/programs/maybe_set.vi", b"", ".txt");
    test_vi(t, "tests/programs/no_return.vi", b"", ".txt");
    test_vi(t, "tests/programs/option_party.vi", b"", ".txt");
    test_vi(t, "tests/programs/par.vi", b"", ".txt");
    test_vi(t, "tests/programs/pretty_div.vi", b"", ".txt");
    test_vi(t, "tests/programs/segmented_sieve.vi", b"", ".txt");
    test_vi(t, "tests/programs/sieve.vi", b"", ".txt");
    test_vi(t, "tests/programs/so_random.vi", b"", ".txt");
    test_vi(t, "tests/programs/specializations.vi", b"", ".txt");
    test_vi(t, "tests/programs/square_case.vi", b"", ".txt");
    test_vi(t, "tests/programs/verbose_add.vi", b"", ".txt");

    for (name, _) in t.glob_in("programs/aoc_2024/", "*.vi") {
      let name: String = name.into();
      let input = fs::read(format!("tests/programs/aoc_2024/input/{name}")).unwrap();
      test_vi(t, leak(format!("tests/programs/aoc_2024/{name}.vi")), leak(input), ".txt");
    }

    t.group("fmt", |t| {
      test_vi_fmt(t, "tests/programs/fmt/objects.vi");
      test_vi_fmt(t, "tests/programs/fmt/uses.vi");
    });

    t.group("fail", |t| {
      test_vi_fail(t, "tests/programs/fail/atypical.vi");
      test_vi_fail(t, "tests/programs/fail/hallo_world.vi");
      test_vi_fail(t, "tests/programs/fail/informal.vi");
      test_vi_fail(t, "tests/programs/fail/is_not.vi");
      test_vi_fail(t, "tests/programs/fail/missing_no.vi");
      test_vi_fail(t, "tests/programs/fail/visibility.vi");
    });

    t.group("repl", |t| {
      test_vi_repl(t, "tests/programs/repl/advanced_repl.vi");
      test_vi_repl(t, "tests/programs/repl/basic_repl.vi");
      test_vi_repl(t, "tests/programs/repl/misc.vi");
      test_vi_repl(t, "tests/programs/repl/objects.vi");
      test_vi_repl(t, "tests/programs/repl/randomness.vi");
    });
  });
}

const VINE: &[&str] = &["vine"];
const IVY: &[&str] = &["ivy", "--release"];

fn test_vi(t: &mut DynTester, path: &'static str, input: &'static [u8], output_ext: &'static str) {
  let name =
    path.strip_prefix("tests/programs/").or(path.strip_prefix("vine/examples/")).unwrap_or(path);
  let name = name.strip_suffix(".vi").unwrap_or(name);
  t.group(AsRef::<Path>::as_ref(name), |t| {
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

fn test_vi_fail(t: &mut DynTester, path: &'static str) {
  let name = Path::file_stem(path.as_ref()).unwrap().to_str().unwrap();
  t.test(name, move || {
    let (_, stderr) = exec(VINE, &["build", path], &[], false);
    test_snapshot(&["vine", "fail", &format!("{name}.txt")], &stderr);
  });
}

fn test_vi_repl(t: &mut DynTester, path: &'static str) {
  let name = Path::file_stem(path.as_ref()).unwrap().to_str().unwrap();
  t.test(name, move || {
    let input = fs::read_to_string(path).unwrap();
    let (stdout, stderr) = exec(VINE, &["repl", "--echo"], input.as_bytes(), true);
    assert!(stderr.is_empty());
    test_snapshot(&["vine", "repl", &format!("{name}.repl.vi")], &stdout);
  });
}

fn test_vi_fmt(t: &mut DynTester, path: &'static str) {
  let name = Path::file_stem(path.as_ref()).unwrap().to_str().unwrap();
  t.test(name, move || {
    let input = fs::read_to_string(path).unwrap();
    let (formatted, stderr) = exec(VINE, &["fmt"], input.as_bytes(), true);
    assert!(stderr.is_empty());
    test_snapshot(&["vine", "fmt", &format!("{name}.fmt.vi")], &formatted);
    let (stdout, stderr) = exec(VINE, &["fmt"], input.as_bytes(), true);
    assert!(stderr.is_empty());
    assert_eq!(stdout, formatted);
  });
}

fn test_iv(t: &mut DynTester, path: &'static str, input: &'static [u8], output_ext: &'static str) {
  let name = Path::file_stem(path.as_ref()).unwrap().to_str().unwrap();
  t.test(name, || {
    run_iv("ivy", name, path, input, output_ext);
  });
}

fn run_iv(group: &str, name: &str, path: &str, input: &[u8], output_ext: &str) {
  let (stdout, stderr) = exec(IVY, &["run", path], input, true);
  test_snapshot(&[group, name, &format!("output{output_ext}")], &stdout);
  let full_stats = String::from_utf8(stderr).unwrap();
  let stats = full_stats.split_once("\nPerformance").unwrap().0;
  test_snapshot(&[group, name, "stats.txt"], stats.as_bytes());
  fs::write(get_snapshot_path(&[group, name, "timing.txt"]), full_stats[stats.len()..].as_bytes())
    .unwrap();
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
  if existing.is_none_or(|x| x != contents) {
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

fn leak<T: AsRef<U> + 'static, U: ?Sized>(value: T) -> &'static U {
  (*Box::leak(Box::new(value))).as_ref()
}
