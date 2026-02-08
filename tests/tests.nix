{
  pkgs,
  flake-utils,
  rustToolchain,
  craneLib,
  cli,
}:
let
  inherit (builtins)
    map
    filter
    attrNames
    dirOf
    baseNameOf
    concatLists
    attrValues
    concatStringsSep
    ;

  inherit (pkgs.lib.strings)
    hasSuffix
    removeSuffix
    splitString
    ;

  forEach = list: fn: map fn list;
  merge = builtins.foldl' pkgs.lib.attrsets.recursiveUpdate { };

  ls_ = pred: dir: map (child: /${dir}/${child}) (filter pred (attrNames (builtins.readDir dir)));
  ls = ls_ (x: true);
  sanitize = path: removeSuffix ".iv" (removeSuffix ".vi" (baseNameOf path));

  vine = "${cli.packages.vine}/bin/vine";
  ivy = "${cli.packages.ivy}/bin/ivy";

  directive =
    keyword: default: src:
    let
      content =
        if builtins.readFileType src == "directory" then
          builtins.readFile "${src}/${baseNameOf src}.vi"
        else
          builtins.readFile src;
      match = builtins.match "(.*\n)?// @${keyword} ?([^\n]*)\n.*" content;
    in
    if match == null then default else builtins.elemAt match 1;

  libs =
    path:
    let
      libs = directive "libs" "" path;
    in
    if libs == "" then
      ""
    else
      concatStringsSep " " (map (x: "--lib ${just ../${x}}") (splitString " " libs));

  just =
    src:
    let
      dir = pkgs.lib.fileset.toSource {
        root = dirOf src;
        fileset = src;
      };
    in
    "${dir}/${baseNameOf src}";

  build =
    {
      name,
      vi,
      buildArgs ? "",
    }:
    pkgs.runCommand "${name}.iv"
      {
        __contentAddressed = true;
      }
      ''
        echo "building ${name}"
        ${vine} build ${vi} ${buildArgs} >$out
      '';

  buildFail =
    {
      name,
      vi,
      buildArgs ? "",
    }:
    pkgs.runCommand "${name}.error" { } ''
      echo "building ${name}"
      ! ${vine} build ${vi} ${buildArgs} 2>$out
    '';

  run =
    {
      name,
      iv,
      input,
      runArgs ? "",
      ext ? "txt",
      fail ? false,
    }:
    pkgs.runCommand name { } ''
      mkdir $out
      cp ${iv} $out/compiled.iv
      echo "running ${name}"
      ${if fail then "!" else ""} ${ivy} run $out/compiled.iv --no-perf ${runArgs} <${input} 2>&1 >$out/output.${ext} | tee $out/stats
    '';

  tests.fail = forEach (ls ./fail) (
    path:
    let
      name = sanitize path;
      vi = just path;
      buildArgs = directive "build" "" vi;
      error = buildFail { inherit name vi buildArgs; };
    in
    {
      checks."tests-fail-${name}" = error;
      snaps."fail/${name}.error" = error;
    }
  );

  program =
    {
      path,
      config ? path,
      key ? "programs",
    }:
    let
      name = sanitize path;
      vi = just path;
      buildArgs = directive "build" "" config;
      runArgs = directive "run" "" config;
      input = ./input/${directive "input" "empty" config};
      ext = directive "output" "txt" config;
      fail = directive "fail" null config != null;
      iv = build { inherit name vi buildArgs; };
      result = run {
        inherit
          name
          iv
          runArgs
          input
          ext
          fail
          ;
      };
    in
    {
      checks."tests-${key}-${name}" = result;
      snaps."${key}/${name}/output.${ext}" = "${result}/output.${ext}";
      snaps."${key}/${name}/stats" = "${result}/stats";
    };

  tests.programs = forEach (ls ./programs) (path: program { inherit path; });

  tests.examples = forEach (ls ../vine/examples) (
    path:
    program {
      inherit path;
      config = ./examples/${sanitize path};
      key = "examples";
    }
  );

  tests.ivy = forEach (ls ../ivy/examples) (
    path:
    let
      name = sanitize path;
      iv = just path;
      input = ./ivy/${name};
      result = run {
        inherit name iv input;
      };
    in
    {
      checks."tests-ivy-${name}" = result;
      snaps."ivy/${name}/output.txt" = "${result}/output.txt";
      snaps."ivy/${name}/stats" = "${result}/stats";
    }
  );

  tests.repl = forEach (ls ./repl) (
    path:
    let
      name = sanitize path;
      args = directive "args" "" path;
      log = pkgs.runCommand "${name}.repl.vi" { } ''
        echo "repl ${name}"
        ${vine} repl --echo ${libs path} ${args} <${path} >$out
      '';
    in
    {
      checks."tests-repl-${name}" = log;
      snaps."repl/${name}.repl.vi" = log;
    }
  );

  tests.fmt = forEach (ls ./fmt) (
    path:
    let
      name = sanitize path;
      args = directive "args" "" path;
      formatted = pkgs.runCommand "${name}.fmt.vi" { } ''
        echo "fmt ${name}"
        ${vine} fmt ${args} <${path} >$out
      '';
    in
    {
      checks."tests-fmt-${name}" = formatted;
      snaps."fmt/${name}.fmt.vi" = formatted;
    }
  );

  tests.compile = forEach (ls ./compile) (
    path:
    let
      name = sanitize path;
      vi = just path;
      buildArgs = directive "build" "" vi;
      iv = build { inherit name vi buildArgs; };
    in
    {
      checks."tests-compile-${name}" = iv;
      snaps."compile/${name}.iv" = iv;
    }
  );

  tests.aoc_2024 = forEach (ls_ (hasSuffix ".vi") ./aoc_2024) (
    path:
    let
      name = sanitize path;
      vi = just path;
      iv = build { inherit name vi; };
      input = ./aoc_2024/input/${name};
      runArgs = "-d";
      result = run {
        inherit
          name
          iv
          input
          runArgs
          ;
      };
    in
    {
      checks."tests-aoc_2024-${name}" = result;
      snaps."aoc_2024/${name}/output.txt" = "${result}/output.txt";
      snaps."aoc_2024/${name}/stats" = "${result}/stats";
    }
  );

  tests.misc = attrValues {
    test =
      let
        vi = just ./misc/basic_test.vi;
        result = pkgs.runCommand "misc-basic_test" { } ''
          ${vine} test --no-stats ${vi} "#basic_test::test" >$out
        '';
      in
      {
        checks.tests-misc-basic_test = result;
        snaps."misc/basic_test/output.txt" = result;
      };

    missing_no =
      let
        error = buildFail {
          name = "missing_no";
          vi = "missing_no.vi";
        };
      in
      {
        checks.tests-misc-missing_no = error;
        snaps."misc/missing_no.error" = error;
      };

    miri =
      let
        cat = "${all.checks.tests-examples-cat}/compiled.iv";
        tiny_f64 = "${all.checks.tests-programs-tiny_f64}/compiled.iv";
      in
      {
        checks.tests-misc-miri = craneLib.mkCargoDerivation (
          cli.internal.ivyConfig
          // {
            name = "miri-tests";
            buildPhaseCargoCommand = ''
              export MIRIFLAGS="-Zmiri-disable-isolation"
              ivy() { cargo miri run --no-default-features -p vine-cli --bin ivy -- "$@"; }
              [[ `echo "Hello, miri!" | ivy run ${cat} -H 2K` == "Hello, miri!" ]]
              [[ `ivy run ${tiny_f64} -H 2K` == "12 + 34 = 46" ]]
            '';
            doInstallCargoArtifacts = false;
            MIRI_SYSROOT = craneLib.mkCargoDerivation {
              pname = "miri-sysroot";
              version = "0.0.0";
              dontUnpack = true;
              buildPhaseCargoCommand = ''
                MIRI_SYSROOT=$out cargo miri setup
              '';
              cargoLock =
                pkgs.runCommand "sysroot-cargoLock"
                  {
                    nativeBuildInputs = [ rustToolchain ];
                  }
                  ''
                    cp `rustc --print sysroot`/lib/rustlib/src/rust/library/Cargo.lock $out
                  '';
              dontFixup = true;
              cargoArtifacts = null;
            };
          }
        );
      };
  };

  snaps = pkgs.writeText "snaps.json" (builtins.toJSON all.snaps);

  self.checks.snaps = pkgs.runCommand "snaps" { } ''
    echo checking snaps
    ${pkgs.nushell}/bin/nu ${./snaps.nu} ${snaps} ${./snaps}
    touch $out
  '';

  self.apps.snaps = flake-utils.lib.mkApp {
    drv = pkgs.writeShellScriptBin "snaps" ''
      ${pkgs.nushell}/bin/nu ${./snaps.nu} ${snaps} ./tests/snaps --write
    '';
  };

  all = merge ([ self ] ++ concatLists (attrValues tests));
in
builtins.removeAttrs all [ "snaps" ]
