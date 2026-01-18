{
  typix,
  hyptyp,
  typsitter,
  flake-utils,
  pkgs,
  system,
  grammars,
  cli,
}:
let
  self.packages.typsitter-langs = pkgs.callPackage typsitter.lib.typsitterMkDerivation {
    typsitterGrammars = {
      inherit (typsitter.lib.typsitterDefaultGrammars pkgs)
        bash
        json
        toml
        ;
      vine = grammars.packages.tree-sitter-vine;
      ivy = grammars.packages.tree-sitter-ivy;
    };
  };

  typixLib = typix.lib.${system};

  hyptypLib = hyptyp.lib.${system} typixLib;

  docCmd = out: "${cli.packages.vine-no-root}/bin/vine doc --debug --no-root root ${out}";
  rootDocs = pkgs.stdenvNoCC.mkDerivation {
    name = "root-docs";
    src = pkgs.lib.fileset.toSource {
      root = ../.;
      fileset = ../root;
    };
    buildPhase = docCmd "$out";
  };

  commonArgs = {
    typstSource = "docs/main.typ";

    typstOpts = {
      features = [ "html" ];
    };

    fontPaths = [
      "${pkgs.atkinson-hyperlegible}/share/fonts/opentype"
      "${pkgs.atkinson-hyperlegible-mono}/share/fonts/opentype"
    ];

    virtualPaths = [
      {
        dest = "docs/deps/typsitter/typsitter.typ";
        src = typsitter.lib.src;
      }
      {
        dest = "docs/deps/typsitter-langs/";
        src = self.packages.typsitter-langs;
      }
      {
        dest = "docs/deps/hyptyp";
        src = hyptypLib.hyptyp-typst;
      }
    ];
  };

  watchArgs = commonArgs // { };

  buildArgs = commonArgs // {
    src = pkgs.lib.fileset.toSource {
      root = ../.;
      fileset = ./.;
    };
    virtualPaths = commonArgs.virtualPaths ++ [
      {
        dest = "docs/root";
        src = "${rootDocs}/root";
      }
    ];
  };

  self.packages.docs = hyptypLib.buildHyptypProject buildArgs;
  self.apps.docs = flake-utils.lib.mkApp {
    drv =
      let
        typstWatcher = hyptypLib.watchHyptypProject watchArgs;
      in
      pkgs.writeShellScriptBin "watch-docs" ''
        ${docCmd "docs"}
        ${pkgs.watchexec}/bin/watchexec -qw root "${docCmd "docs"}" &
        ${typstWatcher}/bin/${typstWatcher.name}
      '';
  };

  self.packages.docs-pdf = typixLib.buildTypstProject buildArgs;
  self.apps.docs-pdf = flake-utils.lib.mkApp {
    drv = hyptypLib.watchHyptypProject watchArgs;
  };
in
self
