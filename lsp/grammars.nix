{
  lib,
  stdenv,
  stdenvNoCC,
  tree-sitter,
  nodejs_24,
  nushell,
}:
let
  grammar =
    name:
    stdenvNoCC.mkDerivation {
      inherit name;
      src = ./${name};
      nativeBuildInputs = [
        tree-sitter
        nodejs_24
      ];
      buildPhase = ''
        tree-sitter generate
        mkdir $out
        cp -r . $out
      '';
    };
in
{
  packages = {
    tree-sitter-vine = grammar "tree-sitter-vine";
    tree-sitter-ivy = grammar "tree-sitter-ivy";
  };

  checks = {
    tree-sitter-vine = stdenv.mkDerivation {
      name = "tree-sitter-vine";

      src = lib.fileset.toSource {
        root = ../.;
        fileset = lib.fileset.unions [
          ../root/.
          ../vine/examples/.
          ../tests/programs/.
          ../lsp/tree-sitter-vine/.
        ];
      };

      nativeBuildInputs = [
        nushell
        nodejs_24
        tree-sitter
      ];

      buildPhase = ''
        # fake $HOME b/c tree-sitter writes ~/.config/ files, but inside
        # nix builds that dir is read-only.
        export HOME=$PWD/.home
        cd lsp/tree-sitter-vine
        nu test.nu
        touch $out
      '';
    };

    tree-sitter-ivy = stdenv.mkDerivation {
      name = "tree-sitter-ivy";

      src = lib.fileset.toSource {
        root = ../.;
        fileset = lib.fileset.unions [
          ../ivy/examples/.
          ../lsp/tree-sitter-ivy/.
        ];
      };

      nativeBuildInputs = [
        nushell
        nodejs_24
        tree-sitter
      ];

      buildPhase = ''
        # fake $HOME b/c tree-sitter writes ~/.config/ files, but inside
        # nix builds that dir is read-only.
        export HOME=$PWD/.home
        cd lsp/tree-sitter-ivy
        nu test.nu
        touch $out
      '';
    };
  };
}
