{
  pkgs,
}:
let
  inherit (pkgs)
    lib
    stdenv
    stdenvNoCC
    tree-sitter
    nodejs_24
    nushell
    ;

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

  check =
    name: fileset:
    stdenv.mkDerivation {
      inherit name;
      src = lib.fileset.toSource {
        root = ../.;
        fileset = lib.fileset.unions fileset;
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
        cd lsp/${name}
        nu test.nu
        touch $out
      '';
    };

  self.packages.tree-sitter-vine = grammar "tree-sitter-vine";
  self.checks.tree-sitter-vine = check "tree-sitter-vine" [
    ../root
    ../vine/examples
    ../tests/programs
    ../lsp/tree-sitter-vine
  ];

  self.packages.tree-sitter-ivy = grammar "tree-sitter-ivy";
  self.checks.tree-sitter-ivy = check "tree-sitter-ivy" [
    ../ivy/examples
    ../lsp/tree-sitter-ivy
  ];
in
self
