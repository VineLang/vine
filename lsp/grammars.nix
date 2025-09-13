{
  lib,
  stdenvNoCC,
  tree-sitter,
  nodejs_24,
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
}
