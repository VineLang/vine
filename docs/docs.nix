{
  typix,
  hyptyp,
  typsitter,
  flake-utils,
  pkgs,
  system,
  grammars,
}:
let
  typsitter-base = pkgs.callPackage typsitter.lib.typsitterMkDerivation {
    typsitterGrammars = {
      inherit (typsitter.lib.typsitterDefaultGrammars pkgs)
        nu
        json
        ;
    };
  };

  typsitter-vine = pkgs.callPackage typsitter.lib.typsitterMkDerivation {
    typsitterGrammars = {
      vine = grammars.packages.tree-sitter-vine;
      ivy = grammars.packages.tree-sitter-ivy;
    };
  };

  typixLib = typix.lib.${system};

  hyptypLib = hyptyp.lib.${system} typixLib;

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
        dest = "docs/deps/typsitter-langs/base";
        src = typsitter-base;
      }
      {
        dest = "docs/deps/typsitter-langs/vine";
        src = typsitter-vine;
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
  };
in
{
  packages = {
    inherit typsitter-base typsitter-vine;
    docs = hyptypLib.buildHyptypProject buildArgs;
    docs-pdf = typixLib.buildTypstProject buildArgs;
  };

  apps = {
    docs = flake-utils.lib.mkApp {
      drv = hyptypLib.watchHyptypProject watchArgs;
    };
    docs-pdf = flake-utils.lib.mkApp {
      drv = typixLib.watchTypstProject watchArgs;
    };
  };
}
