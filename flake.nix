{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane.url = "github:ipetkov/crane";
    typix = {
      url = "github:loqusion/typix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    typsitter = {
      url = "github:TendrilsCompute/typsitter";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyptyp = {
      url = "github:TendrilsCompute/hyptyp";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      rust-overlay,
      crane,
      typix,
      typsitter,
      hyptyp,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ rust-overlay.overlays.default ];
        };

        craneLib = (crane.mkLib pkgs).overrideToolchain (
          pkgs: pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml
        );

        vineConfig = {
          pname = "vine";
          version = "0.0.0";
          src = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [
              ./Cargo.toml
              ./util
              ./ivm
              ./ivy
              ./vine
              ./lsp
              ./cli
              ./tests/Cargo.toml
            ];
          };
          VINE_ROOT_PATH = "../lib/root";
          cargoLock = ./Cargo.lock;
          outputHashes = {
            "git+https://github.com/tjjfvi/class?rev=99738e6#99738e67dd8fb3b97d65e6fc59b92f04c11519a4" =
              "sha256-ye8DqeDRXsNpTWpGGlvWxSSc1AiXOLud99dHpB/VhZg=";
          };
          doCheck = false;
        };

        vineNoRoot = craneLib.buildPackage (
          vineConfig
          // {
            cargoArtifacts = craneLib.buildDepsOnly vineConfig;
          }
        );

        vine = pkgs.runCommand "vine" { } ''
          mkdir $out
          cp -r ${vineNoRoot}/* $out
          mkdir $out/lib
          cp -r ${./root} $out/lib/root
        '';

        grammars = pkgs.callPackage ./lsp/grammars.nix { };
        docs = pkgs.callPackage ./docs/docs.nix {
          inherit
            flake-utils
            grammars
            hyptyp
            typix
            typsitter
            vineNoRoot
            ;
        };
      in
      builtins.foldl' pkgs.lib.attrsets.recursiveUpdate
        {
          formatter = pkgs.nixfmt-tree;

          packages = {
            default = vine;
            inherit vine;
          };

          devShells.default = craneLib.devShell {
            name = "vine-dev";
            nativeBuildInputs = [
              pkgs.nushell
              pkgs.nodejs_24
              pkgs.tree-sitter
            ];
          };
        }
        [
          grammars
          docs
        ]
    );
}
