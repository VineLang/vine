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
      url = "github:TendrilsCompute/typsitter/6d20389c532961aa6c8f6a72405078cc1be0d9a5";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyptyp = {
      url = "github:TendrilsCompute/hyptyp/9244b8a8a9c6acf65512ab846375f728d4d3d126";
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

        vineConfig = rec {
          name = "vine";
          src = pkgs.lib.cleanSource ./.;
          VINE_ROOT_PATH = "${src}/root";
          cargoLock = "${src}/Cargo.lock";
        };

        vine = craneLib.buildPackage (
          vineConfig
          // {
            cargoArtifacts = craneLib.buildDepsOnly vineConfig;
          }
        );

        grammars = import ./lsp/grammars.nix {
          inherit (pkgs)
            lib
            stdenvNoCC
            tree-sitter
            nodejs_24
            ;
        };

        docs = pkgs.callPackage ./docs/docs.nix {
          inherit
            system
            pkgs
            flake-utils
            typix
            typsitter
            hyptyp
            grammars
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
