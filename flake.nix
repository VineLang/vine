{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane.url = "github:ipetkov/crane";
    typsitter.url = "github:TendrilsCompute/typsitter/eb04f70ac94badc2e846e06ff1f3356b1ce32023";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      rust-overlay,
      crane,
      typsitter,
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
          src = pkgs.lib.cleanSource ./.;
          pname = "vine";
          version = "0.0.0";
          VINE_STD_PATH = "${src}/vine/std";
          cargoLock = "${src}/Cargo.lock";
        };

        vine = craneLib.buildPackage (
          vineConfig
          // {
            cargoArtifacts = craneLib.buildDepsOnly vineConfig;
          }
        );
      in
      rec {
        formatter = pkgs.nixfmt-tree;

        packages = {
          default = vine;
          typsitter = typsitter.packages.${system}.default;
        };

        checks = packages;

        devShells.default = craneLib.devShell {
          name = "vine-dev";
          nativeBuildInputs = [
            pkgs.nushell
            pkgs.emscripten
          ];
        };
      }
    );
}
