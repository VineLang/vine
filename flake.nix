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

        self.cli = import ./cli/cli.nix {
          inherit pkgs craneLib;
        };

        self.grammars = import ./lsp/grammars.nix {
          inherit pkgs;
        };

        self.docs = import ./docs/docs.nix {
          inherit
            system
            pkgs
            flake-utils
            hyptyp
            typix
            typsitter
            ;
          inherit (self) grammars cli;
        };
      in
      builtins.foldl' pkgs.lib.attrsets.recursiveUpdate {
        formatter = pkgs.nixfmt-tree;

        packages.default = self.cli.packages.vine;

        devShells.default = craneLib.devShell {
          name = "vine-dev";
          nativeBuildInputs = [
            pkgs.nushell
            pkgs.nodejs_24
            pkgs.tree-sitter
          ];
        };
      } (builtins.attrValues self)
    );
}
