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

        rustToolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
        craneLib = (crane.mkLib pkgs).overrideToolchain (_: rustToolchain);

        self.cli = import ./cli/cli.nix {
          inherit pkgs craneLib;
        };

        self.tests = import ./tests/tests.nix {
          inherit
            pkgs
            flake-utils
            rustToolchain
            craneLib
            ;
          inherit (self) cli;
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

        checks.fmt =
          let
            processPlugin =
              plugin:
              let
                pieces = builtins.match "^(.+)@([0-9a-f]+)$" plugin;
                path = pkgs.fetchurl {
                  url = builtins.elemAt pieces 0;
                  outputHash = builtins.elemAt pieces 1;
                  outputHashAlgo = "sha256";
                };
              in
              if pkgs.lib.strings.hasSuffix ".wasm" path then
                path
              else
                let
                  json = builtins.fromJSON (builtins.readFile path);
                  id =
                    {
                      aarch64-darwin = "darwin-aarch64";
                      aarch64-linux = "linux-aarch64";
                      x86_64-darwin = "darwin-x86_64";
                      x86_64-linux = "linux-x86_64";
                    }
                    .${pkgs.stdenv.hostPlatform.system};
                  rawZip = pkgs.fetchurl {
                    url = json.${id}.reference;
                    outputHash = json.${id}.checksum;
                    outputHashAlgo = "sha256";
                  };
                  zip =
                    if pkgs.stdenv.hostPlatform.isElf then
                      pkgs.stdenvNoCC.mkDerivation {
                        name = "plugin.zip";
                        nativeBuildInputs = [
                          pkgs.zip
                          pkgs.unzip
                          pkgs.autoPatchelfHook
                          pkgs.libgcc
                        ];
                        dontUnpack = true;
                        buildPhase = ''
                          unzip ${rawZip}
                          autoPatchelf .
                          zip $out *
                        '';
                      }
                    else
                      rawZip;
                  plugin = pkgs.writeText "plugin.json" (
                    builtins.toJSON {
                      inherit (json)
                        schemaVersion
                        kind
                        name
                        version
                        ;
                      ${id} = {
                        reference = zip;
                        checksum = builtins.hashFile "sha256" zip;
                      };
                    }
                  );
                in
                "${plugin}@${builtins.hashFile "sha256" plugin}";
            cfg = builtins.fromJSON (builtins.readFile ./dprint.json);
            plugins = builtins.concatStringsSep " " (builtins.map processPlugin cfg.plugins);
          in
          pkgs.stdenvNoCC.mkDerivation {
            name = "fmt-check";
            src = ./.;
            nativeBuildInputs = [
              pkgs.dprint
              rustToolchain
            ];
            buildPhase = ''
              mkdir -p target/release
              ln -s ${self.cli.packages.vine}/bin/vine target/release/vine
              DPRINT_CACHE_DIR=`mktemp -d` dprint check --plugins ${plugins}
              touch $out
            '';
          };

        checks.cspell = pkgs.stdenvNoCC.mkDerivation {
          name = "cspell-check";
          src = ./.;
          nativeBuildInputs = [ pkgs.nodePackages.cspell ];

          buildPhase = ''
            cspell lint --no-progress --config ./cspell.json
          '';

          installPhase = "touch $out";
        };

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
