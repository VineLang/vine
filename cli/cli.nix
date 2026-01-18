{
  pkgs,
  craneLib,
}:
let

  self.packages.ivy = craneLib.buildPackage {
    pname = "ivy";
    version = "0.0.0";
    src = pkgs.lib.fileset.toSource {
      root = ./..;
      fileset = pkgs.lib.fileset.unions [
        ../Cargo.toml
        ../util
        ../ivm
        ../ivy
        ../cli
        ../vine/Cargo.toml
        ../lsp/Cargo.toml
      ];
    };
    cargoLock = ../Cargo.lock;
    preBuild = ''
      mkdir -p vine/src lsp/src
      touch vine/src/lib.rs lsp/src/lib.rs
    '';
    cargoExtraArgs = "--no-default-features --bin ivy";
    doCheck = false;
  };

  self.packages.vine-no-root = craneLib.buildPackage {
    pname = "vine";
    version = "0.0.0";
    src = pkgs.lib.fileset.toSource {
      root = ./..;
      fileset = pkgs.lib.fileset.unions [
        ../Cargo.toml
        ../util
        ../ivm
        ../ivy
        ../vine/Cargo.toml
        ../vine/src
        ../lsp
        ../cli
      ];
    };
    cargoLock = ../Cargo.lock;
    outputHashes = {
      "git+https://github.com/tjjfvi/class?rev=99738e6#99738e67dd8fb3b97d65e6fc59b92f04c11519a4" =
        "sha256-ye8DqeDRXsNpTWpGGlvWxSSc1AiXOLud99dHpB/VhZg=";
    };
    cargoExtraArgs = "--bin vine";
    VINE_ROOT_PATH = "../lib/root";
    doCheck = false;
  };

  self.packages.vine = pkgs.runCommand "vine" { } ''
    mkdir $out
    cp -r ${self.packages.vine-no-root}/* $out
    mkdir $out/lib
    cp -r ${../root} $out/lib/root
  '';
in
self
