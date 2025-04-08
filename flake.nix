{
  description = "Dev environment + build for vine";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, rust-overlay, ... }: 
  let
    supportedSystems = [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    pkgs = system: (import nixpkgs { 
      inherit system; 
      overlays = [ rust-overlay.overlays.default ]; 
    }); 
  in
  {
    devShells = forAllSystems (system: with (pkgs system);
      {
        default = mkShell {
          buildInputs = [ rustup ];
          shellHook = ''
            source <(rustup completions bash)
            source <(rustup completions bash cargo)
          '';
        };
      }
    );

    packages = forAllSystems (system: with (pkgs system);
    let 
      rustPlatform = makeRustPlatform rec {
        cargo = rust-bin.nightly."2024-12-24".default;
        rustc = cargo;
      };
    in
    {
      default = rustPlatform.buildRustPackage rec {
        src = lib.cleanSource ./.;
        pname = "vine";
        version = "0.1";
        CFG_RELEASE_CHANNEL = "nightly";
        VINE_STD_PATH = "${src}/vine/std";
        cargoLock = {
          lockFile = "${src}/Cargo.lock";
          outputHashes = {
            "class-0.1.0" = "sha256-ye8DqeDRXsNpTWpGGlvWxSSc1AiXOLud99dHpB/VhZg=";
          };
        };
      };
    });
  };
}
