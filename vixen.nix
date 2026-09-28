{
  pkgs,
  craneLib,
}:
let
  vixenConfig = {
    pname = "vixen";
    version = "0.0.0";
    src = pkgs.lib.fileset.toSource {
      root = ./.;
      fileset = pkgs.lib.fileset.unions [
        ./Cargo.toml
        ./util
        ./vixen
        ./ivm
        ./ivy
        ./cli
        ./vine
        ./lsp
      ];
    };
    cargoLock = ./Cargo.lock;
    cargoExtraArgs = "--bin vixen";
    doCheck = false;
    cargoArtifacts = craneLib.buildDepsOnly vixenConfig;
  };

  vixenBuild = craneLib.buildPackage vixenConfig;

  builder = "${vixenBuild}/bin/vixen";
  system = pkgs.system;

  to-sri = hash: "blake3-${builtins.replaceStrings [ "-" "_" ] [ "+" "/" ] hash}=";

  react =
    path:
    let
      job_set = builtins.fromJSON (builtins.readFile "${path}/job_set");
      derivations = builtins.mapAttrs (
        _: job:
        let
          drv = builtins.derivation (
            {
              name = pkgs.lib.strings.sanitizeDerivationName job.name;
              inherit builder system;
              args = [
                "_nix"
                (builtins.toJSON job.task)
              ] ++ (builtins.map (id: derivations.${id}) job.inputs);
            }
            // (
              if job.task.type == "Fetch" then
                {
                  outputHash = to-sri job.task.hash;
                }
              else if job.task.type == "Store" then
                {
                  outputHash = to-sri job.task.hash;
                  store = path;
                }
              else
                { }
            )
          );
        in
        if job.task.type == "React" then react drv else drv
      ) job_set.jobs;
    in
    derivations.${job_set.root};

  vixen =
    args:
    react (
      builtins.derivation {
        name = "init";
        inherit builder system;
        args = [
          "_nix"
          "_init"
        ] ++ args;
      }
    );

in
{
  packages.vixen = vixen [ ./foo.json ];
}
