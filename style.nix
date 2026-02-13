{
  pkgs,
  treefmt-nix,
  cli,
}:
let
  dprintConfig = builtins.fromJSON (builtins.readFile ./dprint.json);
  treefmt = treefmt-nix.lib.evalModule pkgs {
    projectRootFile = "flake.nix";

    programs.rustfmt.enable = true;
    programs.nixfmt.enable = true;

    programs.dprint = {
      enable = true;
      includes = [
        "*.yml"
        "*.json"
        "*.md"
        "*.toml"
        "*.js"
        "*.ts"
      ];
      inherit (dprintConfig) excludes;
      settings = dprintConfig // {
        exec = null;
        plugins = pkgs.dprint-plugins.getPluginList (plugins: [
          plugins.g-plane-pretty_yaml
          plugins.dprint-plugin-json
          plugins.dprint-plugin-markdown
          plugins.dprint-plugin-toml
          plugins.dprint-plugin-typescript
        ]);
      };
    };

    settings.formatter.vine = {
      command = "${cli.packages.vine-no-root}/bin/vine";
      options = [ "fmt" ];
      includes = [ "*.vi" ];
      inherit (dprintConfig) excludes;
    };
  };

  self.formatter = treefmt.config.build.wrapper;

  self.checks.fmt = treefmt.config.build.check ./.;

  self.checks.cspell = pkgs.stdenvNoCC.mkDerivation {
    name = "cspell-check";
    src = ./.;
    nativeBuildInputs = [ pkgs.nodePackages.cspell ];

    buildPhase = ''
      cspell lint --no-progress --config ./cspell.json
    '';

    installPhase = "touch $out";
  };
in
self
