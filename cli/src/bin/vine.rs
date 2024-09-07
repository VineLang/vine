use clap::Parser;
use vine_cli::{VineCli, VineCliCommand};

#[derive(Clone, Parser)]
#[command(name = "vine", version, about = "Vine's CLI", propagate_version = true)]
pub struct VineCliArgs {
  #[clap(long, env, default_value = "DEVELOPMENT")]
  pub vine_env: String,
  #[command(subcommand)]
  maybe_command: Option<VineCliCommand>,
}

fn main() {
  let VineCliArgs { vine_env, maybe_command } = VineCliArgs::parse();
  let context = VineCli { vine_env };
  context.execute(maybe_command).unwrap();
}
