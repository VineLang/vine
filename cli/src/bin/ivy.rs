use clap::Parser;
use vine_cli::{IvyCliCommand, IvyCli};

#[derive(Clone, Parser)]
#[command(name = "ivy", version, about = "Ivy's CLI", propagate_version = true)]
pub struct IvyCliArgs {
  #[clap(long, env, default_value = "DEVELOPMENT")]
  pub ivy_env: String,
  #[command(subcommand)]
  maybe_command: Option<IvyCliCommand>,
}

fn main() {
  let IvyCliArgs { ivy_env, maybe_command } = IvyCliArgs::parse();
  let context = IvyCli { ivy_env };
  context.execute(maybe_command).unwrap();
}

