use clap::Args;

use super::VineCli;
use crate::{compile_nets, run_nets};

impl VineCli {
  pub fn run(&self, RunArgs { src }: RunArgs) -> anyhow::Result<()> {
    run_nets(compile_nets(src));
    Ok(())
  }
}

#[derive(Clone, Args)]
pub struct RunArgs {
  #[arg(index = 1, default_value = "main.vi")]
  src: String,
}

impl Default for RunArgs {
  fn default() -> Self {
    Self { src: "main.vi".to_string() }
  }
}
