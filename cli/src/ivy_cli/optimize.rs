use clap::Args;

use super::IvyCli;

impl IvyCli {
  pub fn optimize(&self, _: OptimizeArgs) -> anyhow::Result<()> {
    unimplemented!()
  }
}

#[derive(Clone, Args)]
pub struct OptimizeArgs {
  #[arg(index = 1, default_value = "main.iv")]
  src: String,
  #[arg(name = "dest", long, short = 'o')]
  maybe_dest: String,
}
