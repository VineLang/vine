mod build;
mod run;

use anyhow::Result;
use build::BuildArgs;
use clap::Subcommand;
use run::RunArgs;

pub struct VineCli {
  pub vine_env: String,
}

impl VineCli {
  pub fn execute(&self, maybe_command: Option<VineCliCommand>) -> Result<()> {
    match maybe_command {
      None => self.run(RunArgs::default()),
      Some(command) => match command {
        VineCliCommand::Run(args) => self.run(args),
        VineCliCommand::Build(args) => self.build(args),
      },
    }
  }
}

#[derive(Clone, Subcommand)]
pub enum VineCliCommand {
  #[command(about = "Build and execute the resulting Ivy with the IVM")]
  Run(RunArgs),
  #[command(about = "Compile a Vine file and its module graph into Ivy")]
  Build(BuildArgs),
}
