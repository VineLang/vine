mod optimize;
mod run;

use anyhow::Result;
use clap::Subcommand;
use optimize::OptimizeArgs;
use run::RunArgs;

pub struct IvyCli {
  pub ivy_env: String,
}

impl IvyCli {
  pub fn execute(&self, maybe_command: Option<IvyCliCommand>) -> Result<()> {
    match maybe_command {
      None => self.run(RunArgs::default()),
      Some(command) => match command {
        IvyCliCommand::Run(args) => self.run(args),
        IvyCliCommand::Optimize(args) => self.optimize(args),
      },
    }
  }
}

#[derive(Clone, Subcommand)]
pub enum IvyCliCommand {
  #[command(about = "Build and execute the resulting Ivy with the IVM")]
  Run(RunArgs),
  #[command(about = "Compile a Ivy file and its module graph into Ivy")]
  Optimize(OptimizeArgs),
}
