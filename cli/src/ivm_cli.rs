use std::{fs, io, path::PathBuf};

use anyhow::Result;
use clap::{Args, CommandFactory};
use clap_complete::generate;
use ivy::{name::Table, text::parser::Parser};

use crate::common::RunArgs;

#[derive(Debug, clap::Parser)]
#[command(name = "ivm", version, propagate_version = true)]
pub enum IvmCommand {
  Run(IvmRunCommand),
  Completion(IvmCompletionCommand),
}

impl IvmCommand {
  pub fn execute() -> Result<()> {
    match <Self as clap::Parser>::parse() {
      IvmCommand::Run(run) => run.execute(),
      IvmCommand::Completion(completion) => completion.execute(),
    }
  }
}

#[derive(Debug, Args)]
pub struct IvmRunCommand {
  #[arg()]
  src: PathBuf,
  #[command(flatten)]
  run_args: RunArgs,
}

impl IvmRunCommand {
  pub fn execute(self) -> Result<()> {
    let src_contents = fs::read_to_string(self.src.clone())?;
    let table = &mut Table::default();
    let nets = Parser::parse(table, &src_contents).unwrap();
    let nets = nets.to_flat_nets().unwrap();
    self.run_args.run(table, &nets, false);
    Ok(())
  }
}

#[derive(Debug, Args)]
pub struct IvmCompletionCommand {
  pub shell: clap_complete::Shell,
}

impl IvmCompletionCommand {
  pub fn execute(self) -> Result<()> {
    let shell = self.shell;

    let mut cmd = IvmCommand::command();
    let cmd_name = cmd.get_name().to_string();
    generate(shell, &mut cmd, cmd_name, &mut io::stdout());

    Ok(())
  }
}
