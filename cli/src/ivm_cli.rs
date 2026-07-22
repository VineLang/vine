use std::{fs, io, path::PathBuf};

use anyhow::Result;
use clap::{Args, CommandFactory};
use clap_complete::generate;
use ivy::{
  binary::{decode_binary, is_binary},
  name::Table,
  text::parser::Parser,
};

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
  src: PathBuf,
  #[command(flatten)]
  run_args: RunArgs,
}

impl IvmRunCommand {
  pub fn execute(self) -> Result<()> {
    let data = fs::read(self.src.clone())?;
    let table = &mut Table::default();
    let nets = if is_binary(&data) {
      decode_binary(table, &data).unwrap()
    } else {
      let nets = Parser::parse(table, &String::from_utf8(data).unwrap()).unwrap();
      nets.to_flat_nets().unwrap()
    };
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
