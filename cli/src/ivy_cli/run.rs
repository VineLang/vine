use std::fs;

use clap::Args;
use ivy::parser::IvyParser;

use crate::run_nets;

use super::IvyCli;

impl IvyCli {
  pub fn run(&self, RunArgs { src: src_path }: RunArgs) -> anyhow::Result<()> {
    let src_contents = fs::read_to_string(src_path).unwrap();
    let nets = IvyParser::parse(&src_contents).unwrap();
    run_nets(nets);
    Ok(())
  }
}

#[derive(Clone, Args)]
pub struct RunArgs {
  #[arg(name = "src", index = 1, default_value = "main.iv")]
  src: String,
}

impl Default for RunArgs {
  fn default() -> Self {
    Self { src: "main.iv".to_string() }
  }
}
