use anyhow::{anyhow, Result};
use clap::Args;
use std::fs;

use super::VineCli;
use crate::compile_nets;

impl VineCli {
  pub fn build(&self, BuildArgs { src, maybe_dest }: BuildArgs) -> Result<()> {
    let dest = match maybe_dest {
      Some(dest) => {
        if dest.ends_with(".iv") {
          Ok(Dest::Path(dest))
        } else if dest == "-" {
          Ok(Dest::Stdout)
        } else {
          Err(anyhow!("Invalid output destination \"{dest}\"; must end with \".iv\"."))
        }
      }
      None => Ok(Dest::Path("target.iv".into())),
    }?;
    let nets = compile_nets(src);
    match dest {
      Dest::Path(dest_path) => Ok(fs::write(dest_path, nets.to_string())?),
      Dest::Stdout => {
        println!("{nets}");
        Ok(())
      }
    }
  }
}

#[derive(Clone, Args)]
pub struct BuildArgs {
  #[arg(index = 1, default_value = "main.vi")]
  src: String,
  #[arg(name = "dest", long, short = 'o')]
  maybe_dest: Option<String>,
}

pub enum Dest {
  Path(String),
  Stdout,
}
