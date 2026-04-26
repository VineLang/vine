use anyhow::Result;
use vine_cli::vine_cli::VineCommand;

fn main() -> Result<()> {
  VineCommand::execute()
}
