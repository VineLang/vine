use anyhow::Result;
use vine_cli::ivm_cli::IvmCommand;

fn main() -> Result<()> {
  IvmCommand::execute()
}
