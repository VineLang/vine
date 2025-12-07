use vine_util::parser::Parse;

use crate::{
  components::{lexer::Token, parser::Parser},
  structures::{
    ast::{Ident, Stmt},
    diag::Diag,
  },
};

pub enum ReplCommand {
  Help,
  Scope,
  Clear(Vec<Ident>),
  Set(ReplOption),
  Run(Vec<Stmt>),
}

pub struct ReplOptions {
  pub show_scope: bool,
}

impl Default for ReplOptions {
  fn default() -> Self {
    Self { show_scope: true }
  }
}

pub enum ReplOption {
  ShowScope(bool),
}

pub const HELP: &str = "
/help                   print this message
/scope                  print all variables in scope
/clear [variables...]   unbind local variables

/set                    set option
/set show_scope [bool]  enable/disable printing the scope before each command
";

impl<'src> Parser<'src> {
  pub(super) fn parse_repl_command(&mut self) -> Result<ReplCommand, Diag> {
    Ok(if self.eat(Token::Slash)? {
      let span = self.span();
      let command = self.expect(Token::Ident)?;
      match command {
        "help" => ReplCommand::Help,
        "scope" => ReplCommand::Scope,
        "clear" => {
          let mut vars = Vec::new();
          while self.state.token.is_some() {
            vars.push(self.parse_ident()?);
          }
          ReplCommand::Clear(vars)
        }
        "set" => {
          let span = self.span();
          let option = self.expect(Token::Ident)?;
          ReplCommand::Set(match option {
            "show_scope" => ReplOption::ShowScope(self.parse_bool()?),
            _ => Err(Diag::InvalidCommand { span })?,
          })
        }
        _ => Err(Diag::InvalidCommand { span })?,
      }
    } else {
      let mut stmts = Vec::new();
      while self.state.token.is_some() {
        stmts.push(self.parse_stmt()?);
      }
      ReplCommand::Run(stmts)
    })
  }

  fn parse_bool(&mut self) -> Result<bool, Diag> {
    if self.eat(Token::True)? {
      Ok(true)
    } else if self.eat(Token::False)? {
      Ok(false)
    } else {
      self.unexpected()
    }
  }
}
