use vine_util::parser::Parser;

use crate::{
  components::{lexer::Token, parser::VineParser},
  structures::{
    ast::{Ident, Stmt},
    diag::Diag,
  },
};

pub enum ReplCommand<'core> {
  Help,
  Scope,
  Clear(Vec<Ident<'core>>),
  Set(ReplOption),
  Run(Vec<Stmt<'core>>),
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

impl<'core, 'src> VineParser<'core, 'src> {
  pub(super) fn parse_repl_command(&mut self) -> Result<ReplCommand<'core>, Diag<'core>> {
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

  fn parse_bool(&mut self) -> Result<bool, Diag<'core>> {
    if self.eat(Token::True)? {
      Ok(true)
    } else if self.eat(Token::False)? {
      Ok(false)
    } else {
      self.unexpected()
    }
  }
}
