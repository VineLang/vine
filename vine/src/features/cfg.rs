use std::collections::HashMap;

use vine_util::parser::Parser;

use crate::{
  components::{
    charter::Charter,
    lexer::Token,
    parser::{BP, VineParser},
  },
  structures::{
    ast::{Attr, AttrKind, Cfg, CfgKind, Ident, Span},
    diag::{Diag, ErrorGuaranteed},
  },
};

#[derive(Default)]
pub struct Config {
  values: HashMap<Ident, ConfigValue>,
}

impl Config {
  pub fn insert(&mut self, name: Ident, value: ConfigValue) {
    self.values.insert(name, value);
  }
}

pub enum ConfigValue {
  Bool(bool),
}

impl VineParser<'_> {
  pub(crate) fn parse_cfg(&mut self) -> Result<Cfg, Diag> {
    self.parse_cfg_bp(BP::Min)
  }

  fn parse_cfg_bp(&mut self, bp: BP) -> Result<Cfg, Diag> {
    let span = self.start_span();
    let kind = self.parse_cfg_prefix(bp)?;
    let mut cfg = Cfg { span: self.end_span(span), kind: Box::new(kind) };
    loop {
      cfg = match self.parse_cfg_postfix(cfg, bp)? {
        Ok(kind) => Cfg { span: self.end_span(span), kind: Box::new(kind) },
        Err(cfg) => return Ok(cfg),
      }
    }
  }

  fn parse_cfg_prefix(&mut self, _: BP) -> Result<CfgKind, Diag> {
    if self.check(Token::Ident) {
      let name = self.parse_ident()?;
      return Ok(CfgKind::Bool(name));
    }

    if self.eat(Token::Bang)? {
      let inner = self.parse_cfg_bp(BP::Prefix)?;
      return Ok(CfgKind::Not(inner));
    }

    if self.eat(Token::True)? {
      return Ok(CfgKind::Literal(true));
    }

    if self.eat(Token::False)? {
      return Ok(CfgKind::Literal(false));
    }

    self.unexpected()
  }

  fn parse_cfg_postfix(&mut self, lhs: Cfg, bp: BP) -> Result<Result<CfgKind, Cfg>, Diag> {
    if bp.permits(BP::LogicalAnd) && self.eat(Token::And)? {
      let rhs = self.parse_cfg_bp(BP::LogicalAnd)?;
      return Ok(Ok(CfgKind::And(lhs, rhs)));
    }

    if bp.permits(BP::LogicalOr) && self.eat(Token::Or)? {
      let rhs = self.parse_cfg_bp(BP::LogicalOr)?;
      return Ok(Ok(CfgKind::Or(lhs, rhs)));
    }

    Ok(Err(lhs))
  }
}

impl Charter<'_> {
  pub fn enabled(&mut self, attrs: &[Attr]) -> bool {
    attrs.iter().all(|attr| match &attr.kind {
      AttrKind::Cfg(cfg) => self.eval_cfg(cfg) == Ok(true),
      _ => true,
    })
  }

  pub fn eval_cfg(&mut self, cfg: &Cfg) -> Result<bool, ErrorGuaranteed> {
    let span = cfg.span;
    match &*cfg.kind {
      CfgKind::Bool(name) => match self.get_cfg(span, name.clone())? {
        ConfigValue::Bool(bool) => Ok(*bool),
      },
      CfgKind::Paren(cfg) => self.eval_cfg(cfg),
      CfgKind::Literal(bool) => Ok(*bool),
      CfgKind::And(lhs, rhs) => {
        let lhs = self.eval_cfg(lhs);
        let rhs = self.eval_cfg(rhs);
        Ok(lhs? & rhs?)
      }
      CfgKind::Or(lhs, rhs) => {
        let lhs = self.eval_cfg(lhs);
        let rhs = self.eval_cfg(rhs);
        Ok(lhs? | rhs?)
      }
      CfgKind::Not(cfg) => Ok(!self.eval_cfg(cfg)?),
    }
  }

  pub fn get_cfg(&mut self, span: Span, name: Ident) -> Result<&ConfigValue, ErrorGuaranteed> {
    self.config.values.get(&name).ok_or_else(|| self.diags.report(Diag::UnknownCfg { span, name }))
  }
}
