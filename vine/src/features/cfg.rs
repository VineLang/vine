use std::collections::{HashMap, HashSet};

use vine_util::parser::Parser;

use crate::{
  components::{
    charter::Charter,
    lexer::Token,
    parser::{VineParser, BP},
  },
  structures::{
    ast::{Attr, AttrKind, Cfg, CfgKind, Ident, Span},
    diag::{Diag, ErrorGuaranteed},
  },
};

#[derive(Default)]
pub struct Config<'core> {
  values: HashMap<Ident<'core>, ConfigValue>,
}

impl<'core> Config<'core> {
  pub fn insert(&mut self, name: Ident<'core>, value: ConfigValue) {
    self.values.insert(name, value);
  }
}

pub enum ConfigValue {
  Bool(bool),
  Str(String),
  StrPred(HashSet<String>),
}

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_cfg(&mut self) -> Result<Cfg<'core>, Diag<'core>> {
    self.parse_cfg_bp(BP::Min)
  }

  fn parse_cfg_bp(&mut self, bp: BP) -> Result<Cfg<'core>, Diag<'core>> {
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

  fn parse_cfg_prefix(&mut self, bp: BP) -> Result<CfgKind<'core>, Diag<'core>> {
    if self.check(Token::Ident) {
      let name = self.parse_ident()?;
      if bp.permits(BP::Comparison) {
        if self.eat(Token::EqEq)? {
          let str = self.parse_string()?;
          return Ok(CfgKind::Str(name, false, str));
        }
        if self.eat(Token::Ne)? {
          let str = self.parse_string()?;
          return Ok(CfgKind::Str(name, true, str));
        }
      }
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

  fn parse_cfg_postfix(
    &mut self,
    lhs: Cfg<'core>,
    bp: BP,
  ) -> Result<Result<CfgKind<'core>, Cfg<'core>>, Diag<'core>> {
    if bp.permits(BP::LogicalAnd) && self.eat(Token::AndAnd)? {
      let rhs = self.parse_cfg_bp(BP::LogicalAnd)?;
      return Ok(Ok(CfgKind::And(lhs, rhs)));
    }

    if bp.permits(BP::LogicalOr) && self.eat(Token::OrOr)? {
      let rhs = self.parse_cfg_bp(BP::LogicalOr)?;
      return Ok(Ok(CfgKind::Or(lhs, rhs)));
    }

    Ok(Err(lhs))
  }
}

impl<'core> Charter<'core, '_> {
  pub fn enabled(&self, attrs: &[Attr<'core>]) -> bool {
    attrs.iter().all(|attr| match &attr.kind {
      AttrKind::Cfg(cfg) => self.eval_cfg(cfg) == Ok(true),
      _ => true,
    })
  }

  pub fn eval_cfg(&self, cfg: &Cfg<'core>) -> Result<bool, ErrorGuaranteed> {
    let span = cfg.span;
    match &*cfg.kind {
      CfgKind::Bool(name) => match self.get_cfg(span, *name)? {
        ConfigValue::Bool(bool) => Ok(*bool),
        _ => Err(self.core.report(Diag::BadCfgType { span, name: *name, kind: "boolean" })),
      },
      CfgKind::Str(name, inv, str) => match self.get_cfg(span, *name)? {
        ConfigValue::Str(value) => Ok((value == str) ^ inv),
        _ => Err(self.core.report(Diag::BadCfgType { span, name: *name, kind: "string" })),
      },
      CfgKind::StrPred(name, str) => match self.get_cfg(span, *name)? {
        ConfigValue::StrPred(set) => Ok(set.contains(str)),
        _ => {
          Err(self.core.report(Diag::BadCfgType { span, name: *name, kind: "string predicate" }))
        }
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

  pub fn get_cfg(&self, span: Span, name: Ident<'core>) -> Result<&ConfigValue, ErrorGuaranteed> {
    self.config.values.get(&name).ok_or_else(|| self.core.report(Diag::UnknownCfg { span, name }))
  }
}
