use super::*;

use crate::component::{
  grammar::{self, Grammar, GrammarRule},
  lexer::{Diag, Token},
  parser::{Parse, ParseAttr, ParsePratt, Parser, Prec, PrecCtx},
};

use vine_util::parser::Parse as _;

pub mod control;
pub mod item;
pub mod path;

pub mod annotation;
pub mod builtin;
pub mod call;
pub mod children;
pub mod comment;
pub mod enum_;
pub mod field;
pub mod flex;
pub mod generics;
pub mod hole;
pub mod inline_ivy;
pub mod inverse;
pub mod lint;
pub mod list;
pub mod literal;
pub mod loan;
pub mod method;
pub mod object;
pub mod operator;
pub mod placement;
pub mod read;
pub mod sequence;
pub mod string;
pub mod tuple;
pub mod underscore;
pub mod unsafe_;
pub mod unwrap;
