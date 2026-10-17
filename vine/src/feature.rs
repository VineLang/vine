use super::*;

use crate::component::{
  lexer::{Diag, Token},
  parser::{ParseKind, Parser, Precedence},
};
use vine_util::parser::Parse;

pub mod control;
pub mod item;
pub mod path;

pub mod annotation;
pub mod builtin;
pub mod call;
pub mod enum_;
pub mod field;
pub mod flex;
pub mod generics;
pub mod hole;
pub mod inline_ivy;
pub mod inverse;
pub mod list;
pub mod literal;
pub mod loan;
pub mod method;
pub mod object;
pub mod operator;
pub mod placement;
pub mod read;
pub mod string;
pub mod tuple;
pub mod underscore;
pub mod unsafe_;
pub mod unwrap;
