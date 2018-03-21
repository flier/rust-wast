#![recursion_limit = "128"]
#![allow(dead_code, unused_variables)]

#[macro_use]
extern crate failure;
extern crate itertools;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
#[macro_use]
extern crate nom;
extern crate parity_wasm;
extern crate regex;

#[cfg(test)]
extern crate pretty_env_logger;

#[macro_use]
mod errors;
mod ast;
#[macro_use]
mod parse;
mod ops;
mod func;
mod module;

pub use errors::WastError;
