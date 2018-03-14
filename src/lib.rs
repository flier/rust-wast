#[macro_use]
extern crate failure;
#[macro_use]
extern crate log;
#[macro_use]
extern crate nom;
extern crate parity_wasm;

#[cfg(test)]
#[macro_use]
extern crate lazy_static;
#[cfg(test)]
extern crate pretty_env_logger;

mod errors;
#[macro_use]
mod parse;
mod ops;
mod module;

pub use errors::WastError;
