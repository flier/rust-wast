#[macro_use]
extern crate failure;
extern crate getopts;
#[macro_use]
extern crate log;
extern crate parity_wasm;
extern crate pretty_env_logger;

use std::env;
use std::process;

use failure::Error;
use getopts::{Matches, Options};

fn parse_cmdline(program: &str, args: &[String]) -> Result<Matches, Error> {
    let mut opts = Options::new();

    opts.optopt("e", "expr", "evaluate string", "EXPR");
    opts.optflag("h", "help", "print this help menu");

    let matches = opts.parse(args)?;

    if matches.opt_present("h") {
        let brief = format!("Usage: {} [options] FILE", program);

        print!("{}", opts.usage(&brief));

        process::exit(0);
    } else {
        Ok(matches)
    }
}

fn main() {
    pretty_env_logger::init();

    let args = env::args().collect::<Vec<_>>();
    let (program, args) = args.split_first().unwrap();

    parse_cmdline(program, args)
        .and_then(|matches| {
            if let Some(expr) = matches.opt_str("expr") {
                Ok(())
            } else if let Some(filename) = matches.free.first() {
                Ok(())
            } else {
                bail!("missed filename or expression")
            }
        })
        .unwrap();
}
