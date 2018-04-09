#[macro_use]
extern crate log;
extern crate pretty_env_logger;

use std::fs;
use std::io::Read;
use std::ffi::OsStr;

#[test]
fn parse_spec() {
    pretty_env_logger::init();

    for entry in fs::read_dir("spec/testsuite").unwrap() {
        if let Ok(entry) = entry {
            if entry.file_type().unwrap().is_file() && entry.path().extension() == Some(OsStr::new("wast")) {
                info!("parsing file: {:?}", entry.path());

                let mut file = fs::File::open(entry.path()).unwrap();
                let mut content = vec![];

                file.read_to_end(&mut content).unwrap();
            }
        }
    }
}
