use std::collections::HashMap;

use nom::{ErrorKind, IResult};
use nom::{generate_colors, prepare_errors, print_codes, print_offsets};

#[derive(Debug, Fail)]
pub enum WastError {
    #[fail(display = "out of range: {}", _0)] OutOfRange(isize),
    #[fail(display = "not found: {}", _0)] NotFound(String),
}

#[repr(u32)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Parsing {
    Num,
}

impl From<Parsing> for ErrorKind {
    fn from(v: Parsing) -> Self {
        ErrorKind::Custom(v as u32)
    }
}

pub fn trace_parse_error<O>(input: &[u8], res: IResult<&[u8], O>) {
    let mut values = HashMap::new();

    values.insert(Parsing::Num as u32, "num");

    if let Some(v) = prepare_errors(input, res) {
        let colors = generate_colors(&v);

        trace!(
            "parser `{}` failed\n{}",
            print_codes(colors, values),
            print_offsets(input, 0, &v)
        );
    }
}

#[macro_export]
macro_rules! trace_parse_error {
    ($input:expr, $res:expr) => {
        if $res.is_err() {
            $crate::errors::trace_parse_error($input, $res.clone())
        }
    };
}
