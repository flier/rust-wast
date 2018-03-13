#[derive(Debug, Fail)]
pub enum WastError {
    #[fail(display = "out of range: {}", _0)] OutOfRange(isize),
}

pub enum ParseError {
    OutOfRange,
}
