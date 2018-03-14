#[derive(Debug, Fail)]
pub enum WastError {
    #[fail(display = "out of range: {}", _0)] OutOfRange(isize),
    #[fail(display = "not found: {}", _0)] NotFound(String),
}
