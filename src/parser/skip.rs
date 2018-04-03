use super::{comment, whitespace};

named!(pub skip, recognize!(many0!(alt!(comment | whitespace))));
