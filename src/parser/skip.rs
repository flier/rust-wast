use super::comment;

named!(pub skip, recognize!(many0!(alt!(comment | whitespace))));

named!(whitespace, is_a!(" \t\n\r"));
