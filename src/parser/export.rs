use super::string;

named!(
    pub inline_export<String>,
    delimited!(tag!("("), preceded!(first!(tag!("export")), first!(string)), tag!(")"))
);
