use super::string;

named!(
    pub inline_import<(String, String)>,
    delimited!(
        tag!("("),
        preceded!(first!(tag!("import")), pair!(first!(string), first!(string))),
        tag!(")")
    )
);
