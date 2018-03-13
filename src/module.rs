use parse::Parse;

use parity_wasm::elements::Module;

impl Parse for Module {
    named!(
        parse<Module>,
        do_parse!(tag!("module") >> (Module::default()))
    );
}
