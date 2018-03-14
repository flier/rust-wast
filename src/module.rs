use parity_wasm::elements::Module;

named!(
    module<Module>,
    do_parse!(tag!("module") >> (Module::default()))
);
