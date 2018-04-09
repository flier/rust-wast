use super::{action, assertion, name, opt_bind_var, script_module, string, inline_module1, INPUT, LPAR, OUTPUT,
            REGISTER, RPAR, SCRIPT};
use ast::{Cmd, Meta, Var, Script};

named!(script_var_opt<Option<Var>>, call!(opt_bind_var));

named!(
    pub script<Script>,
    alt!(
        cmd_list        => { |cmds|     Script::Commands(cmds) } |
        inline_module1  => { |fields|   Script::InlineModule(fields) }
    )
);

named!(cmd_list<Vec<Cmd>>, many0!(first!(cmd)));

named!(
    cmd<Cmd>,
    parsing!(Cmd,
        alt!(
            action          => { |action|        Cmd::Action(action) } |
            assertion       => { |assertion|     Cmd::Assertion(assertion) } |
            script_module   => { |script_module| Cmd::ScriptModule(script_module) } |
            register        => { |(id, bind)|    Cmd::Register(id, bind) } |
            meta            => { |meta|          Cmd::Meta(meta) }
        )
    )
);

named!(
    register<(String, Option<Var>)>,
    delimited!(
        LPAR,
        preceded!(REGISTER, tuple!(first!(name), opt_bind_var)),
        RPAR
    )
);

named!(
    meta<Meta>,
    parsing!(Meta,
        delimited!(
            LPAR,
            alt!(
                preceded!(SCRIPT, tuple!(script_var_opt, cmd_list)) => { |(id, cmds)|
                    Meta::Script(id, cmds)
                } |
                preceded!(INPUT, tuple!(script_var_opt, first!(string))) => { |(id, input)|
                    Meta::Input(id, input)
                } |
                preceded!(OUTPUT, tuple!(script_var_opt, opt!(first!(string)))) => { |(id, output)|
                    Meta::Output(id, output)
                }
            ),
            RPAR
        )
    )
);
