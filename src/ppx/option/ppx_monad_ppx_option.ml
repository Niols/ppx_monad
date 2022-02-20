let () = Ppx_monad_lib.register "option"
    ~applies_on:"opt(ion)?"
    ~monad:"Ppx_monad_std.Option"
