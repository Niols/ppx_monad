open Ppxlib

let () = Ppx_monad_lib.register "option"
    ~applies_on:"opt(ion)?"
    ~monad:(Longident.parse "Ppx_monad_std.Option")
