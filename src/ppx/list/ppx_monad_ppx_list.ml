open Ppxlib

let () = Ppx_monad_lib.register "list"
    ~applies_on:"lst|list"
    ~monad:(Longident.parse "Ppx_monad_std.List")
