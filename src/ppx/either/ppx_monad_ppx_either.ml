open Ppxlib

let () = Ppx_monad_lib.register "either.right"
    ~applies_on:"either|(either.)?right"
    ~monad:(Longident.parse "Ppx_monad_std.Either")

let () = Ppx_monad_lib.register "either.left"
    ~applies_on:"(either.)?left"
    ~monad_error:(Longident.parse "Ppx_monad_std.Either")
