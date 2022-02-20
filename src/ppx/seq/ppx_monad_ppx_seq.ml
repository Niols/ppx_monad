open Ppxlib

let () = Ppx_monad_lib.register "seq"
    ~monad:(Longident.parse "Ppx_monad_std.Seq")
