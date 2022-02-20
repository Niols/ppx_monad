open Ppxlib

let () = Ppx_monad_lib.register "result.ok"
    ~applies_on:"ok|res(ult)?(.ok)?"
    ~monad:(Longident.parse "Ppx_monad_std.Result")

let () = Ppx_monad_lib.register "result.error"
    ~applies_on:"(res(ult)?.)?err(or)?"
    ~monad_error:(Longident.parse "Ppx_monad_std.Result")
