let () = Ppx_monad_lib.register "result.ok"
    ~applies_on:"ok|res(ult)?(.ok)?"
    ~monad:"Ppx_monad_std.Result"

let () = Ppx_monad_lib.register "result.error"
    ~applies_on:"(res(ult)?.)?err(or)?"
    ~monad_error:"Ppx_monad_std.Result"
