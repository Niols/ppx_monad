let () = Ppx_monad_lib.register "either.right"
    ~applies_on:"either|(either.)?right"
    ~monad:"Ppx_monad_std.Either"

let () = Ppx_monad_lib.register "either.left"
    ~applies_on:"(either.)?left"
    ~monad_error:"Ppx_monad_std.Either"
