let () = Ppx_monad_lib.register "list"
    ~applies_on:"lst|list"
    ~monad:"Ppx_monad_std.List"
