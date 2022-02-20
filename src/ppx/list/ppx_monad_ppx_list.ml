open Ppxlib

let mk_return ~loc x =
  [%expr [[%e x]]]

let mk_bind ~loc e f =
  [%expr Stdlib.List.flatten (Stdlib.List.map [%e f] [%e e])]

let () = Ppx_monad_lib.register "list"
    ~applies_on:"lst|list"
    ~mk_return ~mk_bind
