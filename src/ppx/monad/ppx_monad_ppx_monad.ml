open Ppxlib

let mk_return ~loc x =
  [%expr return [%e x]]

let mk_bind ~loc e f =
  [%expr bind [%e e] [%e f]]

let mk_fail ~loc x =
  [%expr fail [%e x]]

let mk_catch ~loc e f =
  [%expr catch [%e e] [%e f]]

let () = Ppx_monad_lib.register "monad"
    ~applies_on:"m|monad"
    ~mk_return ~mk_bind
    ~mk_fail ~mk_catch
