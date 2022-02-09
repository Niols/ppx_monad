open Ppxlib

let mk_return ~loc e =
  ignore loc; e

let mk_bind ~loc e f =
  [%expr [%e f] [%e e]]

let mk_fail ~loc e =
  [%expr raise [%e e]]

let mk_catch ~loc e f =
  let pexn, exn = Ppx_monad.fresh_variable () in
  [%expr try [%e e] with [%p pexn] -> [%e f] [%e exn]]

let () = Ppx_monad.register
    "ppx_nop_monad" ~applies_on:"nop"
    ~mk_return ~mk_bind ~mk_fail ~mk_catch
