open Ppxlib

let mk_return ~loc x =
  [%expr Some [%e x]]

let mk_bind ~loc e f =
  let (px, x) = Ppx_monad_lib.fresh_variable () in
  [%expr
    match [%e e] with
    | Some [%p px] -> [%e f] [%e x]
    | None -> None]

let mk_fail ~loc e =
  [%expr let () = [%e e] in None]

let mk_catch ~loc e f =
  let (px, x) = Ppx_monad_lib.fresh_variable () in
  [%expr
    match [%e e] with
    | Some [%p px] -> Some [%e x]
    | None -> [%e f] ()]

let () = Ppx_monad_lib.register "option"
    ~applies_on:"opt(ion)?"
    ~mk_return ~mk_bind
    ~mk_fail ~mk_catch
