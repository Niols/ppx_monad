open Ppxlib

let mk_return ~loc x =
  [%expr Some [%e x]]

let mk_bind ~loc e f =
  let px, x = Ppx_monad.fresh_variable () in
  [%expr
    match [%e e] with
    | Some [%p px] -> [%e f] [%e x]
    | None -> None]

let mk_fail ~loc x =
  [%expr ignore [%e x]; None]

let mk_catch ~loc e f =
  let px, x = Ppx_monad.fresh_variable () in
  [%expr
    match [%e e] with
    | Some [%p px] -> Some [%e x]
    | None -> [%e f] ()]

let () = Ppx_monad.register "option"
    ~applies_on:"opt(ion)?"
    ~mk_return ~mk_bind
    ~mk_fail ~mk_catch
