open Ppxlib

let mk_return ~loc x =
  [%expr Stdlib.Option.Some [%e x]]

let mk_bind ~loc e f =
  [%expr Stdlib.Option.bind [%e e] [%e f]]

let mk_fail ~loc x =
  [%expr let () = [%e x] in Stdlib.Option.None]

let mk_catch ~loc e f =
  [%expr
    (fun e f -> match e with
       | Stdlib.Option.Some x -> Stdlib.Option.Some x
       | Stdlib.Option.None -> f ()) [%e e] [%e f]]

let () = Ppx_monad_lib.register "option"
    ~applies_on:"opt(ion)?"
    ~mk_return ~mk_bind
    ~mk_fail ~mk_catch
