open Ppxlib

let mk_return ~loc x =
  [%expr Stdlib.Either.Right [%e x]]

let mk_bind ~loc e f =
  let (px, x) = Ppx_monad_lib.fresh_variable () in
  let (py, y) = Ppx_monad_lib.fresh_variable () in
  [%expr
    match [%e e] with
    | Stdlib.Either.Right [%p px] -> [%e f] [%e x]
    | Stdlib.Either.Left [%p py] -> Stdlib.Either.Left [%e y]]

let mk_fail ~loc y =
  [%expr Stdlib.Either.Left [%e y]]

let mk_catch ~loc e f =
  let (px, x) = Ppx_monad_lib.fresh_variable () in
  let (py, y) = Ppx_monad_lib.fresh_variable () in
  [%expr
    match [%e e] with
    | Stdlib.Either.Right [%p px] -> Stdlib.Either.Right [%e x]
    | Stdlib.Either.Left [%p py] -> [%e f] [%e y]]

let () = Ppx_monad_lib.register "either.right"
    ~applies_on:"either|(either.)?right"
    ~mk_return ~mk_bind
    ~mk_fail ~mk_catch

let () = Ppx_monad_lib.register "either.left"
    ~applies_on:"(either.)?left"
    ~mk_return:mk_fail
    ~mk_bind:mk_catch
