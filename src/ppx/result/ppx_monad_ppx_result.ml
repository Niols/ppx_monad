open Ppxlib

let mk_return ~loc x =
  [%expr Ok [%e x]]

let mk_bind ~loc e f =
  let (px, x) = Ppx_monad_lib.fresh_variable () in
  let (py, y) = Ppx_monad_lib.fresh_variable () in
  [%expr
    match [%e e] with
    | Ok [%p px] -> [%e f] [%e x]
    | Error [%p py] -> Error [%e y]]

let mk_fail ~loc y =
  [%expr Error [%e y]]

let mk_catch ~loc e f =
  let (px, x) = Ppx_monad_lib.fresh_variable () in
  let (py, y) = Ppx_monad_lib.fresh_variable () in
  [%expr
    match [%e e] with
    | Ok [%p px] -> Ok [%e x]
    | Error [%p py] -> [%e f] [%e y]]

let () = Ppx_monad_lib.register "result.ok"
    ~applies_on:"ok|res(ult)?(.ok)?"
    ~mk_return ~mk_bind
    ~mk_fail ~mk_catch

let () = Ppx_monad_lib.register "result.error"
    ~applies_on:"(res(ult)?.)?err(or)?"
    ~mk_return:mk_fail
    ~mk_bind:mk_catch
