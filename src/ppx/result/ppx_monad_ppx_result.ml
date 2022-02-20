open Ppxlib

let mk_return ~loc x =
  [%expr Stdlib.Result.Ok [%e x]]

let mk_bind ~loc e f =
  [%expr Stdlib.Result.bind [%e e] [%e f]]

let mk_fail ~loc y =
  [%expr Stdlib.Result.Error [%e y]]

let mk_catch ~loc e f =
  [%expr
    (fun e f -> match e with
       | Stdlib.Result.Ok x -> Stdlib.Result.Ok x
       | Stdlib.Result.Error y -> f y) [%e e] [%e f]]

let () = Ppx_monad_lib.register "result.ok"
    ~applies_on:"ok|res(ult)?(.ok)?"
    ~mk_return ~mk_bind
    ~mk_fail ~mk_catch

let () = Ppx_monad_lib.register "result.error"
    ~applies_on:"(res(ult)?.)?err(or)?"
    ~mk_return:mk_fail
    ~mk_bind:mk_catch
