open Ppxlib

let mk_return ~loc x =
  [%expr Stdlib.Either.Right [%e x]]

let mk_bind ~loc e f =
  [%expr
    (fun e f -> match e with
       | Stdlib.Either.Left y -> Stdlib.Either.Left y
       | Stdlib.Either.Right x -> f x) [%e e] [%e f]]

let mk_fail ~loc y =
  [%expr Stdlib.Result.Left [%e y]]

let mk_catch ~loc e f =
  [%expr
    (fun e f -> match e with
       | Stdlib.Either.Right x -> Stdlib.Either.Right x
       | Stdlib.Either.Left y -> f y) [%e e] [%e f]]

let () = Ppx_monad.register "either.right"
    ~applies_on:"either|(either.)?right"
    ~mk_return ~mk_bind
    ~mk_fail ~mk_catch

let () = Ppx_monad.register "either.left"
    ~applies_on:"(either.)?left"
    ~mk_return:mk_fail
    ~mk_bind:mk_catch
