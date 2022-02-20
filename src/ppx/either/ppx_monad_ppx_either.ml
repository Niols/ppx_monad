open Ppxlib

let () = Ppx_monad_lib.register "either.right"
    ~applies_on:"either|(either.)?right"
    ~monad:(Longident.parse "Ppx_monad_std.Either")

let mk_fail ~loc y =
  [%expr Ppx_monad_std.Either.fail [%e y]]

let mk_catch ~loc e f =
  [%expr Ppx_monad_std.Either.catch [%e e] [%e f]]

let () = Ppx_monad_lib.register "either.left"
    ~applies_on:"(either.)?left"
    ~mk_return:mk_fail
    ~mk_bind:mk_catch
