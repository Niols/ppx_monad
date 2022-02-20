open Ppxlib
open Helpers

let mk
    ?monad ?monad_error
    ?mk_return ?mk_bind ?mk_fail ?mk_catch
    ~loc e1 e2
  =
  ignore mk_return;
  let mk_bind = first_or_does_not_support "sequence" [
      mk_bind;
      Common.mk_catch_of_monad <$> monad_error;
      Common.mk_bind_of_monad <$> monad;
    ]
  in
  ignore mk_fail;
  ignore mk_catch;
  (* FIXME: this forces the first element of a sequence to be of type unit, but
     this is usually just a warning in OCaml. Maybe there is a way to do that? *)
  mk_bind ~loc e1 [%expr fun () -> [%e e2]]
