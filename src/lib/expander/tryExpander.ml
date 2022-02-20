open Ppxlib
open Ast_helper
open Helpers

let mk
    ?monad ?monad_error
    ?mk_return ?mk_bind ?mk_fail ?mk_catch
    ~loc e cases
  =
  ignore monad_error;
  ignore mk_return;
  ignore mk_bind;
  let mk_catch = first_or_does_not_support "try" [
      mk_catch;
      Common.mk_catch_of_monad <$> monad;
    ]
  in
  let cases = add_catchall_if_needed ~loc ?mk_return:mk_fail cases in
  mk_catch ~loc e (Exp.function_ cases)
