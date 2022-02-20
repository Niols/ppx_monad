open Ppxlib
open Ast_helper
open Helpers

let mk
    ?monad ?monad_error
    ?mk_return ?mk_bind ?mk_fail ?mk_catch
    ~loc e
  =
  let mk_return = first_or_does_not_support "assert" [
      mk_return;
      Common.mk_fail_of_monad <$> monad_error;
      Common.mk_return_of_monad <$> monad;
    ]
  in
  let mk_bind = first_or_does_not_support "assert" [
      mk_bind;
      Common.mk_catch_of_monad <$> monad_error;
      Common.mk_bind_of_monad <$> monad;
    ]
  in
  let mk_fail = first_or_does_not_support "assert" [
      mk_fail;
      Common.mk_fail_of_monad <$> monad;
    ]
  in
  ignore mk_catch;
  (* assert% false   =>   try assert false with exn -> fail exn *)
  (* assert% e       =>   e >>= function
                            | true -> return ()
                            | false -> try assert false with exn -> fail exn *)
  let assert_false =
    let pexn, eexn = fresh_variable () in
    [%expr try assert false with [%p pexn] -> [%e mk_fail ~loc eexn]]
  in
  match e with
  | [%expr false] -> assert_false
  | e ->
    mk_bind ~loc e Exp.(function_ [
        case [%pat? true] (mk_return ~loc [%expr ()]);
        case [%pat? false] assert_false
      ])
