open Ppxlib
open Ast_helper
open Helpers

let mk
    ?monad ?monad_error
    ?mk_return ?mk_bind ?mk_fail ?mk_catch
    ~loc e1 e2 e3
  =
  let mk_return = first [
      mk_return;
      Common.mk_fail_of_monad <$> monad_error;
      Common.mk_return_of_monad <$> monad;
    ]
  in
  let mk_bind = first_or_does_not_support "if-then-else" [
      mk_bind;
      Common.mk_catch_of_monad <$> monad_error;
      Common.mk_bind_of_monad <$> monad;
    ]
  in
  ignore mk_fail;
  ignore mk_catch;
  match e3 with
  | None ->
    (
      let mk_return = unwrap_or_does_not_support "if-then with no else" mk_return in
      (* e1 >>= function true -> e2 | false -> return () *)
      mk_bind ~loc e1 Exp.(function_ [
          case [%pat? true] e2;
          case [%pat? false] (mk_return ~loc [%expr ()])
        ])
    )
  | Some e3 ->
    (* e1 >>= function true -> e2 | false -> e3 *)
    mk_bind ~loc e1 Exp.(function_ [case [%pat? true] e2; case [%pat? false] e3])
