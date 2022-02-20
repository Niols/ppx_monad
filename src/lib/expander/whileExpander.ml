open Ppxlib
open Ast_helper
open Helpers

let mk
    ?monad ?monad_error
    ?mk_return ?mk_bind ?mk_fail ?mk_catch
    ~loc e1 e2
  =
  let mk_return = first_or_does_not_support "while" [
      mk_return;
      Common.mk_fail_of_monad <$> monad_error;
      Common.mk_return_of_monad <$> monad;
    ]
  in
  let mk_bind = first_or_does_not_support "while" [
      mk_bind;
      Common.mk_catch_of_monad <$> monad_error;
      Common.mk_bind_of_monad <$> monad;
    ]
  in
  ignore mk_fail;
  ignore mk_catch;
  (* while% e1 do e2 done
     =>
     let while_ () =
       e1 >>= function
         | true -> bind e2 while_
         | false -> return () *)
  let pwhile, ewhile = fresh_variable () in
  [%expr
    let rec [%p pwhile] = fun () ->
      [%e mk_bind ~loc e1 Exp.(function_ [
          case [%pat? true] (mk_bind ~loc e2 ewhile) ;
          case [%pat? false] (mk_return ~loc [%expr ()])
        ])]
    in [%e ewhile] ()]
