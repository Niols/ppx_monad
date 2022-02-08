open Ppxlib
open Ast_helper

let mk
    ?mk_return ?mk_bind ?mk_fail ?mk_catch
    ~loc e1 e2
  =
  ignore mk_fail; ignore mk_catch;
  let (mk_return, mk_bind) =
    Helpers.unwrap2_or_does_not_support (mk_return, mk_bind)
      "while" ~requires:"mk_return+mk_bind"
  in
  (* while% e1 do e2 done
     =>
     let while_ () =
       e1 >>= function
         | true -> bind e2 while_
         | false -> return () *)
  let pwhile, ewhile = Helpers.fresh_variable () in
  [%expr
    let rec [%p pwhile] = fun () ->
      [%e mk_bind ~loc e1 Exp.(function_ [
          case [%pat? true] (mk_bind ~loc e2 ewhile) ;
          case [%pat? false] (mk_return ~loc [%expr ()])
        ])]
    in [%e ewhile] ()]
