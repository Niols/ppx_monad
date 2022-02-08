open Ppxlib
open Ast_helper

let mk
    ?mk_return ?mk_bind ?mk_fail ?mk_catch
    ~loc e1 e2 e3
  =
  ignore mk_fail; ignore mk_catch;
  let mk_bind = Helpers.unwrap_or_does_not_support mk_bind
      "if-then-else" ~requires:"mk_bind"
  in
  match e3 with
  | None ->
    (
      let mk_return = Helpers.unwrap_or_does_not_support mk_return
          "if-then with no else" ~requires:"mk_return+mk_bind"
      in
      (* e1 >>= function true -> e2 | false -> return () *)
      mk_bind ~loc e1 Exp.(function_ [
          case [%pat? true] e2;
          case [%pat? false] (mk_return ~loc [%expr ()])
        ])
    )
  | Some e3 ->
    (* e1 >>= function true -> e2 | false -> e3 *)
    mk_bind ~loc e1 Exp.(function_ [case [%pat? true] e2; case [%pat? false] e3])
