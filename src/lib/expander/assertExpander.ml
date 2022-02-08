open Ppxlib
open Ast_helper

let mk
    ?mk_return ?mk_bind ?mk_fail ?mk_catch
    ~loc e
  =
  ignore mk_catch;
  let (mk_return, mk_bind, mk_fail) =
    Helpers.unwrap3_or_does_not_support (mk_return, mk_bind, mk_fail)
      "assert" ~requires:"mk_return+mk_bind+mk_fail"
  in
  (* assert% false   =>   try assert false with exn -> fail exn *)
  (* assert% e       =>   e >>= function
                            | true -> return ()
                            | false -> try assert false with exn -> fail exn *)
  let assert_false =
    let pexn, eexn = Helpers.fresh_variable () in
    [%expr try assert false with [%p pexn] -> [%e mk_fail ~loc eexn]]
  in
  match e with
  | [%expr false] -> assert_false
  | e ->
    mk_bind ~loc e Exp.(function_ [
        case [%pat? true] (mk_return ~loc [%expr ()]);
        case [%pat? false] assert_false
      ])
