open Ppxlib
open Ast_helper
open Helpers

let mk
    ?monad ?monad_error
    ?mk_return ?mk_bind ?mk_fail ?mk_catch
    ~loc rec_flag vbs e
  =
  let mk_return = first [
      mk_return;
      Common.mk_fail_of_monad <$> monad_error;
      Common.mk_return_of_monad <$> monad;
    ]
  in
  let mk_bind = first_or_does_not_support "let" [
      mk_bind;
      Common.mk_catch_of_monad <$> monad_error;
      Common.mk_bind_of_monad <$> monad;
    ]
  in
  ignore mk_fail;
  ignore mk_catch;
  let mk_and ~loc e1 e2 =
    (* e1 >>= fun v1 ->
       e2 >>= fun v2 ->
       return (v1, v2) *)
    let mk_return = unwrap_or_does_not_support "and" mk_return in
    let (pv1, v1) = fresh_variable () in
    let (pv2, v2) = fresh_variable () in
    mk_bind ~loc e1
      [%expr fun [%p pv1] ->
        [%e mk_bind ~loc e2 [%expr fun [%p pv2] ->
            [%e mk_return ~loc [%expr ([%e v1], [%e v2])]]]]]
  in
  (* let% x1 = e1 and x2 = e2 ... and xn = en in e
     =>
     let% (...(x1, x2), ... xn) = (...(e1, e2), ... en) in e
     except we do not build (...(e1, e2), ... en) ourselves but we ask mk_and, so:
     let% (...(x1, x2), ... xn) = (mk_and ... (mk_and e1 e2) en) in e
  *)
  assert_or_does_not_support "recursive let" (rec_flag = Nonrecursive);
  let ands =
    List.fold_left
      (fun ands vb -> mk_and ~loc ands vb.pvb_expr)
      (List.hd vbs).pvb_expr
      (List.tl vbs)
  in
  let pats =
    List.fold_left
      (fun pats vb -> Pat.tuple [pats; vb.pvb_pat])
      (List.hd vbs).pvb_pat
      (List.tl vbs)
  in
  mk_bind ~loc ands [%expr fun [%p pats] -> [%e e]]
