open Ppxlib
open Ast_helper

let mk
    ?mk_return ?mk_bind ?mk_fail ?mk_catch
    ~loc rec_flag vbs e
  =
  ignore mk_fail; ignore mk_catch;
  let mk_bind = Helpers.unwrap_or_does_not_support mk_bind
      "let" ~requires:"mk_bind"
  in
  let mk_and ~loc e1 e2 =
    (* e1 >>= fun v1 ->
       e2 >>= fun v2 ->
       return (v1, v2) *)
    match mk_return with
    | None -> Helpers.does_not_support "and" ~requires:"mk_return+mk_bind"
    | Some mk_return ->
      let (pv1, v1) = Helpers.fresh_variable () in
      let (pv2, v2) = Helpers.fresh_variable () in
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
  if rec_flag <> Nonrecursive then
    Helpers.does_not_support "recursive let";
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
