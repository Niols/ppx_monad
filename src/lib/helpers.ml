open Ppxlib
open Ast_helper

let fresh_variable =
  let next_fresh_var = ref 0 in
  fun () ->
    incr next_fresh_var;
    let str = "ppx_monad_var_" ^ (string_of_int !next_fresh_var) in
    let loc = !default_loc in
    Pat.var { txt = str; loc },
    Exp.ident { txt = Longident.Lident str; loc }

let does_not_support ?(ppx_name="This PPX") ?requires construct =
  Location.raise_errorf "%s does not support %s%a"
    ppx_name construct
    (fun fmt -> function
       | None -> ()
       | Some requires ->
         Format.fprintf fmt "\n(requires %s)" requires)
    requires

let unwrap_or_does_not_support x ?ppx_name ?requires construct =
  match x with
  | Some x -> x
  | _ -> does_not_support ?ppx_name ?requires construct

let unwrap2_or_does_not_support xs ?ppx_name ?requires construct =
  match xs with
  | Some x1, Some x2 -> x1, x2
  | _ -> does_not_support ?ppx_name ?requires construct

let unwrap3_or_does_not_support xs ?ppx_name ?requires construct =
  match xs with
  | Some x1, Some x2, Some x3 -> x1, x2, x3
  | _ -> does_not_support ?ppx_name ?requires construct

let add_catchall_if_needed ~loc ?mk_return cases =
  let is_catchall case =
    let rec is_pat_catchall pat =
      match pat.ppat_desc with
      | Ppat_any | Ppat_var _ -> true
      | Ppat_alias (pat, _) | Ppat_constraint (pat,_) -> is_pat_catchall pat
      | _ -> false
    in
    case.pc_guard = None
    && is_pat_catchall case.pc_lhs
  in
  if List.exists is_catchall cases then
    cases
  else
    match mk_return with
    | None -> does_not_support "cases without catch-all"
    | Some mk_return ->
      cases @ [Exp.case [%pat? any] (mk_return ~loc [%expr any])]
