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

let rec first = function
  | [] -> None
  | None :: rest -> first rest
  | Some res :: _ -> Some res

let (<$>) = Option.map

let does_not_support ?(ppx_name="This PPX") feature =
  Location.raise_errorf "%s does not support %s"
    ppx_name feature

let unwrap_or_does_not_support ?ppx_name feature = function
  | Some x -> x
  | _ -> does_not_support ?ppx_name feature

let first_or_does_not_support ?ppx_name feature options =
  unwrap_or_does_not_support ?ppx_name feature (first options)

let assert_or_does_not_support ?ppx_name feature bool =
  if not bool then does_not_support ?ppx_name feature

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
