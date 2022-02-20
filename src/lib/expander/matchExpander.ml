open Ppxlib
open Ast_helper

let mk_simple ~mk_bind ~loc e cases =
  mk_bind ~loc e (Exp.function_ cases)

let mk_with_exception
    ?mk_return ~mk_bind ?mk_fail ?mk_catch
    ~loc e cases exception_cases
  =
  let mk_return, mk_catch =
    match mk_return, mk_catch with
    | Some mk_return, Some mk_catch ->
      mk_return, mk_catch
    | _ -> Helpers.does_not_support "match with exceptions" ~requires:"mk_return+mk_fail+mk_catch"
  in
  let (pv, v) = Helpers.fresh_variable () in
  (* wrap e in code that classify it between Normal and Exception *)
  let e =
    mk_catch ~loc
      (mk_bind ~loc e
         [%expr fun [%p pv] ->
           [%e mk_return ~loc [%expr Ppx_monad_std.Result.Ok [%e v]]]])
      [%expr fun [%p pv] ->
        [%e mk_return ~loc [%expr Ppx_monad_std.Result.Error [%e v]]]]
  in
  (* wrap all the cases so that they match on Normal or Exception. for
     exceptions, we first unwrap them from the 'exception' keyword, then add a
     catchall if needed and only then wrap them in the Ppx_monad.*.Exception. *)
  let cases =
    cases |> List.map @@ fun case ->
    { case with pc_lhs = [%pat? Ppx_monad_std.Result.Ok [%p case.pc_lhs]] }
  in
  let exception_cases =
    List.map
      (fun case ->
         match case with
         | { pc_lhs = [%pat? exception [%p? pc_lhs]]; _ } ->
           { case with pc_lhs }
         | _ -> assert false)
      exception_cases
  in
  let exception_cases =
    Helpers.add_catchall_if_needed
      ~loc ?mk_return:mk_fail exception_cases
  in
  let exception_cases =
    exception_cases |> List.map @@ fun case ->
    { case with pc_lhs = [%pat? Ppx_monad_std.Result.Error [%p case.pc_lhs]] }
  in
  (* return a match on all this *)
  mk_simple ~mk_bind ~loc e (cases @ exception_cases)

let mk
    ?mk_return ?mk_bind ?mk_fail ?mk_catch
    ~loc e cases
  =
  ignore mk_fail;
  let mk_bind = Helpers.unwrap_or_does_not_support mk_bind
      "match" ~requires:"mk_return+mk_bind"
  in
  (* split the match's cases between normal and exceptional ones *)
  let (cases, exception_cases) =
    List.partition
      (fun case ->
         match case.pc_lhs.ppat_desc with
         | Ppat_exception _ -> false
         | _ -> true)
      cases
  in
  (* if there are no exception cases, we go for a normal, simple encoding.
     otherwise, we have to go for fancy stuff *)
  if exception_cases = [] then
    mk_simple ~mk_bind ~loc e cases
  else
    mk_with_exception
      ?mk_return ~mk_bind ?mk_fail ?mk_catch
      ~loc e cases exception_cases
