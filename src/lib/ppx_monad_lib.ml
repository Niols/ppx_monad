open Ppxlib

let fresh_variable = Helpers.fresh_variable

let register
    (* Module containing monadic functions *)
    ?monad ?monad_error
    (* Monadic functions *)
    ?mk_return ?mk_bind
    ?mk_fail ?mk_catch
    (* Name and where it applies *)
    ?applies_on name
  =
  (* compute the expander *)
  let expander =
    Expander.mk
      ?monad ?monad_error
      ?mk_return ?mk_bind ?mk_fail ?mk_catch
      ()
  in
  (* compute the labels to which this extension should apply *)
  let labels =
    (match applies_on with
     | Some applies_on -> applies_on
     | None -> name)
    |> (fun r -> "monad.(" ^ r ^ ")")
    |> SimpleRegexp.from_string
    |> SimpleRegexp.unfoldings
  in
  (* for each label, create a rule using the expander *)
  let rule_of_label label =
    Extension.V3.declare label
      Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      expander
    |> Ppxlib.Context_free.Rule.extension
  in
  let rules = List.map rule_of_label labels in
  (* register a transformation with all those rules *)
  Driver.register_transformation ~rules name
