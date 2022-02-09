open Ppxlib

let expand ~ctxt payload =
  ignore ctxt; payload

let () =
  let rule =
    Extension.V3.declare "nop"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      expand
    |> Ppxlib.Context_free.Rule.extension
  in
  Driver.register_transformation ~rules:[rule] "ppx_nop_from_scratch"
