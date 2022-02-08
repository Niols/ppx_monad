open Ppxlib
open Ast_helper

let mk
    ?mk_return ?mk_bind ?mk_fail ?mk_catch
    ~loc e cases
  =
  ignore mk_return; ignore mk_bind;
  let mk_catch = Helpers.unwrap_or_does_not_support mk_catch
      "try" ~requires:"mk_catch"
  in
  let cases = Helpers.add_catchall_if_needed ~loc ?mk_return:mk_fail cases in
  mk_catch ~loc e (Exp.function_ cases)
