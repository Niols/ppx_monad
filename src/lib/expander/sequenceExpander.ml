open Ppxlib

let mk
    ?mk_return ?mk_bind ?mk_fail ?mk_catch
    ~loc e1 e2
  =
  ignore mk_return; ignore mk_fail; ignore mk_catch;
  let mk_bind = Helpers.unwrap_or_does_not_support mk_bind
      "sequence" ~requires:"mk_bind"
  in
  (* FIXME: this forces the first element of a sequence to be of type unit, but
     this is usually just a warning in OCaml. Maybe there is a way to do that? *)
  mk_bind ~loc e1 [%expr fun () -> [%e e2]]
