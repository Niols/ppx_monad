open Ppxlib

let mk
    ?monad ?monad_error
    ?mk_return ?mk_bind ?mk_fail ?mk_catch ()
  =
  fun ~ctxt expression ->
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match expression.pexp_desc with
  | Pexp_assert e ->
    AssertExpander.mk
      ?monad ?monad_error
      ?mk_return ?mk_bind ?mk_fail ?mk_catch
      ~loc e
  | Pexp_for (i, start, stop, dir, e) ->
    ForExpander.mk
      ?monad ?monad_error
      ?mk_return ?mk_bind ?mk_fail ?mk_catch
      ~loc i start stop dir e
  | Pexp_ifthenelse (e1, e2, e3) ->
    IfthenelseExpander.mk
      ?monad ?monad_error
      ?mk_return ?mk_bind ?mk_fail ?mk_catch
      ~loc e1 e2 e3
  | Pexp_let (rf, vbs, e) ->
    LetExpander.mk
      ?monad ?monad_error
      ?mk_return ?mk_bind ?mk_fail ?mk_catch
      ~loc rf vbs e
  | Pexp_match (e, cases) ->
    MatchExpander.mk
      ?monad ?monad_error
      ?mk_return ?mk_bind ?mk_fail ?mk_catch
      ~loc e cases
  | Pexp_sequence (e1, e2) ->
    SequenceExpander.mk
      ?monad ?monad_error
      ?mk_return ?mk_bind ?mk_fail ?mk_catch
      ~loc e1 e2
  | Pexp_try (e, cases) ->
    TryExpander.mk
      ?monad ?monad_error
      ?mk_return ?mk_bind ?mk_fail ?mk_catch
      ~loc e cases
  | Pexp_while (e1, e2) ->
    WhileExpander.mk
      ?monad ?monad_error
      ?mk_return ?mk_bind ?mk_fail ?mk_catch
      ~loc e1 e2
  | _ ->
    Helpers.does_not_support "other expressions than assert, for, if-then-else, let, match, sequence, try, while"
