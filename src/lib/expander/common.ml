open Ppxlib
open Ast_helper

let monad_function ~loc monad function_ =
  Exp.ident ~loc { txt = Ldot (monad, function_) ; loc }

let mk_return_of_monad monad = fun ~loc x ->
  Exp.apply ~loc (monad_function ~loc monad "return") [Nolabel, x]

let mk_bind_of_monad monad = fun ~loc e f ->
  Exp.apply ~loc (monad_function ~loc monad "bind") [Nolabel, e; Nolabel, f]

let mk_fail_of_monad monad = fun ~loc y ->
  Exp.apply ~loc (monad_function ~loc monad "fail") [Nolabel, y]

let mk_catch_of_monad monad = fun ~loc e f ->
  Exp.apply ~loc (monad_function ~loc monad "catch") [Nolabel, e; Nolabel, f]
