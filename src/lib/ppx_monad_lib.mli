open Ppxlib

val register :
  ?monad:Longident.t ->
  ?monad_error:Longident.t ->
  ?mk_return:(loc:location -> expression -> expression) ->
  ?mk_bind:(loc:location -> expression -> expression -> expression) ->
  ?mk_fail:(loc:location -> expression -> expression) ->
  ?mk_catch:(loc:location -> expression -> expression -> expression) ->
  ?applies_on:string ->
  string -> unit
(** Register a new monadic PPX. *)

val fresh_variable : unit -> pattern * expression
(** Creates a fresh variable never used anywhere else in the program. Returns a
   pattern binding it and an expression evaluating to it. *)
