(* dummy module that includes [Ppx_monad_lib] but also depends on all the PPXes.
   This allows to use the dependency [ppx_monad] both as the PPX and the library
   to provide PPXes. *)

include Ppx_monad_lib
