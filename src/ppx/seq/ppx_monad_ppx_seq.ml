open Ppxlib

let mk_return ~loc x =
  [%expr Stdlib.Seq.return [%e x]]

let mk_bind ~loc e f =
  [%expr Stdlib.Seq.flat_map [%e f] [%e e]]

let () = Ppx_monad_lib.register "seq"
    ~mk_return ~mk_bind
