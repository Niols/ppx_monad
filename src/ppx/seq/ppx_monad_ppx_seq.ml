open Ppxlib

let mk_return ~loc x =
  [%expr Seq.return [%e x]]

let mk_bind ~loc e f =
  [%expr Seq.flat_map [%e f] [%e e]]

let () = Ppx_monad_lib.register "seq"
    ~mk_return ~mk_bind
