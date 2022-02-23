open Ppxlib

let mk_return ~loc x =
  [%expr [[%e x]]]

let mk_bind ~loc e f =
  let (pcmap, cmap) = Ppx_monad_lib.fresh_variable () in
  let (pbind, bind) = Ppx_monad_lib.fresh_variable () in
  (* re-implement concat_map, only in Stdlib.List since 4.10.0 *)
  [%expr
    let [%p pcmap] = fun f l ->
      let rec aux f acc = function
        | [] -> Stdlib.List.rev acc
        | x :: l ->
          let xs = f x in
          aux f (Stdlib.List.rev_append xs acc) l
      in aux f [] l
    in
    let [%p pbind] = fun e f -> [%e cmap] f e in
    [%e bind] [%e e] [%e f]]

let () = Ppx_monad_lib.register "list"
    ~applies_on:"lst|list"
    ~mk_return ~mk_bind
