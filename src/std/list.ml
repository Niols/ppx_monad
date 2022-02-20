let return x = [x]

let list_concat_map f l =
  let open Stdlib.List in
  let rec aux f acc = function
    | [] -> rev acc
    | x :: l ->
      let xs = f x in
      aux f (rev_append xs acc) l
  in aux f [] l

let bind e f = list_concat_map f e
