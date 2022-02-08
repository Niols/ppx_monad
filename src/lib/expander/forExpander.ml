open Ppxlib

let mk
    ?mk_return ?mk_bind ?mk_fail ?mk_catch
    ~loc i start stop dir e
  =
  ignore mk_fail; ignore mk_catch;
  let (mk_return, mk_bind) =
    Helpers.unwrap2_or_does_not_support (mk_return, mk_bind)
      "for" ~requires:"mk_return+mk_bind"
  in
  let pfor, for_  = Helpers.fresh_variable () in
  let pj,  j  = Helpers.fresh_variable () in
  let pj0, j0 = Helpers.fresh_variable () in
  let pjn, jn = Helpers.fresh_variable () in
  let j_gt_jn, j_plus_1 =
    match dir with
    | Upto -> [%expr [%e j] > [%e jn]], [%expr [%e j] + 1]
    | Downto -> [%expr [%e j] < [%e jn]], [%expr [%e j] - 1]
  in
  [%expr
    let [%p pj0] = [%e start] in
    let [%p pjn] = [%e stop] in
    let rec [%p pfor] = fun [%p pj] ->
      if [%e j_gt_jn] then
        [%e mk_return ~loc [%expr ()]]
      else
        [%e mk_bind ~loc [%expr let [%p i] = [%e j] in [%e e]]
            [%expr fun () -> [%e for_] [%e j_plus_1]]]
    in
    [%e for_] [%e j0]]
