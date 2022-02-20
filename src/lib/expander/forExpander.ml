open Ppxlib
open Helpers

let mk
    ?monad ?monad_error
    ?mk_return ?mk_bind ?mk_fail ?mk_catch
    ~loc i start stop dir e
  =
  let mk_return = first_or_does_not_support "for" [
      mk_return;
      Common.mk_fail_of_monad <$> monad_error;
      Common.mk_return_of_monad <$> monad;
    ]
  in
  let mk_bind = first_or_does_not_support "for" [
      mk_bind;
      Common.mk_catch_of_monad <$> monad_error;
      Common.mk_bind_of_monad <$> monad;
    ]
  in
  ignore mk_fail;
  ignore mk_catch;
  let pfor, for_  = fresh_variable () in
  let pj,  j  = fresh_variable () in
  let pj0, j0 = fresh_variable () in
  let pjn, jn = fresh_variable () in
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
