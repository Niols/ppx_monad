{
  type t = component list

  and component =
    | Literal of string
    | Maybe of t
    | Or of t * t

  let recompose_accumulator acc =
    List.flatten (List.rev acc)
}

rule parse_ end_in_par acc = parse

  | "(" {
      let acc = parse_ true [] lexbuf :: acc in
      parse_ end_in_par acc lexbuf
    }

  | "?" {
      match acc with
      | [] -> assert false
      | last :: acc ->
        parse_ end_in_par ([Maybe last] :: acc) lexbuf
    }

  | "|" {
      [Or (
          recompose_accumulator acc,
          parse_ end_in_par [] lexbuf
        )]
    }

  | ")" {
      assert end_in_par;
      recompose_accumulator acc
    }

  | eof {
      assert (not end_in_par);
      recompose_accumulator acc
    }

  | _ as c {
      parse_ end_in_par ([Literal (String.make 1 c)] :: acc) lexbuf
    }

{
  let parse_ = parse_ false []

  let from_string s =
    parse_ (Lexing.from_string s)

  let rec unfoldings = function
    | [] -> [[]]
    | Literal s :: r' -> List.(map (cons s) (unfoldings r'))
    | Maybe r :: r' -> unfoldings r' @ unfoldings (r @ r')
    | Or (r1, r2) :: r' -> unfoldings (r1 @ r') @ unfoldings (r2 @ r')

  let unfoldings r =
    unfoldings r
    |> List.map (String.concat "")
    |> List.sort_uniq String.compare

  let matches r s =
    unfoldings r
    |> List.mem s
}
