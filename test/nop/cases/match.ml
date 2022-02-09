let () =
  match%nop print_endline "match"; None with
  | None -> print_endline "none"
  | Some true -> print_endline "true"
  | Some false -> print_endline "false"

let () =
  match%nop print_endline "match"; Some true with
  | None -> print_endline "none"
  | Some true -> print_endline "true"
  | Some false -> print_endline "false"

let () =
  match%nop print_endline "match"; Some false with
  | None -> print_endline "none"
  | Some true -> print_endline "true"
  | Some false -> print_endline "false"

let () =
  match%nop print_endline "match"; None with
  | Some true -> print_endline "true"
  | None -> print_endline "none"
  | Some false -> print_endline "false"

let () =
  match%nop print_endline "match"; Some false with
  | Some false -> print_endline "false"
  | None -> print_endline "none"
  | Some true -> print_endline "true"

let () =
  match%nop print_endline "match"; Some true with
  | Some false -> print_endline "false"
  | _ -> print_endline "_"

let () =
  match%nop print_endline "match"; Some false with
  | Some false -> print_endline "false"
  | _ -> print_endline "_"
