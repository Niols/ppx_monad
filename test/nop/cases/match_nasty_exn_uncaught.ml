try
  match%nop print_endline "match"; Some true with
  | None -> print_endline "none"
  | exception Invalid_argument _ -> print_endline "invalid argument"
  | Some true -> print_endline "true"; failwith "jkljklklj"
  | Some false -> print_endline "false"
with
  Failure _ -> print_endline "uncaught failure"
