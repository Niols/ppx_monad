try
  match%nop print_endline "match"; Some true with
  | None -> print_endline "none"
  | exception Failure _ -> print_endline "failure"
  | Some true -> print_endline "true"; failwith "jkljklklj"
  | Some false -> print_endline "false"
with
  Failure _ -> print_endline "uncaught failure"
