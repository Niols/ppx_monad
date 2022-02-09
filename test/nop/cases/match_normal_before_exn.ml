match%nop print_endline "match"; None with
| None -> print_endline "none"
| Some true -> print_endline "true"
| exception Failure _ -> print_endline "failure"
| Some false -> print_endline "false"
