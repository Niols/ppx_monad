match%nop print_endline "match"; Some false with
| None when print_endline "guard"; false -> print_endline "none"
| Some true -> print_endline "true"
| Some false when print_endline "guard"; true -> print_endline "false"
| _ -> print_endline "_"
