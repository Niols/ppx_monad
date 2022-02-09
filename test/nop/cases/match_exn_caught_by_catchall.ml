match%nop print_endline "match"; failwith "qjdswflk" with
| None -> print_endline "none"
| exception _ -> print_endline "_"
| Some true -> print_endline "true"
| Some false -> print_endline "false"
