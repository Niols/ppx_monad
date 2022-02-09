let () =
  if%nop print_endline "test"; true then
    print_endline "true"
  else
    print_endline "false"

let () =
  if%nop print_endline "test"; false then
    print_endline "true"
  else
    print_endline "false"

let () =
  if%nop print_endline "test"; true then
    print_endline "true"

let () =
  if%nop print_endline "test"; false then
    print_endline "true"
