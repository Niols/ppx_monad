let () =
  try
    assert%nop (print_endline "assert"; true)
  with
    Assert_failure _ -> print_endline "assert failure"

let () =
  try
    assert%nop (print_endline "assert"; false)
  with
    Assert_failure _ -> print_endline "assert failure"

let () =
  try
    print_endline ("assert" ^ assert%nop false)
  with
    Assert_failure _ -> print_endline "assert failure"

let () =
  try
    print_int (1 + assert%nop false);
    print_newline ()
  with
    Assert_failure _ -> print_endline "assert failure"
