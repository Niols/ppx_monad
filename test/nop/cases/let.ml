let () =
  let%nop foo = print_endline "foo"; "foo" in
  let%nop bar = print_endline "bar"; "bar" in
  let%nop baz = print_endline "baz"; "baz" in
  print_endline foo;
  print_endline bar;
  print_endline baz

let () =
  let%nop foo = print_endline "foo"; "foo"
  and     bar = print_endline "bar"; "bar"
  and     baz = print_endline "baz"; "baz"
  in
  print_endline foo;
  print_endline bar;
  print_endline baz
