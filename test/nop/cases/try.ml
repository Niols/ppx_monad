let () =
  try%nop
    print_endline "try"
  with
  | Failure _ -> print_endline "failure"
  | _ -> print_endline "_"

let () =
  try%nop
    print_endline "try";
    failwith "sdkfh"
  with
  | Failure _ -> print_endline "failure"
  | _ -> print_endline "_"

let () =
  try%nop
    print_endline "try";
    invalid_arg "sldkfj"
  with
  | Failure _ -> print_endline "failure"
  | _ -> print_endline "_"

let () =
  try%nop
    print_endline "try";
    failwith "sldkfj"
  with
    Failure _ -> print_endline "failure"

let () =
  try%nop
    print_endline "try";
    failwith "sldkfj"
  with
  | Failure _ -> print_endline "failure"
  | Invalid_argument _ -> print_endline "invalid argument"

let () =
  try%nop
    print_endline "try";
    failwith "sldkfj"
  with
  | Invalid_argument _ -> print_endline "invalid argument"
  | Failure _ -> print_endline "failure"

let () =
  try
    try%nop
      print_endline "try";
      failwith "sldkfj"
    with
      Invalid_argument _ -> print_endline "invalid argument"
  with
    Failure _ -> print_endline "failure"
