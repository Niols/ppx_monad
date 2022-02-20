let return x = Some x

let bind e f = match e with
  | Some x -> f x
  | None -> None

let fail () = None

let catch e f = match e with
  | Some x -> Some x
  | None -> f ()
