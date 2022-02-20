let return x = Ok x

let bind e f = match e with
  | Ok x -> f x
  | Error y -> Error y

let fail y = Error y

let catch e f = match e with
  | Ok x -> Ok x
  | Error y -> f y

module Error = struct
  let return = fail
  let bind = catch
end
