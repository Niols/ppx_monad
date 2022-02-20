open Stdlib.Either

let return x = Right x

let bind e f = match e with
  | Right x -> f x
  | Left y -> Left y

let fail y = Left y

let catch e f = match e with
  | Right x -> Right x
  | Left y -> f y

module Error = struct
  let return = fail
  let bind = catch
end
