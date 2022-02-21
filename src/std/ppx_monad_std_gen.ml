module C = Configurator.V1

let modules = Hashtbl.create 8

let add_module ?minimum_version name implementation =
  Hashtbl.add modules name (implementation, minimum_version)

let iter_modules f =
  Hashtbl.iter
    (fun name (implementation, minimum_version) ->
       f name implementation minimum_version)
    modules

let () = add_module "List" {|
  let return x = [x]

  let list_concat_map f l =
    let open List in
    let rec aux f acc = function
      | [] -> rev acc
      | x :: l ->
        let xs = f x in
        aux f (rev_append xs acc) l
    in aux f [] l

  let bind e f = list_concat_map f e
|}

let () = add_module "Option" {|
  let return x = Some x

  let bind e f = match e with
    | Some x -> f x
    | None -> None

  let fail () = None

  let catch e f = match e with
    | Some x -> Some x
    | None -> f ()
|}

let () = add_module "Result" {|
  type ('a, 'b) t = ('a, 'b) result =
    | Ok of 'a
    | Error of 'b

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
|}

let () = add_module "Seq" ~minimum_version:(4, 7) {|
  let return = Seq.return

  let bind e f = Seq.flat_map f e
|}

let () = add_module "Either" ~minimum_version:(4, 12) {|
  open Either

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
|}

let () =
  C.main ~name:"ppx_monad_std_gen" @@ fun c ->
  let version = C.ocaml_config_var_exn c "version" in
  let version = Scanf.sscanf version "%u.%u" (fun maj min -> maj, min) in
  iter_modules @@ fun name implementation minimum_version ->
  match minimum_version with
  | Some minimum_version when version < minimum_version -> ()
  | _ -> Printf.printf "module M%s = struct\n%s\nend\nmodule %s = M%s\n\n"
           name implementation name name
