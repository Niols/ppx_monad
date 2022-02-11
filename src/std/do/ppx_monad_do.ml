open Ppxlib
open Ast_helper

let rec expression_to_pattern ~loc expression =
  match expression.pexp_desc with
  | Pexp_ident {txt=Lident "__"; _} -> Pat.any ~loc ()
  | Pexp_ident {txt=Lident id; _} -> Pat.var ~loc {txt=id; loc}
  | Pexp_tuple expressions -> Pat.tuple (List.map (expression_to_pattern ~loc) expressions)
  (* FIXME: record (including _) *)
  | _ -> assert false

let expander ~bind ~loc =
  let rec expander = function
    | [%expr [%e? {pexp_desc=Pexp_setinstvar (x, e);_}]; [%e? next]] -> (* x <- e; next *)
      [%expr [%e bind] [%e e] (fun [%p Pat.var x] -> [%e expander next])]
    | [%expr [%e? x] <-- [%e? e]; [%e? next]] ->
      let x = expression_to_pattern x ~loc in
      [%expr [%e bind] [%e e] (fun [%p x] -> [%e expander next])]
    | [%expr [%e? e]; [%e? next]] ->
      [%expr [%e bind] [%e e] (fun () -> [%e expander next])]
    | [%expr let [%p? x] = [%e? e] in [%e? next]] ->
      [%expr let [%p x] = [%e e] in [%e expander next]]
    | expression -> expression
  in
  expander

let extract_bind_from_attributes ~loc attributes =
  let bind_from_payload ~loc = function
    | PStr [{pstr_desc=Pstr_eval ({pexp_desc=Pexp_ident bind;_}, _);_}] -> bind
    | _ -> Location.raise_errorf ~loc "the attribute `bind` expects a function identifier"
  in
  let monad_from_payload ~loc = function
    | PStr [{pstr_desc=Pstr_eval ({pexp_desc=Pexp_construct(monad, None);_}, _); _}] -> monad
    | _ -> Location.raise_errorf ~loc "the attribute `monad` expects a module identifier"
  in
  let rec extract_bind_from_attributes = function
    | [] -> { txt = Lident "bind"; loc }
    | {attr_name={txt="bind";_}; attr_payload; attr_loc} :: _ ->
      bind_from_payload ~loc:attr_loc attr_payload
    | {attr_name={txt="monad";_}; attr_payload; attr_loc} :: _ ->
      let { txt; loc } = monad_from_payload ~loc:attr_loc attr_payload in
      { txt = Ldot (txt, "bind"); loc }
    | _ :: rest -> extract_bind_from_attributes rest
  in
  Exp.ident (extract_bind_from_attributes attributes)

let expander ~ctxt expression _ =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let bind = extract_bind_from_attributes ~loc expression.pexp_attributes in
  expander ~bind ~loc expression

let () =
  let extension =
    Extension.V3.declare "do"
      Extension.Context.expression
      Ast_pattern.(pstr ((pstr_eval __ __) ^:: nil))
      expander
  in
  let rule = Ppxlib.Context_free.Rule.extension extension in
  Driver.register_transformation ~rules:[rule] "do"
