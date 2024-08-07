open Ppxlib
open Parsetree
open Ast_helper
open Utils

type parsed_decl = {
  name : string;
  (* "NAME" *)
  key : expression;
  (* v.NAME *)
  field : expression;
  codecs : expression option * expression option;
  default : expression option;
  is_optional : bool;
  is_option : bool;
}

let generate_encoder decls unboxed =
  match unboxed with
  | true ->
      let { codecs; field } = List.hd decls in
      let e, _ = codecs in
      [%expr fun v -> [%e Option.get e] [%e field]]
  | false ->
      let arrExpr =
        decls
        |> List.map
             (fun { key; field; codecs = encoder, _; is_optional; is_option } ->
               if is_optional || is_option then
                 [%expr [%e key], [%e Option.get encoder] [%e field]]
               else
                 [%expr
                   [%e key],
                     (* The Encoder for option `Spice.optionToJson` returns option type.
                        So, encoder for other types return with Some to match type of encoder. *)
                     Some ([%e Option.get encoder] [%e field])])
        |> Exp.array
      in
      Exp.constraint_
        [%expr
          Js.Json.Object (Js.Dict.fromArray (Spice.filterOptional [%e arrExpr]))]
        Utils.ctyp_json_t
      |> Exp.fun_ Asttypes.Nolabel None [%pat? v]
      |> Utils.expr_func ~arity:1

let generate_dict_get { key; codecs = _, decoder; default } =
  let decoder = Option.get decoder in
  match default with
  | Some default ->
      [%expr
        Some
          (Belt.Option.getWithDefault
             (Belt.Option.map (Js.Dict.get dict [%e key]) [%e decoder])
             (Ok [%e default]))]
  | None -> [%expr Belt.Option.map (Js.Dict.get dict [%e key]) [%e decoder]]

let generate_error_case { key } =
  {
    pc_lhs = [%pat? Some (Error (e : Spice.decodeError))];
    pc_guard = None;
    pc_rhs = [%expr Error { e with path = "." ^ [%e key] ^ e.path }];
  }

let generate_error_case2 { key } =
  {
    pc_lhs = [%pat? None];
    pc_guard = None;
    pc_rhs = [%expr Spice.error ([%e key] ^ " missing") v];
  }

let generate_final_record_expr path decls =
  decls
  |> List.map (fun { name; is_optional } ->
         let attrs = if is_optional then [ Utils.attr_optional ] else [] in
         let is_opt = List.assoc_opt name path in
         match is_opt with
         | Some true -> (lid name, make_ident_expr ~attrs name)
         | Some false -> (lid name, Exp.construct ~attrs (lid "None") None)
         | None -> assert false)
  |> fun l -> [%expr Ok [%e Exp.record l None]]

let generate_success_case { name } success_expr =
  {
    pc_lhs = (mknoloc name |> Pat.var |> fun p -> [%pat? Some (Ok [%p p])]);
    pc_guard = None;
    pc_rhs = success_expr;
  }

let generate_success_case2 success_expr =
  { pc_lhs = [%pat? None]; pc_guard = None; pc_rhs = success_expr }

let rec generate_nested_switches_recurse path all_decls remaining_decls =
  let ({ is_optional; is_option } as current), success_expr, success_expr2 =
    match remaining_decls with
    | [] -> failwith "Spice internal error: [] not expected"
    | [ ({ name } as last) ] ->
        ( last,
          generate_final_record_expr
            (List.rev_append path [ (name, true) ])
            all_decls,
          generate_final_record_expr
            (List.rev_append path [ (name, false) ])
            all_decls )
    | ({ name } as first) :: tail ->
        ( first,
          generate_nested_switches_recurse
            (List.rev_append path [ (name, true) ])
            all_decls tail,
          generate_nested_switches_recurse
            (List.rev_append path [ (name, false) ])
            all_decls tail )
  in
  if is_optional || is_option then
    [ generate_error_case current ]
    |> List.append [ generate_success_case2 success_expr2 ]
    |> List.append [ generate_success_case current success_expr ]
    |> Exp.match_ (generate_dict_get current)
  else
    [ generate_error_case current ]
    |> List.append [ generate_error_case2 current ]
    |> List.append [ generate_success_case current success_expr ]
    |> Exp.match_ (generate_dict_get current)
[@@ocaml.doc
  " Recursively generates an expression containing nested switches, first\n\
  \ *  decoding the first record items, then (if successful) the second, etc. "]

let generate_nested_switches decls =
  generate_nested_switches_recurse [] decls decls

let generate_decoder decls unboxed =
  match unboxed with
  | true ->
      let { codecs; name } = List.hd decls in
      let _, d = codecs in

      let record_expr = Exp.record [ (lid name, make_ident_expr "v") ] None in

      [%expr
        fun v ->
          Belt.Result.map ([%e Option.get d] v) (fun v -> [%e record_expr])]
  | false ->
      [%expr
        fun v ->
          match (v : Js.Json.t) with
          | Js.Json.Object dict -> [%e generate_nested_switches decls]
          | _ -> Spice.error "Not an object" v]

let parse_decl generator_settings
    { pld_name = { txt }; pld_loc; pld_type; pld_attributes } =
  let default =
    match get_attribute_by_name pld_attributes "spice.default" with
    | Ok (Some attribute) -> Some (get_expression_from_payload attribute)
    | Ok None -> None
    | Error s -> fail pld_loc s
  in
  let key =
    match get_attribute_by_name pld_attributes "spice.key" with
    | Ok (Some attribute) -> get_expression_from_payload attribute
    | Ok None -> Exp.constant (Pconst_string (txt, Location.none, None))
    | Error s -> fail pld_loc s
  in
  let optional_attrs = [ "ns.optional"; "res.optional" ] in
  let is_optional =
    optional_attrs
    |> List.map (fun attr -> get_attribute_by_name pld_attributes attr)
    |> List.exists (function Ok (Some _) -> true | _ -> false)
  in
  let is_option = Utils.check_option_type pld_type in
  let codecs = Codecs.generate_codecs generator_settings pld_type in
  let codecs =
    if is_optional then
      match codecs with
      | Some encode, Some decode ->
          ( Some [%expr Spice.optionToJson [%e encode]],
            Some [%expr Spice.optionFromJson [%e decode]] )
      | Some encode, _ -> (Some [%expr Spice.optionToJson [%e encode]], None)
      | _, Some decode -> (None, Some [%expr Spice.optionFromJson [%e decode]])
      | None, None -> codecs
    else codecs
  in

  {
    name = txt;
    key;
    field = Exp.field [%expr v] (lid txt);
    codecs;
    default;
    is_optional;
    is_option;
  }

let generate_codecs ({ do_encode; do_decode } as generator_settings) decls
    unboxed =
  let parsed_decls = List.map (parse_decl generator_settings) decls in
  ( (if do_encode then Some (generate_encoder parsed_decls unboxed) else None),
    if do_decode then Some (generate_decoder parsed_decls unboxed) else None )
