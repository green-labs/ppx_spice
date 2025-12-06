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
      Utils.expr_func ~arity:1 [%expr fun v -> [%e Option.get e] [%e field]]
  | false ->
      let arrExpr =
        decls
        |> List.map
             (fun { key; field; codecs = encoder, _; is_optional; is_option } ->
               if is_optional || is_option then
                 Exp.tuple
                   [ key; Exp.apply (Option.get encoder) [ (Nolabel, field) ] ]
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

let generate_flat_decoder_expr decls =
  let loc = !default_loc in
  let dict_expr = Exp.ident (mknoloc (Longident.Lident "dict")) in
  let field_results =
    List.map
      (fun d ->
        let { name; key; codecs; default; is_optional; is_option } = d in
        let result_name = name ^ "_result" in
        let decode_expr =
          match codecs with
          | _, Some decode ->
              let get_expr = [%expr Js.Dict.get [%e dict_expr] [%e key]] in
              let decode_applied = [%expr [%e decode]] in
              let opt_map =
                [%expr Belt.Option.map [%e get_expr] [%e decode_applied]]
              in
              let default_expr =
                match (is_optional, is_option, default) with
                | true, _, Some d ->
                    [%expr Belt.Option.getWithDefault [%e opt_map] (Ok [%e d])]
                | true, _, None ->
                    [%expr Belt.Option.getWithDefault [%e opt_map] (Ok None)]
                | _, true, _ ->
                    [%expr Belt.Option.getWithDefault [%e opt_map] (Ok None)]
                | _, _, Some d ->
                    [%expr Belt.Option.getWithDefault [%e opt_map] (Ok [%e d])]
                | _, _, None ->
                    [%expr
                      Belt.Option.getWithDefault [%e opt_map]
                        (Spice.error ([%e key] ^ " missing") v)]
              in
              default_expr
          | _ -> [%expr Spice.error ([%e key] ^ " missing") v]
        in
        (result_name, decode_expr))
      decls
  in
  let let_bindings =
    List.map
      (fun (result_name, decode_expr) ->
        Vb.mk (Pat.var (mknoloc result_name)) decode_expr)
      field_results
  in
  let tuple_expr =
    match field_results with
    | [ (result_name, _) ] -> Exp.ident (mknoloc (Longident.Lident result_name))
    | _ ->
        Exp.tuple
          (List.map
             (fun (result_name, _) ->
               Exp.ident (mknoloc (Longident.Lident result_name)))
             field_results)
  in
  let ok_pattern =
    match decls with
    | [ d ] ->
        let { name; _ } = d in
        Pat.construct
          (mknoloc (Longident.Lident "Ok"))
          (Some (Pat.var (mknoloc name)))
    | _ ->
        Pat.tuple
          (List.map
             (fun d ->
               let { name; _ } = d in
               Pat.construct
                 (mknoloc (Longident.Lident "Ok"))
                 (Some (Pat.var (mknoloc name))))
             decls)
  in
  let ok_expr =
    let record_fields =
      List.map
        (fun d ->
          let { name; is_optional } = d in
          let attrs = if is_optional then [ Utils.attr_optional ] else [] in
          (lid name, make_ident_expr ~attrs name))
        decls
    in
    [%expr Ok [%e Exp.record record_fields None]]
  in
  let error_patterns =
    List.mapi
      (fun i d ->
        let { key; _ } = d in
        let pats =
          List.init (List.length decls) (fun j ->
              if i = j then
                Pat.construct
                  (mknoloc (Longident.Lident "Error"))
                  (Some
                     (Pat.constraint_
                        (Pat.var (mknoloc "e"))
                        (Typ.constr
                           (mknoloc (Longident.parse "Spice.decodeError"))
                           [])))
              else Pat.any ())
        in
        let pat = match pats with [ p ] -> p | _ -> Pat.tuple pats in
        Exp.case pat [%expr Spice.error ~path:("." ^ [%e key] ^ e.path) e.message e.value])
      decls
  in
  let match_expr =
    Exp.match_ tuple_expr (Exp.case ok_pattern ok_expr :: error_patterns)
  in
  List.fold_right
    (fun vb acc -> Exp.let_ Nonrecursive [ vb ] acc)
    let_bindings match_expr

let generate_nested_switches_recurse _path decls _remaining_decls =
  generate_flat_decoder_expr decls

let generate_nested_switches decls =
  generate_nested_switches_recurse [] decls decls

let generate_decoder decls unboxed =
  match unboxed with
  | true ->
      let { codecs; name } = List.hd decls in
      let _, d = codecs in

      let record_expr = Exp.record [ (lid name, make_ident_expr "v") ] None in

      Utils.expr_func ~arity:1
        [%expr
          fun v ->
            Belt.Result.map ([%e Option.get d] v) (fun v -> [%e record_expr])]
  | false ->
      Utils.expr_func ~arity:1
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
    | Ok None -> Exp.constant (Pconst_string (txt, Location.none, Some "*j"))
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
  let add_attrs attrs e = { e with pexp_attributes = attrs } in
  let codecs =
    if is_optional then
      match codecs with
      | Some encode, Some decode ->
          ( Some
              (add_attrs [ Utils.attr_partial ]
                 [%expr Spice.optionToJson [%e encode]]),
            Some
              (add_attrs [ Utils.attr_partial ]
                 [%expr Spice.optionFromJson [%e decode]]) )
      | Some encode, _ ->
          ( Some
              (add_attrs [ Utils.attr_partial ]
                 [%expr Spice.optionToJson [%e encode]]),
            None )
      | _, Some decode ->
          ( None,
            Some
              (add_attrs [ Utils.attr_partial ]
                 [%expr Spice.optionFromJson [%e decode]]) )
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
