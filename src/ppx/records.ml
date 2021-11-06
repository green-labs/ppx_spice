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
}

let generate_encoder decls unboxed =
  match unboxed with
  | true ->
      let { codecs; field } = List.hd decls in
      let e, _ = codecs in
      [%expr fun v -> [%e Option.get e] [%e field]]
  | false ->
      let arr_expr =
        decls
        |> List.map (fun { key; field; codecs = encoder, _ } ->
               [%expr [%e key], [%e Option.get encoder] [%e field]])
        |> Exp.array
      in
      [%expr [%e arr_expr] |> Js.Dict.fromArray |> Js.Json.object_]
      |> Exp.fun_ Asttypes.Nolabel None [%pat? v]

let generate_dict_get { key; codecs = _, decoder; default } =
  let decoder = Option.get decoder in
  match default with
  | Some default ->
      [%expr
        Belt.Option.getWithDefault
          (Belt.Option.map (Js.Dict.get dict [%e key]) [%e decoder])
          Belt.Result.Ok [%e default]]
  | None ->
      [%expr
        Belt.Option.getWithDefault (Js.Dict.get dict, [%e key]) Js.Json.null
        |> [%e decoder]]

let generate_dict_gets decls =
  decls |> List.map generate_dict_get |> tuple_or_singleton Exp.tuple

let generate_error_case { key } =
  {
    pc_lhs = [%pat? Belt.Result.Error (e : Spice.decodeError)];
    pc_guard = None;
    pc_rhs = [%expr Belt.Result.Error { e with path = "." ^ [%e key] ^ e.path }];
  }

let generate_final_record_expr decls =
  decls |> List.map (fun { name } -> (lid name, make_ident_expr name))
  |> fun l -> [%expr Belt.Result.Ok [%e Exp.record l None]]

let generate_success_case { name } success_expr =
  {
    pc_lhs = (mknoloc name |> Pat.var |> fun p -> [%pat? Belt.Result.Ok [%p p]]);
    pc_guard = None;
    pc_rhs = success_expr;
  }

(** Recursively generates an expression containing nested switches, first
 *  decoding the first record items, then (if successful) the second, etc. *)
let rec generate_nested_switches_recurse all_decls remaining_decls =
  let current, success_expr =
    match remaining_decls with
    | [] -> failwith "Spice internal error: [] not expected"
    | [ last ] -> (last, generate_final_record_expr all_decls)
    | first :: tail -> (first, generate_nested_switches_recurse all_decls tail)
  in
  [ generate_error_case current ]
  |> List.append [ generate_success_case current success_expr ]
  |> Exp.match_ (generate_dict_get current)

let generate_nested_switches decls =
  generate_nested_switches_recurse decls decls

let generate_decoder decls unboxed =
  if unboxed then
    let { codecs; name } = List.hd decls in
    let _, d = codecs in

    let record_expr = Exp.record [ (lid name, make_ident_expr "v") ] None in

    [%expr
      fun v -> Belt.Result.map ([%e Option.get d] v, v => [%e record_expr])]
  else
    [%expr
      fun v ->
        match Js.Json.classify v with
        | Js.Json.JSONObject dict -> [%e generate_nested_switches decls]
        | _ -> Belt.Result.Error ("Not an object", v)]

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
  {
    name = txt;
    key;
    field = Exp.field [%expr v] (lid txt);
    codecs = Codecs.generate_codecs generator_settings pld_type;
    default;
  }

let generate_codecs ({ do_encode; do_decode } as generator_settings) decls
    unboxed =
  let parsed_decls = List.map (parse_decl generator_settings) decls in
  ( (match do_encode with
    | true -> Some (generate_encoder parsed_decls unboxed)
    | false -> None),
    match do_decode with
    | true -> Some (generate_decoder parsed_decls unboxed)
    | false -> None )
