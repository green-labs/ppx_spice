open Ppxlib
open Parsetree
open Ast_helper
open Utils

let rec parameterize_codecs type_args encoder_func decoder_func
    generator_settings =
  let sub_encoders, sub_decoders =
    type_args
    |> List.map (fun core_type -> generate_codecs generator_settings core_type)
    |> List.split
  in
  ( (match encoder_func with
    | None -> None
    | Some encoder_func ->
        sub_encoders
        |> List.map (fun e -> (Asttypes.Nolabel, Option.get e))
        |> Exp.apply encoder_func |> Option.some),
    match decoder_func with
    | None -> None
    | Some decoder_func ->
        sub_decoders
        |> List.map (fun e -> (Asttypes.Nolabel, Option.get e))
        |> Exp.apply decoder_func |> Option.some )

and generate_constr_codecs { do_encode; do_decode }
    { Location.txt = identifier; loc } =
  let open Longident in
  match identifier with
  | Lident "string" -> (
      ( (match do_encode with
        | true -> Some [%expr Spice.stringToJson]
        | false -> None),
        match do_decode with
        | true -> Some [%expr Spice.stringFromJson]
        | false -> None ))
  | Lident "int" -> (
      ( (match do_encode with
        | true -> Some [%expr Spice.intToJson]
        | false -> None),
        match do_decode with
        | true -> Some [%expr Spice.intFromJson]
        | false -> None ))
  | Lident "int64" -> (
      ( (match do_encode with
        | true -> Some [%expr Spice.int64ToJson]
        | false -> None),
        match do_decode with
        | true -> Some [%expr Spice.int64FromJson]
        | false -> None ))
  | Lident "float" -> (
      ( (match do_encode with
        | true -> Some [%expr Spice.floatToJson]
        | false -> None),
        match do_decode with
        | true -> Some [%expr Spice.floatFromJson]
        | false -> None ))
  | Lident "bool" -> (
      ( (match do_encode with
        | true -> Some [%expr Spice.boolToJson]
        | false -> None),
        match do_decode with
        | true -> Some [%expr Spice.boolFromJson]
        | false -> None ))
  | Lident "unit" -> (
      ( (match do_encode with
        | true -> Some [%expr Spice.unitToJson]
        | false -> None),
        match do_decode with
        | true -> Some [%expr Spice.unitFromJson]
        | false -> None ))
  | Lident "array" -> (
      ( (match do_encode with
        | true -> Some [%expr Spice.arrayToJson]
        | false -> None),
        match do_decode with
        | true -> Some [%expr Spice.arrayFromJson]
        | false -> None ))
  | Lident "list" -> (
      ( (match do_encode with
        | true -> Some [%expr Spice.listToJson]
        | false -> None),
        match do_decode with
        | true -> Some [%expr Spice.listFromJson]
        | false -> None ))
  | Lident "option" -> (
      ( (match do_encode with
        | true -> Some [%expr Spice.optionToJson]
        | false -> None),
        match do_decode with
        | true -> Some [%expr Spice.optionFromJson]
        | false -> None ))
  | Ldot (Ldot (Lident "Belt", "Result"), "t") -> (
      ( (match do_encode with
        | true -> Some [%expr Spice.resultToJson]
        | false -> None),
        match do_decode with
        | true -> Some [%expr Spice.resultFromJson]
        | false -> None ))
  | Ldot (Ldot (Lident "Js", "Dict"), "t") -> (
      ( (match do_encode with
        | true -> Some [%expr Spice.dictToJson]
        | false -> None),
        match do_decode with
        | true -> Some [%expr Spice.dictFromJson]
        | false -> None ))
  | Ldot (Ldot (Lident "Js", "Json"), "t") -> (
      ( (match do_encode with true -> Some [%expr fun v -> v] | false -> None),
        match do_decode with
        | true -> Some [%expr fun v -> Belt.Result.Ok v]
        | false -> None ))
  | Lident s -> (
      ( (match do_encode with
        | true -> Some (make_ident_expr (s ^ Utils.encoder_func_suffix))
        | false -> None),
        match do_decode with
        | true -> Some (make_ident_expr (s ^ Utils.decoder_func_suffix))
        | false -> None ))
  | Ldot (left, right) -> (
      ( (match do_encode with
        | true ->
            Some
              (Exp.ident
                 (mknoloc (Ldot (left, right ^ Utils.encoder_func_suffix))))
        | false -> None),
        match do_decode with
        | true ->
            Some
              (Exp.ident
                 (mknoloc (Ldot (left, right ^ Utils.decoder_func_suffix))))
        | false -> None ))
  | Lapply (_, _) -> fail loc "Lapply syntax not yet handled by spice"

and generate_codecs ({ do_encode; do_decode } as generator_settings)
    { ptyp_desc; ptyp_loc; ptyp_attributes } =
  match ptyp_desc with
  | Ptyp_any -> fail ptyp_loc "Can't generate codecs for `any` type"
  | Ptyp_arrow (_, _, _) ->
      fail ptyp_loc "Can't generate codecs for function type"
  | Ptyp_package _ -> fail ptyp_loc "Can't generate codecs for module type"
  | Ptyp_tuple types -> (
      let composite_codecs =
        List.map (generate_codecs generator_settings) types
      in
      ( (match do_encode with
        | true ->
            Some
              (composite_codecs
              |> List.map (fun (e, _) -> Option.get e)
              |> Tuple.generate_encoder)
        | false -> None),
        match do_decode with
        | true ->
            Some
              (composite_codecs
              |> List.map (fun (_, d) -> Option.get d)
              |> Tuple.generate_decoder)
        | false -> None ))
  | Ptyp_var s -> (
      ( (match do_encode with
        | true -> Some (make_ident_expr (encoder_var_prefix ^ s))
        | false -> None),
        match do_decode with
        | true -> Some (make_ident_expr (decoder_var_prefix ^ s))
        | false -> None ))
  | Ptyp_constr (constr, typeArgs) -> (
      let custom_codec = get_attribute_by_name ptyp_attributes "spice.codec" in
      let encode, decode =
        match custom_codec with
        | Ok None -> generate_constr_codecs generator_settings constr
        | Ok (Some attribute) -> (
            let expr = get_expression_from_payload attribute in
            ( (match do_encode with
              | true ->
                  Some
                    [%expr
                      let e, _ = [%e expr] in
                      e]
              | false -> None),
              match do_decode with
              | true ->
                  Some
                    [%expr
                      let _, d = [%e expr] in
                      d]
              | false -> None ))
        | Error s -> fail ptyp_loc s
      in
      match List.length typeArgs = 0 with
      | true -> (encode, decode)
      | false -> parameterize_codecs typeArgs encode decode generator_settings)
  | _ -> fail ptyp_loc "This syntax is not yet handled by spice"
