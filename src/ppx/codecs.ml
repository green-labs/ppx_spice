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
  | Lident "string" ->
      ( some_if_true do_encode [%expr Spice.stringToJson],
        some_if_true do_decode [%expr Spice.stringFromJson] )
  | Lident "int" ->
      ( some_if_true do_encode [%expr Spice.intToJson],
        some_if_true do_decode [%expr Spice.intFromJson] )
  | Lident "int64" ->
      ( some_if_true do_encode [%expr Spice.int64ToJson],
        some_if_true do_decode [%expr Spice.int64FromJson] )
  | Lident "float" ->
      ( some_if_true do_encode [%expr Spice.floatToJson],
        some_if_true do_decode [%expr Spice.floatFromJson] )
  | Lident "bool" ->
      ( some_if_true do_encode [%expr Spice.boolToJson],
        some_if_true do_decode [%expr Spice.boolFromJson] )
  | Lident "unit" ->
      ( some_if_true do_encode [%expr Spice.unitToJson],
        some_if_true do_decode [%expr Spice.unitFromJson] )
  | Lident "array" ->
      ( some_if_true do_encode [%expr Spice.arrayToJson],
        some_if_true do_decode [%expr Spice.arrayFromJson] )
  | Lident "list" ->
      ( some_if_true do_encode [%expr Spice.listToJson],
        some_if_true do_decode [%expr Spice.listFromJson] )
  | Lident "option" ->
      ( some_if_true do_encode [%expr Spice.optionToJson],
        some_if_true do_decode [%expr Spice.optionFromJson] )
  | Ldot (Ldot (Lident "Belt", "Result"), "t") ->
      ( some_if_true do_encode [%expr Spice.resultToJson],
        some_if_true do_decode [%expr Spice.resultFromJson] )
  | Ldot (Ldot (Lident "Js", "Dict"), "t") ->
      ( some_if_true do_encode [%expr Spice.dictToJson],
        some_if_true do_decode [%expr Spice.dictFromJson] )
  | Ldot (Ldot (Lident "Js", "Json"), "t") ->
      ( some_if_true do_encode [%expr fun v -> v],
        some_if_true do_decode [%expr fun v -> Belt.Result.Ok v] )
  | Lident s ->
      ( some_if_true do_encode (make_ident_expr (s ^ Utils.encoder_func_suffix)),
        some_if_true do_decode (make_ident_expr (s ^ Utils.decoder_func_suffix))
      )
  | Ldot (left, right) ->
      ( some_if_true do_encode
          (Exp.ident (mknoloc (Ldot (left, right ^ Utils.encoder_func_suffix)))),
        some_if_true do_decode
          (Exp.ident (mknoloc (Ldot (left, right ^ Utils.decoder_func_suffix))))
      )
  | Lapply (_, _) -> fail loc "Lapply syntax not yet handled by spice"

and generate_codecs ?(is_optional = false)
    ({ do_encode; do_decode } as generator_settings)
    { ptyp_desc; ptyp_loc; ptyp_attributes } =
  match ptyp_desc with
  | Ptyp_any -> fail ptyp_loc "Can't generate codecs for `any` type"
  | Ptyp_arrow (_, _, _) ->
      fail ptyp_loc "Can't generate codecs for function type"
  | Ptyp_package _ -> fail ptyp_loc "Can't generate codecs for module type"
  | Ptyp_tuple types ->
      let composite_codecs =
        List.map (generate_codecs ~is_optional generator_settings) types
      in
      ( some_if_true do_encode
          (composite_codecs
          |> List.map (fun (e, _) -> Option.get e)
          |> Tuple.generate_encoder),
        some_if_true do_decode
          (composite_codecs
          |> List.map (fun (_, d) -> Option.get d)
          |> Tuple.generate_decoder) )
  | Ptyp_var s ->
      ( some_if_true do_encode (make_ident_expr (encoder_var_prefix ^ s)),
        some_if_true do_decode (make_ident_expr (decoder_var_prefix ^ s)) )
  | Ptyp_constr (constr, typeArgs) -> (
      let custom_codec = get_attribute_by_name ptyp_attributes "spice.codec" in
      let encode, decode =
        match custom_codec with
        | Ok None -> generate_constr_codecs generator_settings constr
        | Ok (Some attribute) ->
            let expr = get_expression_from_payload attribute in
            ( some_if_true do_encode
                [%expr
                  let e, _ = [%e expr] in
                  e],
              some_if_true do_decode
                [%expr
                  let _, d = [%e expr] in
                  d] )
        | Error s -> fail ptyp_loc s
      in
      let encode, decode =
        if is_optional then
          match (encode, decode) with
          | Some encode, Some decode ->
              ( Some [%expr Spice.optionToJson [%e encode]],
                Some [%expr Spice.optionFromJson [%e decode]] )
          | _ -> (encode, decode)
        else (encode, decode)
      in
      match List.length typeArgs = 0 with
      | true -> (encode, decode)
      | false -> parameterize_codecs typeArgs encode decode generator_settings)
  | _ -> fail ptyp_loc "This syntax is not yet handled by spice"
