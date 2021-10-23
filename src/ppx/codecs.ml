open Ppxlib
open Parsetree
open Ast_helper
open Utils

let rec parameterizeCodecs typeArgs encoderFunc decoderFunc generatorSettings =
  let subEncoders, subDecoders =
    typeArgs
    |> List.map (fun core_type -> generateCodecs generatorSettings core_type)
    |> List.split
  in
  ( (match encoderFunc with
    | None -> None
    | Some encoderFunc ->
        subEncoders
        |> List.map (fun e -> (Asttypes.Nolabel, Option.get e))
        |> Exp.apply encoderFunc |> Option.some),
    match decoderFunc with
    | None -> None
    | Some decoderFunc ->
        subDecoders
        |> List.map (fun e -> (Asttypes.Nolabel, Option.get e))
        |> Exp.apply decoderFunc |> Option.some )

and generateConstrCodecs { doEncode; doDecode }
    { Location.txt = identifier; loc } =
  let open Longident in
  match identifier with
  | Lident "string" -> (
      ( (match doEncode with
        | true -> Some [%expr Decco.stringToJson]
        | false -> None),
        match doDecode with
        | true -> Some [%expr Decco.stringFromJson]
        | false -> None ))
  | Lident "int" -> (
      ( (match doEncode with
        | true -> Some [%expr Decco.intToJson]
        | false -> None),
        match doDecode with
        | true -> Some [%expr Decco.intFromJson]
        | false -> None ))
  | Lident "int64" -> (
      ( (match doEncode with
        | true -> Some [%expr Decco.int64ToJson]
        | false -> None),
        match doDecode with
        | true -> Some [%expr Decco.int64FromJson]
        | false -> None ))
  | Lident "float" -> (
      ( (match doEncode with
        | true -> Some [%expr Decco.floatToJson]
        | false -> None),
        match doDecode with
        | true -> Some [%expr Decco.floatFromJson]
        | false -> None ))
  | Lident "bool" -> (
      ( (match doEncode with
        | true -> Some [%expr Decco.boolToJson]
        | false -> None),
        match doDecode with
        | true -> Some [%expr Decco.boolFromJson]
        | false -> None ))
  | Lident "unit" -> (
      ( (match doEncode with
        | true -> Some [%expr Decco.unitToJson]
        | false -> None),
        match doDecode with
        | true -> Some [%expr Decco.unitFromJson]
        | false -> None ))
  | Lident "array" -> (
      ( (match doEncode with
        | true -> Some [%expr Decco.arrayToJson]
        | false -> None),
        match doDecode with
        | true -> Some [%expr Decco.arrayFromJson]
        | false -> None ))
  | Lident "list" -> (
      ( (match doEncode with
        | true -> Some [%expr Decco.listToJson]
        | false -> None),
        match doDecode with
        | true -> Some [%expr Decco.listFromJson]
        | false -> None ))
  | Lident "option" -> (
      ( (match doEncode with
        | true -> Some [%expr Decco.optionToJson]
        | false -> None),
        match doDecode with
        | true -> Some [%expr Decco.optionFromJson]
        | false -> None ))
  | Ldot (Ldot (Lident "Belt", "Result"), "t") -> (
      ( (match doEncode with
        | true -> Some [%expr Decco.resultToJson]
        | false -> None),
        match doDecode with
        | true -> Some [%expr Decco.resultFromJson]
        | false -> None ))
  | Ldot (Ldot (Lident "Js", "Dict"), "t") -> (
      ( (match doEncode with
        | true -> Some [%expr Decco.dictToJson]
        | false -> None),
        match doDecode with
        | true -> Some [%expr Decco.dictFromJson]
        | false -> None ))
  | Ldot (Ldot (Lident "Js", "Json"), "t") -> (
      ( (match doEncode with true -> Some [%expr fun v -> v] | false -> None),
        match doDecode with
        | true -> Some [%expr fun v -> Belt.Result.Ok v]
        | false -> None ))
  | Lident s -> (
      ( (match doEncode with
        | true -> Some (makeIdentExpr (s ^ Utils.encoderFuncSuffix))
        | false -> None),
        match doDecode with
        | true -> Some (makeIdentExpr (s ^ Utils.decoderFuncSuffix))
        | false -> None ))
  | Ldot (left, right) -> (
      ( (match doEncode with
        | true ->
            Some
              (Exp.ident
                 (mknoloc (Ldot (left, right ^ Utils.encoderFuncSuffix))))
        | false -> None),
        match doDecode with
        | true ->
            Some
              (Exp.ident
                 (mknoloc (Ldot (left, right ^ Utils.decoderFuncSuffix))))
        | false -> None ))
  | Lapply (_, _) -> fail loc "Lapply syntax not yet handled by decco"

and generateCodecs ({ doEncode; doDecode } as generatorSettings)
    { ptyp_desc; ptyp_loc; ptyp_attributes } =
  match ptyp_desc with
  | Ptyp_any -> fail ptyp_loc "Can't generate codecs for `any` type"
  | Ptyp_arrow (_, _, _) ->
      fail ptyp_loc "Can't generate codecs for function type"
  | Ptyp_package _ -> fail ptyp_loc "Can't generate codecs for module type"
  | Ptyp_tuple types -> (
      let compositeCodecs = List.map (generateCodecs generatorSettings) types in
      ( (match doEncode with
        | true ->
            Some
              (compositeCodecs
              |> List.map (fun (e, _) -> Option.get e)
              |> Tuple.generateEncoder)
        | false -> None),
        match doDecode with
        | true ->
            Some
              (compositeCodecs
              |> List.map (fun (_, d) -> Option.get d)
              |> Tuple.generateDecoder)
        | false -> None ))
  | Ptyp_var s -> (
      ( (match doEncode with
        | true -> Some (makeIdentExpr (encoderVarPrefix ^ s))
        | false -> None),
        match doDecode with
        | true -> Some (makeIdentExpr (decoderVarPrefix ^ s))
        | false -> None ))
  | Ptyp_constr (constr, typeArgs) -> (
      let customCodec = getAttributeByName ptyp_attributes "decco.codec" in
      let encode, decode =
        match customCodec with
        | Ok None -> generateConstrCodecs generatorSettings constr
        | Ok (Some attribute) -> (
            let expr = getExpressionFromPayload attribute in

            ( (match doEncode with
              | true ->
                  Some
                    [%expr
                      let e, _ = [%e expr] in
                      e]
              | false -> None),
              match doDecode with
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
      | false -> parameterizeCodecs typeArgs encode decode generatorSettings)
  | _ -> fail ptyp_loc "This syntax is not yet handled by decco"
