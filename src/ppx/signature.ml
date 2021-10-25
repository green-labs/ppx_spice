open Ppxlib
open Parsetree
open Utils

let rec addEncoderParams paramNames resultType =
  match paramNames with
  | [] -> resultType
  | hd :: tl ->
      [%type: ([%t Ast_helper.Typ.var hd] -> string) -> [%t resultType]]
      |> addEncoderParams tl

let makeResultType valueType =
  [%type: ([%t valueType], Spice.decodeError) Belt.Result.t]

let rec addDecoderParams paramNames resultType =
  match paramNames with
  | [] -> resultType
  | hd :: tl ->
      let decoderParam =
        [%type: string -> [%t makeResultType (Ast_helper.Typ.var hd)]]
      in
      [%type: [%t decoderParam] -> [%t resultType]] |> addDecoderParams tl

let generateSigDecls { doEncode; doDecode } typeName paramNames =
  let encoderPat = typeName ^ Utils.encoderFuncSuffix in
  let decoderPat = typeName ^ Utils.decoderFuncSuffix in
  let valueType =
    paramNames
    |> List.map Ast_helper.Typ.var
    |> Ast_helper.Typ.constr (lid typeName)
  in

  let decls = [] in

  let decls =
    match doEncode with
    | true ->
        decls
        @ [
            [%type: [%t valueType] -> string]
            |> addEncoderParams (List.rev paramNames)
            |> Ast_helper.Val.mk (mknoloc encoderPat)
            |> Ast_helper.Sig.value;
          ]
    | false -> decls
  in

  let decls =
    match doDecode with
    | true ->
        decls
        @ [
            [%type: string -> [%t makeResultType valueType]]
            |> addDecoderParams (List.rev paramNames)
            |> Ast_helper.Val.mk (mknoloc decoderPat)
            |> Ast_helper.Sig.value;
          ]
    | false -> decls
  in

  decls

let mapTypeDecl decl =
  let {
    ptype_attributes;
    ptype_name = { txt = typeName };
    ptype_params;
    ptype_loc;
  } =
    decl
  in

  match getGeneratorSettingsFromAttributes ptype_attributes with
  | Error s -> fail ptype_loc s
  | Ok None -> []
  | Ok (Some generatorSettings) ->
      generateSigDecls generatorSettings typeName (getParamNames ptype_params)

let mapSignatureItem mapper ({ psig_desc } as signatureItem) =
  match psig_desc with
  | Psig_type (_, decls) ->
      let generatedSigItems = decls |> List.map mapTypeDecl |> List.concat in
      mapper#signature_item signatureItem :: generatedSigItems
  | _ -> [ mapper#signature_item signatureItem ]
