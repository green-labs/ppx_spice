open Ppxlib
open Parsetree
open Ast_helper
open Utils

type parsedDecl = {
  name : string;
  alias : expression;
  constrDecl : Parsetree.constructor_declaration;
}

let generateEncoderCase generatorSettings
    { name; alias; constrDecl = { pcd_args; pcd_loc } } =
  let alias_name = getStringFromExpression alias in
  let constructorExpr =
    Exp.constant (Pconst_string (alias_name, Location.none, None))
  in

  {
    pc_lhs = Pat.construct (lid name) None;
    pc_guard = None;
    pc_rhs = [%expr Js.Json.string [%e constructorExpr]];
  }

let generateDecoderCase generatorSettings
    { name; alias; constrDecl = { pcd_args; pcd_loc } } =
  let decoded =
    let ident = lid name in
    [%expr Belt.Result.Ok [%e Exp.construct ident None]]
  in

  let alias_name = getStringFromExpression alias in

  {
    pc_lhs =
      ( Pconst_string (alias_name, Location.none, None) |> Pat.constant
      |> fun v -> Some v |> Pat.construct (lid "Js.Json.JSONString") );
    pc_guard = None;
    pc_rhs = [%expr [%e decoded]];
  }

let generateUnboxedDecode generatorSettings
    { pcd_name = { txt = name }; pcd_args; pcd_loc } =
  match pcd_args with
  | Pcstr_tuple args -> (
      match args with
      | [ a ] -> (
          let _, d = Codecs.generateCodecs generatorSettings a in
          match d with
          | Some d ->
              let constructor = Exp.construct (lid name) (Some [%expr v]) in
              Some
                [%expr
                  fun v -> Belt.Result.map (([%e d] v), fun v -> [%e constructor])]
          | None -> None)
      | _ -> fail pcd_loc "Expected exactly one type argument")
  | Pcstr_record _ -> fail pcd_loc "This syntax is not yet implemented by decco"

let parseDecl generatorSettings
    ({ pcd_name = { txt }; pcd_loc; pcd_attributes } as constrDecl) =
  let ({ pexp_desc } as alias) =
    match getAttributeByName pcd_attributes "spice.as" with
    | Ok (Some attribute) -> getExpressionFromPayload attribute
    | Ok None -> Exp.constant (Pconst_string (txt, Location.none, None))
    | Error s -> fail pcd_loc s
  in

  { name = txt; alias; constrDecl }

let generateCodecs ({ doEncode; doDecode } as generatorSettings) constrDecls
    unboxed =
  let parsedDecls = List.map (parseDecl generatorSettings) constrDecls in

  let encoder =
    match doEncode with
    | true ->
        List.map (generateEncoderCase generatorSettings) parsedDecls
        |> Exp.match_ [%expr v]
        |> Exp.fun_ Asttypes.Nolabel None [%pat? v]
        |> Option.some
    | false -> None
  in

  let decoderDefaultCase =
    {
      pc_lhs = [%pat? _];
      pc_guard = None;
      pc_rhs =
        [%expr Decco.error "Invalid variant constructor" v];
    }
  in

  let decoder =
    match not doDecode with
    | true -> None
    | false -> (
        match unboxed with
        | true -> generateUnboxedDecode generatorSettings (List.hd constrDecls)
        | false ->
            let decoderSwitch =
              List.map (generateDecoderCase generatorSettings) parsedDecls
              |> fun l ->
              l @ [ decoderDefaultCase ] |> Exp.match_ [%expr tagged]
            in

            Some
              [%expr
                fun v ->
                  match Js.Json.classify v with
                  | Js.Json.JSONString _ ->
                      let tagged = Js.Json.classify v in
                      [%e decoderSwitch]
                  | _ -> Decco.error "Not a variants" v])
  in

  (encoder, decoder)
