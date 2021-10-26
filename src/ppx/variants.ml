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
  let alias_name, _, delimit = getStringFromExpression alias in
  let constructorExpr =
    Exp.constant (Pconst_string (alias_name, Location.none, delimit))
  in

  {
    pc_lhs = Pat.construct (lid name) None;
    pc_guard = None;
    pc_rhs = [%expr [%e constructorExpr]];
  }

let generateDecoderCase generatorSettings
    { name; alias; constrDecl = { pcd_args; pcd_loc } } =
  let alias_name, _, delimit = getStringFromExpression alias in
  let decoded =
    let ident = lid name in
    [%expr Belt.Result.Ok [%e Exp.construct ident None]]
  in

  let if' =
    Exp.apply (makeIdentExpr "=")
      [
        ( Asttypes.Nolabel,
          Pconst_string (alias_name, Location.none, delimit) |> Exp.constant );
        (Asttypes.Nolabel, [%expr v]);
      ]
  in
  let then' = [%expr [%e decoded]] in

  (if', then')

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

  let rec makeIfThenElse cases =
    match cases with
    | [] -> [%expr Belt.Result.Error ("Not matched" ^ v)]
    | hd :: tl ->
        let if_, then_ = hd in
        Exp.ifthenelse if_ then_ (Some (makeIfThenElse tl))
  in

  let decoder =
    match not doDecode with
    | true -> None
    | false ->
        let decoderSwitch =
          List.map (generateDecoderCase generatorSettings) parsedDecls
          |> makeIfThenElse
        in

        Some [%expr fun v -> [%e decoderSwitch]]
  in

  (encoder, decoder)
