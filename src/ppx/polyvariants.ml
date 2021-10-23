open Parsetree
open Ast_helper
open Utils

(* Polyvariants arguments are wrapped inside a Tuple, meaning that if there's only
   one arg it's the coreType, but if there's more than one arg it's a tuple of one tuple with those args.
   This function abstract this particuliarity from polyvariants (It's different from Variants). *)

type parsedField = {
  name : string;
  alias : expression;
  rowField : Parsetree.row_field;
}

let generateEncoderCase generatorSettings unboxed row =
  let { name; alias; rowField = { prf_desc } } = row in
  let alias_name = getStringFromExpression alias in
  match prf_desc with
  | Rtag ({ loc }, _attributes, coreTypes) ->
      let constructorExpr =
        Exp.constant (Pconst_string (alias_name, Location.none, None))
      in

      {
        pc_lhs = Pat.variant name None;
        pc_guard = None;
        pc_rhs = [%expr Js.Json.string [%e constructorExpr]];
      }
  (* We don't have enough information to generate a encoder *)
  | Rinherit arg ->
      fail arg.ptyp_loc "This syntax is not yet implemented by decco"

let generateDecoderCase generatorSettings row =
  let { name; alias; rowField = { prf_desc } } = row in
  match prf_desc with
  | ((Rtag ({ txt; loc }, _, coreTypes)) [@explicit_arity]) ->
      let decoded =
        let resultantExp = Exp.variant txt None in
        [%expr Belt.Result.Ok [%e resultantExp]]
      in

      let alias_name = getStringFromExpression alias in

      {
        pc_lhs =
          ( Pconst_string (alias_name, Location.none, None) |> Pat.constant
          |> fun v -> Some v |> Pat.construct (lid "Js.Json.JSONString") );
        pc_guard = None;
        pc_rhs = [%expr [%e decoded]];
      }
  | ((Rinherit coreType) [@explicit_arity]) ->
      fail coreType.ptyp_loc "This syntax is not yet implemented by decco"

let generateUnboxedDecode generatorSettings row =
  let { prf_desc } = row in
  match prf_desc with
  | Rtag ({ txt; loc }, _, args) -> (
      match args with
      | [ a ] -> (
          let _, d = Codecs.generateCodecs generatorSettings a in
          match d with
          | Some d ->
              let constructor = Exp.construct (lid txt) (Some [%expr v]) in

              Some
                [%expr
                  fun v -> Belt.Result.map ([%e d] v, fun v -> [%e constructor])]
          | None -> None)
      | _ -> fail loc "Expected exactly one type argument")
  | Rinherit coreType ->
      fail coreType.ptyp_loc "This syntax is not yet implemented by decco"

let parseDecl generatorSettings
    ({ prf_desc; prf_loc; prf_attributes } as rowField) =
  let txt =
    match prf_desc with
    | Rtag ({ txt }, _, _) -> txt
    | _ -> failwith "cannot get polymorphic variant constructor"
  in

  let alias =
    match getAttributeByName prf_attributes "spice.as" with
    | Ok (Some attribute) -> getExpressionFromPayload attribute
    | Ok None -> Exp.constant (Pconst_string (txt, Location.none, None))
    | Error s -> fail prf_loc s
  in

  { name = txt; alias; rowField }

let generateCodecs ({ doEncode; doDecode } as generatorSettings) rowFields
    unboxed =
  let parsedFields = List.map (parseDecl generatorSettings) rowFields in

  let encoder =
    match doEncode with
    | true ->
        List.map (generateEncoderCase generatorSettings unboxed) parsedFields
        |> Exp.match_ [%expr v]
        |> Exp.fun_ Asttypes.Nolabel None [%pat? v]
        |> Option.some
    | false -> None
  in

  let decoderDefaultCase =
    {
      pc_lhs = [%pat? _];
      pc_guard = None;
      pc_rhs = [%expr Decco.error "Invalid polyvariant constructor" v];
    }
  in
  let decoder =
    match not doDecode with
    | true -> None
    | false -> (
        match unboxed with
        | true -> generateUnboxedDecode generatorSettings (List.hd rowFields)
        | false ->
            let decoderSwitch =
              List.map (generateDecoderCase generatorSettings) parsedFields
              |> fun l ->
              l @ [ decoderDefaultCase ] |> Exp.match_ [%expr tagged]
            in
            (Some
               [%expr
                 fun v ->
                   match Js.Json.classify v with
                   | Js.Json.JSONString _ ->
                       let tagged = Js.Json.classify v in
                       [%e decoderSwitch]
                   | _ -> Decco.error "Not a polyvariant" v]
            [@explicit_arity]))
  in
  (encoder, decoder)
