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

let getArgsFromPolyvars ~loc coreTypes =
  match coreTypes with
  | [] -> []
  | [ coreType ] -> (
      match coreType.ptyp_desc with
      (* If it's a tuple, return the args *)
      | Ptyp_tuple coreTypes -> coreTypes
      (* If it's any other coreType, return it *)
      | _ -> [ coreType ])
  | _ ->
      fail loc
        "This error shoudn't happen, means that the AST of your polyvariant is \
         wrong"

let generateEncoderCase generatorSettings unboxed row =
  let { name; alias; rowField = { prf_desc } } = row in
  let alias_name = getStringFromExpression alias in
  match prf_desc with
  | Rtag ({ loc }, _attributes, coreTypes) ->
      let constructorExpr =
        Exp.constant (Pconst_string (alias_name, Location.none, None))
      in
      let args = getArgsFromPolyvars ~loc coreTypes in

      let lhsVars =
        match args with
        | [] -> None
        | [ _ ] -> Some (Pat.var (mknoloc "v0"))
        | _ ->
            args
            |> List.mapi (fun i _ ->
                   mkloc ("v" ^ string_of_int i) loc |> Pat.var)
            |> Pat.tuple
            |> fun v -> Some v
      in

      let rhsList =
        args
        |> List.map (Codecs.generateCodecs generatorSettings)
        |> List.map (fun (encoder, _) -> Option.get encoder)
        |> List.mapi (fun i e ->
               Exp.apply ~loc e
                 [ (Asttypes.Nolabel, makeIdentExpr ("v" ^ string_of_int i)) ])
        |> List.append [ [%expr Js.Json.string [%e constructorExpr]] ]
      in

      {
        pc_lhs = Pat.variant name lhsVars;
        pc_guard = None;
        pc_rhs =
          (if unboxed then List.tl rhsList |> List.hd
          else [%expr Js.Json.array [%e rhsList |> Exp.array]]);
      }
  (* We don't have enough information to generate a encoder *)
  | Rinherit arg ->
      fail arg.ptyp_loc "This syntax is not yet implemented by decco"

let generateDecodeSuccessCase numArgs constructorName =
  {
    pc_lhs =
      Array.init numArgs (fun i ->
          mknoloc ("v" ^ string_of_int i) |> Pat.var |> fun p ->
          [%pat? Belt.Result.Ok [%p p]])
      |> Array.to_list |> tupleOrSingleton Pat.tuple;
    pc_guard = None;
    pc_rhs =
      ( Array.init numArgs (fun i -> makeIdentExpr ("v" ^ string_of_int i))
      |> Array.to_list |> tupleOrSingleton Exp.tuple
      |> (fun v -> Some v)
      |> Exp.variant constructorName
      |> fun e -> [%expr Belt.Result.Ok [%e e]] );
  }

let generateArgDecoder generatorSettings args constructorName =
  let numArgs = List.length args in
  args
  |> List.mapi (Decode_cases.generateErrorCase numArgs)
  |> List.append [ generateDecodeSuccessCase numArgs constructorName ]
  |> Exp.match_
       (args
       |> List.map (Codecs.generateCodecs generatorSettings)
       |> List.mapi (fun i (_, decoder) ->
              Exp.apply (Option.get decoder)
                [
                  ( Asttypes.Nolabel,
                    let idx =
                      Pconst_integer (string_of_int (i + 1), None)
                      |> Exp.constant
                    in
                    [%expr Belt.Array.getExn jsonArr [%e idx]] );
                ])
       |> tupleOrSingleton Exp.tuple)

let generateDecoderCase generatorSettings row =
  let { name; alias; rowField = { prf_desc } } = row in
  match prf_desc with
  | ((Rtag ({ txt; loc }, _, coreTypes)) [@explicit_arity]) ->
      let args = getArgsFromPolyvars ~loc coreTypes in
      
      let argLen =
        (Pconst_integer (string_of_int (List.length args + 1), None)
        [@explicit_arity])
        |> Exp.constant
      in
      
      let decoded =
        match args with
        | [] ->
            let resultantExp = Exp.variant txt None in
            [%expr Belt.Result.Ok [%e resultantExp]]
        | _ -> generateArgDecoder generatorSettings args txt
      in

      let alias_name = getStringFromExpression alias in

      {
        pc_lhs =
          (Pconst_string (alias_name, Location.none, None)
          |> Pat.constant
          |> fun v ->
            (Some v [@explicit_arity])
            |> Pat.construct (lid "Js.Json.JSONString") );
        pc_guard = None;
        pc_rhs =
          [%expr
            match Js.Array.length tagged != [%e argLen] with
            | true ->
                Decco.error
                  "Invalid number of arguments to polyvariant constructor" v
            | false -> [%e decoded]];
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
      pc_rhs =
        [%expr
          Decco.error "Invalid polyvariant constructor"
            (Belt.Array.getExn jsonArr 0)];
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
              l @ [ decoderDefaultCase ]
              |> Exp.match_ [%expr Belt.Array.getExn tagged 0]
            in
            (Some
               [%expr
                 fun v ->
                   match Js.Json.classify v with
                   | ((Js.Json.JSONArray [||]) [@explicit_arity]) ->
                       Decco.error "Expected polyvariant, found empty array" v
                   | ((Js.Json.JSONArray jsonArr) [@explicit_arity]) ->
                       let tagged = Js.Array.map Js.Json.classify jsonArr in
                       [%e decoderSwitch]
                   | _ -> Decco.error "Not a polyvariant" v]
            [@explicit_arity]))
  in
  (encoder, decoder)
