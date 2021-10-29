open Parsetree
open Ast_helper
open Utils

(* Polyvariants arguments are wrapped inside a Tuple, meaning that if there's only
   one arg it's the coreType, but if there's more than one arg it's a tuple of one tuple with those args.
   This function abstract this particuliarity from polyvariants (It's different from Variants). *)

type parsed_field = {
  name : string;
  alias : expression;
  row_field : Parsetree.row_field;
}

let generate_encoder_case _generator_settings _unboxed row =
  let { name; alias; row_field = { prf_desc } } = row in
  let alias_name, _, delimit = get_string_from_expression alias in
  match prf_desc with
  | Rtag (_, _attributes, _) ->
      let constructor_expr =
        Exp.constant (Pconst_string (alias_name, Location.none, delimit))
      in

      {
        pc_lhs = Pat.variant name None;
        pc_guard = None;
        pc_rhs = [%expr [%e constructor_expr]];
      }
  (* We don't have enough information to generate a encoder *)
  | Rinherit arg ->
      fail arg.ptyp_loc "This syntax is not yet implemented by spice"

let generate_decoder_case _generator_settings row =
  let { alias; row_field = { prf_desc } } = row in
  match prf_desc with
  | Rtag ({ txt }, _, _) ->
      let alias_name, loc, delimit = get_string_from_expression alias in
      let decoded =
        let resultant_exp = Exp.variant txt None in
        [%expr Belt.Result.Ok [%e resultant_exp]]
      in

      let if' =
        Exp.apply (make_ident_expr "=")
          [
            ( Asttypes.Nolabel,
              Pconst_string (alias_name, Location.none, delimit) |> Exp.constant
            );
            (Asttypes.Nolabel, [%expr v]);
          ]
      in
      let then' = [%expr [%e decoded]] in

      (if', then')
  | Rinherit core_type ->
      fail core_type.ptyp_loc "This syntax is not yet implemented by spice"

let parse_decl _generator_settings
    ({ prf_desc; prf_loc; prf_attributes } as row_field) =
  let txt =
    match prf_desc with
    | Rtag ({ txt }, _, _) -> txt
    | _ -> failwith "cannot get polymorphic variant constructor"
  in

  let alias =
    match get_attribute_by_name prf_attributes "spice.as" with
    | Ok (Some attribute) -> get_expression_from_payload attribute
    | Ok None -> Exp.constant (Pconst_string (txt, Location.none, None))
    | Error s -> fail prf_loc s
  in

  { name = txt; alias; row_field }

let generate_codecs ({ do_encode; do_decode } as generator_settings) row_fields
    unboxed =
  let parsed_fields = List.map (parse_decl generator_settings) row_fields in

  let encoder =
    match do_encode with
    | true ->
        List.map (generate_encoder_case generator_settings unboxed) parsed_fields
        |> Exp.match_ [%expr v]
        |> Exp.fun_ Asttypes.Nolabel None [%pat? v]
        |> Option.some
    | false -> None
  in

  let rec make_ifthenelse cases =
    match cases with
    | [] -> [%expr Belt.Result.Error ("Not matched" ^ v)]
    | hd :: tl ->
        let if_, then_ = hd in
        Exp.ifthenelse if_ then_ (Some (make_ifthenelse tl))
  in

  let decoder =
    match not do_decode with
    | true -> None
    | false ->
        let decoder_switch =
          List.map (generate_decoder_case generator_settings) parsed_fields
          |> make_ifthenelse
        in

        Some [%expr fun v -> [%e decoder_switch]]
  in

  (encoder, decoder)
