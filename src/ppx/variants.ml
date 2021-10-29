open Ppxlib
open Parsetree
open Ast_helper
open Utils

type parsed_decl = {
  name : string;
  alias : expression;
  constrDecl : Parsetree.constructor_declaration;
}

let generate_encoder_case _generator_settings { name; alias } =
  let alias_name, _, delimit = get_string_from_expression alias in
  let constructor_expr =
    Exp.constant (Pconst_string (alias_name, Location.none, delimit))
  in

  {
    pc_lhs = Pat.construct (lid name) None;
    pc_guard = None;
    pc_rhs = [%expr [%e constructor_expr]];
  }

let generate_decoder_case _generator_settings { name; alias } =
  let alias_name, _, delimit = get_string_from_expression alias in
  let decoded =
    let ident = lid name in
    [%expr Belt.Result.Ok [%e Exp.construct ident None]]
  in

  let if' =
    Exp.apply (make_ident_expr "=")
      [
        ( Asttypes.Nolabel,
          Pconst_string (alias_name, Location.none, delimit) |> Exp.constant );
        (Asttypes.Nolabel, [%expr v]);
      ]
  in
  let then' = [%expr [%e decoded]] in

  (if', then')

let parse_decl _generator_settings
    ({ pcd_name = { txt }; pcd_loc; pcd_attributes } as constrDecl) =
  let alias =
    match get_attribute_by_name pcd_attributes "spice.as" with
    | Ok (Some attribute) -> get_expression_from_payload attribute
    | Ok None -> Exp.constant (Pconst_string (txt, Location.none, None))
    | Error s -> fail pcd_loc s
  in

  { name = txt; alias; constrDecl }

let generate_codecs ({ do_encode; do_decode } as generator_settings) constr_decls
    _unboxed =
  let parsed_decls = List.map (parse_decl generator_settings) constr_decls in

  let encoder =
    match do_encode with
    | true ->
        List.map (generate_encoder_case generator_settings) parsed_decls
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
          List.map (generate_decoder_case generator_settings) parsed_decls
          |> make_ifthenelse
        in

        Some [%expr fun v -> [%e decoder_switch]]
  in

  (encoder, decoder)
