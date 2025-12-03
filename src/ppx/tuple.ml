open Ppxlib
open Parsetree
open Ast_helper
open Utils

let generate_encoder composite_encoders =
  let arrExp =
    composite_encoders
    |> List.mapi (fun i e ->
           let vExp = Exp.ident (lid ("v" ^ string_of_int i)) in
           [%expr [%e e] [%e vExp]])
    |> Exp.array
  in
  let deconstructor_pattern =
    composite_encoders
    |> List.mapi (fun i _ -> Pat.var (mknoloc ("v" ^ string_of_int i)))
    |> Pat.tuple
  in
  let return_exp =
    Exp.constraint_ [%expr JSON.Array [%e arrExp]] Utils.ctyp_json_t
  in
  Utils.expr_func ~arity:1
    [%expr fun [%p deconstructor_pattern] -> [%e return_exp]]

let generate_decode_switch composite_decoders =
  let num_args = List.length composite_decoders in

  (* Build the final Ok expression with the tuple *)
  let ok_expr =
    let tuple_elements =
      Array.init num_args (fun i -> make_ident_expr ("v" ^ string_of_int i))
      |> Array.to_list
    in
    [%expr Ok [%e Exp.tuple tuple_elements]]
  in

  (* Generate decode expression for each element *)
  let generate_decode_expr i decoder =
    let ident = make_ident_expr ("v" ^ string_of_int i) in
    [%expr [%e decoder] [%e ident]]
  in

  (* Build nested matches from the last element backwards using iterative approach *)
  let build_nested_matches indexed_decoders inner_expr =
    let rec loop acc = function
      | [] -> acc
      | (i, decoder) :: rest ->
          let decode_expr = generate_decode_expr i decoder in
          let var_pat = Pat.var (mknoloc ("v" ^ string_of_int i)) in
          let ok_case = Exp.case
            (Pat.construct (mknoloc (Longident.Lident "Ok")) (Some var_pat)) acc in
          let error_case =
            Exp.case
              (Pat.construct
                 (mknoloc (Longident.Lident "Error"))
                 (Some
                    (Pat.constraint_ (Pat.var (mknoloc "e"))
                       (Typ.constr
                          (mknoloc (Longident.parse "Spice.decodeError"))
                          []))))
              [%expr
                Error
                  { e with path = [%e index_const i] ^ e.path }]
          in
          let match_expr = Exp.match_ decode_expr [ ok_case; error_case ] in
          loop match_expr rest
    in
    loop inner_expr (List.rev indexed_decoders)
  in

  (* Create indexed list of decoders *)
  let indexed_decoders = List.mapi (fun i d -> (i, d)) composite_decoders in
  build_nested_matches indexed_decoders ok_expr

let generate_decoder composite_decoders =
  let match_arr_pattern =
    composite_decoders
    |> List.mapi (fun i _ -> Pat.var (mknoloc ("v" ^ string_of_int i)))
    |> Pat.array
  in
  let match_pattern = [%pat? JSON.Array [%p match_arr_pattern]] in
  let outer_switch =
    Exp.match_
      (Exp.constraint_ [%expr json] Utils.ctyp_json_t)
      [
        Exp.case match_pattern (generate_decode_switch composite_decoders);
        Exp.case
          [%pat? JSON.Array _]
          [%expr Spice.error "Incorrect cardinality" json];
        Exp.case [%pat? _] [%expr Spice.error "Not a tuple" json];
      ]
  in
  Utils.expr_func ~arity:1 [%expr fun json -> [%e outer_switch]]
