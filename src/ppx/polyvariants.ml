open Ppxlib
open Parsetree
open Ast_helper
open Utils

(* Polyvariants arguments are wrapped inside a Tuple, meaning that if there's only
   one arg it's the coreType, but if there's more than one arg it's a tuple of one tuple with those args.
   This function abstract this particuliarity from polyvariants (It's different from Variants). *)

type parsed_field = {
  name : string;
  alias : expression;
  has_attr_as : bool;
  row_field : Parsetree.row_field;
}

let get_args_from_polyvars ~loc coreTypes =
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

let generate_encoder_case generator_settings unboxed has_attr_as row =
  let { name; alias = constructor_expr; row_field = { prf_desc } } = row in
  match prf_desc with
  | Rtag (_, _attributes, core_types) ->
      let json_expr =
        match constructor_expr with
        | { pexp_desc = Pexp_constant const; pexp_loc } -> (
            match const with
            | Pconst_string _ -> [%expr JSON.String [%e constructor_expr]]
            | Pconst_float _ -> [%expr JSON.Number [%e constructor_expr]]
            | _ -> fail pexp_loc "cannot find a name??")
        | { pexp_loc } -> fail pexp_loc "cannot find a name??"
      in
      let args = get_args_from_polyvars ~loc core_types in

      let lhs_vars =
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

      let rhs_list =
        args
        |> List.map (Codecs.generate_codecs generator_settings)
        |> List.map (fun (encoder, _) -> Option.get encoder)
        |> List.mapi (fun i e ->
               Exp.apply ~loc e
                 [ (Asttypes.Nolabel, make_ident_expr ("v" ^ string_of_int i)) ])
        |> List.append [ json_expr ]
      in

      {
        pc_lhs = Pat.variant name lhs_vars;
        pc_guard = None;
        pc_rhs =
          (if unboxed then List.tl rhs_list |> List.hd (* diff *)
           else if has_attr_as then json_expr
           else [%expr JSON.Array [%e rhs_list |> Exp.array]]);
      }
  (* We don't have enough information to generate a encoder *)
  | Rinherit arg ->
      fail arg.ptyp_loc "This syntax is not yet implemented by spice"

(* O(n) nested matching for polyvariant arg decoding - replaces O(n²) pattern generation *)
let generate_arg_decoder generator_settings args constructor_name =
  let num_args = List.length args in

  (* Build the final Ok expression with the constructed polyvariant *)
  let ok_expr =
    let tuple_elements =
      Array.init num_args (fun i -> make_ident_expr ("v" ^ string_of_int i))
      |> Array.to_list
      |> tuple_or_singleton Exp.tuple
    in
    let constructed = Exp.variant constructor_name (Some tuple_elements) in
    [%expr Ok [%e constructed]]
  in

  (* Generate decode expression for each arg *)
  let generate_decode_expr i (_, decoder) =
    let idx = Pconst_integer (string_of_int (i + 1), None) |> Exp.constant in
    [%expr [%e Option.get decoder] (Array.getUnsafe json_arr [%e idx])]
  in

  (* Build nested matches from the last arg backwards *)
  let build_nested_matches indexed_codecs inner_expr =
    let rec loop acc = function
      | [] -> acc
      | (i, codec) :: rest ->
          let decode_expr = generate_decode_expr i codec in
          let var_pat = Pat.var (mknoloc ("v" ^ string_of_int i)) in
          let ok_case =
            Exp.case
              (Pat.construct (mknoloc (Longident.Lident "Ok")) (Some var_pat))
              acc
          in
          let error_case =
            Exp.case
              (Pat.construct
                 (mknoloc (Longident.Lident "Error"))
                 (Some
                    (Pat.constraint_
                       (Pat.var (mknoloc "e"))
                       (Typ.constr
                          (mknoloc
                             (Longident.Ldot
                                (Longident.Lident "Spice", "decodeError")))
                          []))))
              [%expr
                Error
                  (* +1 because index 0 is the constructor *)
                  { e with path = [%e index_const (i + 1)] ^ e.path }]
          in
          let match_expr = Exp.match_ decode_expr [ ok_case; error_case ] in
          loop match_expr rest
    in
    loop inner_expr (List.rev indexed_codecs)
  in

  (* Create indexed list of codecs *)
  let codecs = List.map (Codecs.generate_codecs generator_settings) args in
  let indexed_codecs = List.mapi (fun i c -> (i, c)) codecs in
  build_nested_matches indexed_codecs ok_expr

let generate_decoder_case generator_settings { prf_desc } =
  match prf_desc with
  | Rtag ({ txt }, _, core_types) ->
      let args = get_args_from_polyvars ~loc core_types in
      let arg_len =
        Pconst_integer (string_of_int (List.length args + 1), None)
        |> Exp.constant
      in
      let decoded =
        match args with
        | [] ->
            let resultant_exp = Exp.variant txt None in
            [%expr Ok [%e resultant_exp]]
        | _ -> generate_arg_decoder generator_settings args txt
      in

      {
        pc_lhs =
          ( Pconst_string (txt, Location.none, None) |> Pat.constant |> fun v ->
            Some v |> Pat.construct (lid "JSON.String") );
        pc_guard = None;
        pc_rhs =
          [%expr
            if Array.length json_arr != [%e arg_len] then
              Spice.error
                "Invalid number of arguments to polyvariant constructor" v
            else [%e decoded]];
      }
  | Rinherit core_type ->
      fail core_type.ptyp_loc "This syntax is not yet implemented by spice"

let generate_decoder_case_attr ~is_string generator_settings row =
  let { alias; row_field = { prf_desc } } = row in
  match prf_desc with
  | Rtag ({ txt }, _, core_types) -> (
      let args = get_args_from_polyvars ~loc core_types in
      let const =
        if is_string then get_string_from_expression alias
        else get_float_from_expression alias
      in
      let decoded =
        match args with
        | [] ->
            let resultant_exp = Exp.variant txt None in
            [%expr Ok [%e resultant_exp]]
        | _ -> generate_arg_decoder generator_settings args txt
      in

      match const with
      | Some const ->
          let if' =
            Exp.apply (make_ident_expr "=")
              [
                (Asttypes.Nolabel, Exp.constant const);
                (Asttypes.Nolabel, [%expr str_or_num]);
              ]
          in
          let then' = [%expr [%e decoded]] in

          Some (if', then')
      | None -> None)
  | Rinherit core_type ->
      fail core_type.ptyp_loc "This syntax is not yet implemented by spice"

let generate_unboxed_decode generator_settings { prf_desc } =
  match prf_desc with
  | Rtag ({ txt; loc }, _, args) -> (
      match args with
      | [ a ] -> (
          let _, d = Codecs.generate_codecs generator_settings a in
          match d with
          | Some d ->
              let constructor = Exp.construct (lid txt) (Some [%expr v]) in

              Some
                (Utils.expr_func ~arity:1
                   [%expr
                     fun v -> Result.map ([%e d] v) (fun v -> [%e constructor])])
          | None -> None)
      | _ -> fail loc "Expected exactly one type argument")
  | Rinherit coreType ->
      fail coreType.ptyp_loc "This syntax is not yet implemented by spice"

let parse_decl ({ prf_desc; prf_loc; prf_attributes } as row_field) =
  let txt =
    match prf_desc with
    | Rtag ({ txt }, _, _) -> txt
    | _ -> failwith "cannot get polymorphic variant constructor"
  in

  let alias, has_attr_as =
    match get_attribute_by_name prf_attributes "spice.as" with
    | Ok (Some attribute) -> (get_expression_from_payload attribute, true)
    | Ok None ->
        (Exp.constant (Pconst_string (txt, Location.none, Some "*j")), false)
    | Error s -> (fail prf_loc s, false)
  in

  { name = txt; alias; has_attr_as; row_field }

let generate_codecs ({ do_encode; do_decode } as generator_settings) row_fields
    unboxed =
  let parsed_fields = List.map parse_decl row_fields in
  let count_has_attr =
    parsed_fields |> List.filter (fun v -> v.has_attr_as) |> List.length
  in
  let has_attr_as =
    if count_has_attr > 0 then
      if count_has_attr = List.length parsed_fields then true
      else failwith "Partial @spice.as usage is not allowed"
    else false
  in

  let encoder =
    if do_encode then
      Some
        (List.map
           (generate_encoder_case generator_settings unboxed has_attr_as)
           parsed_fields
        |> Exp.match_ [%expr v]
        |> Exp.fun_ Asttypes.Nolabel None [%pat? v]
        |> Utils.expr_func ~arity:1)
    else None
  in

  let decoder =
    match not do_decode with
    | true -> None
    | false ->
        if unboxed then
          generate_unboxed_decode generator_settings (List.hd row_fields)
        else if has_attr_as then
          let rec make_ifthenelse cases =
            match cases with
            | [] -> [%expr Spice.error "Not matched" v]
            | hd :: tl ->
                let if_, then_ = hd in
                Exp.ifthenelse if_ then_ (Some (make_ifthenelse tl))
          in

          let decoder_switch =
            parsed_fields
            |> List.filter_map
                 (generate_decoder_case_attr ~is_string:true generator_settings)
            |> make_ifthenelse
          in

          let decoder_switch_num =
            parsed_fields
            |> List.filter_map
                 (generate_decoder_case_attr ~is_string:false generator_settings)
            |> make_ifthenelse
          in

          Some
            (Utils.expr_func ~arity:1
               [%expr
                 fun v ->
                   match (v : JSON.t) with
                   | JSON.String str_or_num -> [%e decoder_switch]
                   | JSON.Number str_or_num -> [%e decoder_switch_num]
                   | _ -> Spice.error "Not a JSONString" v])
        else
          let decoder_default_case =
            {
              pc_lhs = [%pat? _];
              pc_guard = None;
              pc_rhs =
                [%expr
                  Spice.error "Invalid polymorphic variant constructor"
                    (Array.getUnsafe json_arr 0)];
            }
          in

          let decoder_switch =
            row_fields |> List.map (generate_decoder_case generator_settings)
            |> fun l ->
            l @ [ decoder_default_case ]
            |> Exp.match_ [%expr Array.getUnsafe json_arr 0]
          in

          Some
            (Utils.expr_func ~arity:1
               [%expr
                 fun v ->
                   match (v : JSON.t) with
                   | JSON.Array [||] ->
                       Spice.error "Expected polyvariant, found empty array" v
                   | JSON.Array json_arr -> [%e decoder_switch]
                   | _ -> Spice.error "Not a polyvariant" v])
  in

  (encoder, decoder)
