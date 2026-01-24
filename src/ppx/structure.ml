open Ppxlib
open Parsetree
open Ast_helper
open Codecs
open Utils

(** For parameterized types, wrap the codec expression with function parameters
    for each type parameter's codec. Sets the correct arity for uncurried calls.

    e.g., for [type t<'a, 'b>], generates encoder with signature:
    [('a => JSON.t, 'b => JSON.t) => t<'a, 'b> => JSON.t] *)
let add_params param_names expr =
  match param_names with
  | [] -> expr
  | _ ->
      let num_params, with_params =
        List.fold_right
          (fun s (count, acc) ->
            let pat = Pat.var (mknoloc s) in
            (count + 1, Exp.fun_ Asttypes.Nolabel None pat acc))
          param_names (0, expr)
      in
      Utils.expr_func ~arity:num_params with_params

let generate_codec_decls type_name param_names (encoder, decoder) =
  let encoder_pat = Pat.var (mknoloc (type_name ^ Utils.encoder_func_suffix)) in
  let encoder_param_names =
    List.map (fun s -> encoder_var_prefix ^ s) param_names
  in

  let decoder_pat = Pat.var (mknoloc (type_name ^ Utils.decoder_func_suffix)) in
  let decoder_param_names =
    List.map (fun s -> decoder_var_prefix ^ s) param_names
  in

  let vbs = [] in

  let vbs =
    match encoder with
    | None -> vbs
    | Some encoder ->
        vbs @ [ Vb.mk encoder_pat (add_params encoder_param_names encoder) ]
  in

  let vbs =
    match decoder with
    | None -> vbs
    | Some decoder ->
        vbs @ [ Vb.mk decoder_pat (add_params decoder_param_names decoder) ]
  in

  vbs

let map_type_decl decl =
  let {
    ptype_attributes;
    ptype_name = { txt = type_name };
    ptype_manifest;
    ptype_params;
    ptype_loc;
    ptype_kind;
  } =
    decl
  in

  let is_unboxed =
    match Utils.get_attribute_by_name ptype_attributes "unboxed" with
    | Ok (Some _) -> true
    | _ -> false
  in

  match get_generator_settings_from_attributes ptype_attributes with
  | Ok None -> []
  | Ok (Some generator_settings) -> (
      match (ptype_manifest, ptype_kind) with
      | None, Ptype_abstract ->
          fail ptype_loc "Can't generate codecs for unspecified type"
      | Some { ptyp_desc = Ptyp_variant (row_fields, _, _) }, Ptype_abstract ->
          generate_codec_decls type_name
            (get_param_names ptype_params)
            (Polyvariants.generate_codecs generator_settings row_fields
               is_unboxed)
      | Some manifest, _ ->
          generate_codec_decls type_name
            (get_param_names ptype_params)
            (generate_codecs generator_settings manifest)
      | None, Ptype_variant decls ->
          generate_codec_decls type_name
            (get_param_names ptype_params)
            (Variants.generate_codecs generator_settings decls is_unboxed)
      | None, Ptype_record decls ->
          generate_codec_decls type_name
            (get_param_names ptype_params)
            (Records.generate_codecs generator_settings decls is_unboxed)
      | _ -> fail ptype_loc "This type is not handled by spice")
  | Error s -> fail ptype_loc s

let map_structure_item mapper ({ pstr_desc } as structure_item) =
  match pstr_desc with
  | Pstr_type (rec_flag, decls) -> (
      let value_bindings = decls |> List.map map_type_decl |> List.concat in
      [ mapper#structure_item structure_item ]
      @
      match List.length value_bindings > 0 with
      | true -> [ Str.value rec_flag value_bindings ]
      | false -> [])
  | _ -> [ mapper#structure_item structure_item ]
