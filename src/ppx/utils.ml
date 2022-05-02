open Ppxlib
open Parsetree
open Ast_helper

let annotation_name = "spice"

let encoder_func_suffix = "_encode"

let decoder_func_suffix = "_decode"

let encoder_var_prefix = "encoder_"

let decoder_var_prefix = "decoder_"

let loc = !default_loc

let fail loc message = Location.raise_errorf ~loc "%s" message

let longident_parse = Longident.parse [@@ocaml.warning "-3"]

let mkloc txt loc = { Location.txt; loc }

let mknoloc txt = mkloc txt Location.none

let lid ?(loc = Location.none) s = mkloc (Longident.parse s) loc

let make_ident_expr s = Exp.ident (mknoloc (longident_parse s))

let tuple_or_singleton tuple l =
  match List.length l > 1 with true -> tuple l | false -> List.hd l

let get_attribute_by_name attributes name =
  let filtered =
    attributes
    |> List.filter (fun { attr_name = { Location.txt } } -> txt = name)
  in
  match filtered with
  | [] -> Ok None
  | [ attribute ] -> Ok (Some attribute)
  | _ -> Error ("Too many occurrences of \"" ^ name ^ "\" attribute")

type generator_settings = { do_encode : bool; do_decode : bool }

let get_generator_settings_from_attributes attributes =
  match get_attribute_by_name attributes annotation_name with
  | Ok None -> (
      match
        ( get_attribute_by_name attributes (annotation_name ^ ".decode"),
          get_attribute_by_name attributes (annotation_name ^ ".encode") )
      with
      | Ok (Some _), Ok (Some _) ->
          Ok (Some { do_encode = true; do_decode = true })
      | Ok (Some _), Ok None ->
          Ok (Some { do_encode = false; do_decode = true })
      | Ok None, Ok (Some _) ->
          Ok (Some { do_encode = true; do_decode = false })
      | Ok None, Ok None -> Ok None
      | (Error _ as e), _ -> e
      | _, (Error _ as e) -> e)
  | Ok (Some _) -> Ok (Some { do_encode = true; do_decode = true })
  | Error _ as e -> e

let get_expression_from_payload { attr_name = { loc }; attr_payload = payload }
    =
  match payload with
  | PStr [ { pstr_desc } ] -> (
      match pstr_desc with
      | Pstr_eval (expr, _) -> expr
      | _ -> fail loc "Expected expression as attribute payload")
  | _ -> fail loc "Expected expression as attribute payload"

let get_param_names params =
  params
  |> List.map (fun ({ ptyp_desc; ptyp_loc }, _) ->
         match ptyp_desc with
         | Ptyp_var s -> s
         | _ ->
             fail ptyp_loc "Unhandled param type" |> fun v ->
             Location.Error v |> raise)

let get_string_from_expression { pexp_desc; pexp_loc } =
  match pexp_desc with
  | Pexp_constant const -> (
      match const with
      | Pconst_string (name, loc, delimit) -> (name, loc, delimit)
      | _ -> fail pexp_loc "cannot find a name??")
  | _ -> fail pexp_loc "cannot find a name??"

let index_const i =
  Pconst_string ("[" ^ string_of_int i ^ "]", Location.none, None)
  |> Exp.constant

let rec is_identifier_used_in_core_type type_name { ptyp_desc; ptyp_loc } =
  match ptyp_desc with
  | Ptyp_arrow (_, _, _) ->
      fail ptyp_loc "Can't generate codecs for function type"
  | Ptyp_any -> fail ptyp_loc "Can't generate codecs for `any` type"
  | Ptyp_package _ -> fail ptyp_loc "Can't generate codecs for module type"
  | Ptyp_variant (_, _, _) -> fail ptyp_loc "Unexpected Ptyp_variant"
  | Ptyp_var _ -> false
  | Ptyp_tuple child_types ->
      List.exists (is_identifier_used_in_core_type type_name) child_types
  | Ptyp_constr ({ txt }, child_types) -> (
      match txt = Lident type_name with
      | true -> true
      | false ->
          List.exists (is_identifier_used_in_core_type type_name) child_types)
  | _ -> fail ptyp_loc "This syntax is not yet handled by spice"

let attr_warning expr =
  {
    attr_name = mkloc "ocaml.warning" loc;
    attr_payload = PStr [ { pstr_desc = Pstr_eval (expr, []); pstr_loc = loc } ];
    attr_loc = loc;
  }

let some_if_true cond a = match cond with true -> Some a | false -> None
