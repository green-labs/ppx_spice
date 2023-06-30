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
  [%expr fun [%p deconstructor_pattern] -> Js.Json.array [%e arrExp]]

let generate_decode_success_case num_args =
  {
    pc_lhs =
      Array.init num_args (fun i ->
          mknoloc ("v" ^ string_of_int i) |> Pat.var |> fun p ->
          [%pat? Belt.Result.Ok [%p p]])
      |> Array.to_list
      |> tuple_or_singleton Pat.tuple;
    pc_guard = None;
    pc_rhs =
      ( Array.init num_args (fun i -> make_ident_expr ("v" ^ string_of_int i))
      |> Array.to_list |> Exp.tuple
      |> fun e -> [%expr Belt.Result.Ok [%e e]] );
  }

let generate_decode_switch composite_decoders =
  let decode_expr =
    composite_decoders
    |> List.mapi (fun i d ->
           let ident = make_ident_expr ("v" ^ string_of_int i) in
           [%expr [%e d] [%e ident]])
    |> Exp.tuple
  in
  composite_decoders
  |> List.mapi
       (Decode_cases.generate_error_case (List.length composite_decoders))
  |> List.append
       [ generate_decode_success_case (List.length composite_decoders) ]
  |> Exp.match_ decode_expr

let generate_decoder composite_decoders =
  let match_arr_pattern =
    composite_decoders
    |> List.mapi (fun i _ -> Pat.var (mknoloc ("v" ^ string_of_int i)))
    |> Pat.array
  in
  let match_pattern = [%pat? Js.Json.JSONArray [%p match_arr_pattern]] in
  let outer_switch =
    Exp.match_ [%expr Js.Json.classify json]
      [
        Exp.case match_pattern (generate_decode_switch composite_decoders);
        Exp.case
          [%pat? Js.Json.JSONArray _]
          [%expr Spice.error "Incorrect cardinality" json];
        Exp.case [%pat? _] [%expr Spice.error "Not a tuple" json];
      ]
  in
  [%expr fun json -> [%e outer_switch]]
