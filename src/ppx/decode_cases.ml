open Ppxlib
open Parsetree
open Ast_helper
open Utils

(* The valueOffset is for adjusting the index in error paths, for example for Polyvariants *)
let generate_error_case ?(valueOffset = 0) numArgs i _ =
  {
    pc_lhs =
      Array.init numArgs (fun which ->
          match which == i with
          | true -> [%pat? Error (e : Spice.decodeError)]
          | false -> [%pat? _])
      |> Array.to_list
      |> tuple_or_singleton Pat.tuple;
    pc_guard = None;
    pc_rhs =
      [%expr Spice.error ~path: ([%e index_const (i + valueOffset)] ^ e.path) e.message e.value];
  }
