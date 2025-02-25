open Ocamlformat_utils.Parsing

val occurrences :
  dune_build_dir:string ->
  package:string ->
  unit:string ->
  (string * Longident.t Location.loc) list
(** Find all the occurrences of values from module [unit] of package [package]
    in the [.ocaml-index] files found in Dune's [_build] directory. *)
