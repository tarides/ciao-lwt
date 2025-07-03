open Ocamlformat_utils.Parsing

val occurrences :
  dune_build_dir:string ->
  packages:string list ->
  units:(string -> bool) ->
  ((string * string) * Longident.t Location.loc) list
(** Find all the occurrences of values from module matched by [units] of
    packages matching the ocamlfind query specified with [package_query] in the
    [.ocaml-index] files found in Dune's [_build] directory. *)
