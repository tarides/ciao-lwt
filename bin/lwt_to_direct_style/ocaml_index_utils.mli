open Ocaml_parsing

type t

val of_paths : string list -> t
(** Build an index from a list of [.ocaml-index] files. *)

val locs_from_comp_unit : t -> string -> Longident.t Location.loc list
(** Extract every occurrences of identifiers coming from the given compilation
    unit. *)
