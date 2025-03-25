open Ocamlformat_utils.Parsing

type occurrences = ((string * string) * Longident.t Location.loc) list

val migrate :
  packages:string list ->
  units:(string -> bool) ->
  modify_ast:(fname:string -> occurrences -> Ocamlformat_utils.modify_ast) ->
  unit
(** Modify files containing occurrences to modules matched by [units] of
    packages [packages] using [modify_ast]. Will scan file in [_build] and in
    the current directory. *)

val print_occurrences : packages:string list -> units:(string -> bool) -> unit
(** Print occurrences that would be rewritten by [migrate]. *)
