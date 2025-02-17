module Parsing : sig
  (** OCamlformat's version of OCaml's parsing modules *)

  include module type of Ocamlformat_ocaml_common
  include module type of Ocamlformat_parser_extended
end

module Ast_utils : module type of Ast_utils
(** Utility functions for contructing AST nodes. *)

open Parsing

val format_structure_in_place :
  file:string ->
  modify_ast:(Parsetree.structure -> Parsetree.structure) ->
  (unit, [ `Msg of string ]) result
(** Format the content of and overwrite [file]. [modify_ast] can be used to
    apply any change to the program. Raises [Failure] and [Sys_error]. *)
