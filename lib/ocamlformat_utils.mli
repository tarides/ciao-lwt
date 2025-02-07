open Ocamlformat_lib

val build_config : file:string -> Conf.t

val parse_and_format :
  'ast Extended_ast.t ->
  input_name:string ->
  source:string ->
  modify_ast:('ast -> 'ast) ->
  Conf.t ->
  string

module Parsetree = Ocamlformat_lib.Extended_ast

val format_structure_in_place :
  file:string -> modify_ast:(Parsetree.structure -> Parsetree.structure) -> unit
(** Format the content of and overwrite [file]. [modify_ast] can be used to
    apply any change to the program. Raises [Failure] and [Sys_error]. *)
