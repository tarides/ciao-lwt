(* SPDX-License-Identifier: MIT
 * Copyright (c) 2025 Jules Aguillon <jules@j3s.fr>
 *)

open Ocamlformat_utils.Parsing
open Parsetree

type state

module Occ : sig
  (** Manage occurrences of identifiers that should be migrated. *)

  val pop : state -> 'a Location.loc -> bool
  (** Whether the given longident is an occurrence of an Lwt function. This will
      change the internal table and any subsequent calls for the same longident
      will return [false]. *)

  val get : state -> 'a Location.loc -> (string * string) option

  val may_rewrite :
    state -> 'a Location.loc -> (string * string -> 'b option) -> 'b option
  (** Calls [f] if [lid] should be rewritten. A rewrite happen if
      [f (unit_name, ident)] is [Some ast] where [ident] is the basename of the
      longident at [lid]. The occurrence is removed from the table if a rewrite
      happened. *)

  val remove : state -> 'a Location.loc -> unit
  (** Remove an occurrence from the table. *)
end

val add_comment : state -> ?loc:Location.t -> string -> unit
(** Add comments in the output. Comments are attached to a location that must
    appear in the output AST and cannot be a ghost location. If [loc] is [None],
    use the location set with [set_default_comment_loc] instead. *)

val set_default_comment_loc : state -> Location.t -> unit
(** Set the default location for comments added with [add_comment]. *)

type modify_ast = {
  structure : state -> structure -> structure;
  signature : state -> signature -> signature;
}

val migrate :
  packages:string list ->
  units:(string -> bool) ->
  modify_ast:(fname:string -> modify_ast) ->
  (unit, [ `Msg of string ]) result
(** Modify files containing occurrences to modules matched by [units] of
    packages [packages] using [modify_ast]. Will scan file in [_build] and in
    the current directory. *)

val print_occurrences :
  packages:string list ->
  units:(string -> bool) ->
  (unit, [ `Msg of string ]) result
(** Print occurrences that would be rewritten by [migrate]. *)
