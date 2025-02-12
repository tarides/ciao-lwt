open Ocamlformat_parser_extended.Parsetree

val remove_lwt_ppx : use_lwt_bind:bool -> structure -> structure
(** Rewrite occurrences of lwt ppx to Lwt library function calls. *)
