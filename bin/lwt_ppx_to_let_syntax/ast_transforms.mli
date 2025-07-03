(* SPDX-License-Identifier: MIT
 * Copyright (c) 2025 Jules Aguillon <jules@j3s.fr>
 *)

val remove_lwt_ppx : use_lwt_bind:bool -> Ocamlformat_utils.modify_ast
(** Rewrite occurrences of lwt ppx to Lwt library function calls. *)
