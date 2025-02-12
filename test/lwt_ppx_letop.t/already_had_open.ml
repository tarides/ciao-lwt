open Lwt.Syntax

(* This file already has [open Lwt.Syntax], no need to add one more. *)

let _ =
  let%lwt a = b in
  c
