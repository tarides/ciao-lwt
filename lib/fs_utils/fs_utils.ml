(* SPDX-License-Identifier: MIT
 * Copyright (c) 2025 Jules Aguillon <jules@j3s.fr>
 *)

(** List files in a directory. Returns an empty array on error. *)
let list_dir p =
  match Sys.readdir p with
  | exception Sys_error msg ->
      Printf.eprintf "%s\n%!" msg;
      [||]
  | files ->
      (* Sorted to ensure reproducibility. *)
      Array.sort String.compare files;
      Array.iteri (fun i fname -> files.(i) <- Filename.concat p fname) files;
      files

let rec scan_dir ~descend_into f acc path =
  match Sys.is_directory path with
  | exception Sys_error msg ->
      Printf.eprintf "%s\n%!" msg;
      acc
  | true ->
      if descend_into path then _scan_dir ~descend_into f acc path else acc
  | false -> f acc path

and _scan_dir ~descend_into f acc parent =
  Array.fold_left (scan_dir ~descend_into f) acc (list_dir parent)

(** Call [f] on every files found recursively into [path]. [descend_into] is
    called on every directories. Directories for which [descend_into] is [false]
    are not iterated. *)
let scan_dir ?(descend_into = fun _ -> true) f acc path =
  scan_dir ~descend_into f acc path

(** Calls [f] on every OCaml implementation files in a directory tree. *)
let find_ml_files f path =
  let is_ml_file fname =
    match Filename.extension fname with
    | ".ml" | ".eliom" | ".mli" | ".eliomi" -> true
    | ext ->
        (* Accept extensions of the form [foo.client.ml] *)
        String.ends_with ~suffix:".ml" ext
  in
  let descend_into path =
    match Filename.basename path with
    | "_build" | "_opam" | ".git" -> false
    | _ -> true
  in
  scan_dir ~descend_into (fun () p -> if is_ml_file p then f p) () path
