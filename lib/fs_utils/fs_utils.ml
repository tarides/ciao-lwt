let rec scan_dir ~descend_into f acc path =
  match Sys.is_directory path with
  | exception Sys_error msg ->
      Printf.eprintf "%s\n%!" msg;
      acc
  | true ->
      if descend_into path then _scan_dir ~descend_into f acc path else acc
  | false -> f acc path

and _scan_dir ~descend_into f acc parent =
  match Sys.readdir parent with
  | exception Sys_error msg ->
      Printf.eprintf "%s\n%!" msg;
      acc
  | files ->
      Array.sort String.compare files;
      Array.fold_left
        (fun acc child ->
          scan_dir ~descend_into f acc (Filename.concat parent child))
        acc files

(** Call [f] on every files found recursively into [path]. [descend_into] is
    called on every directories. Directories for which [descend_into] is [false]
    are not iterated. *)
let scan_dir ?(descend_into = fun _ -> true) f acc path =
  scan_dir ~descend_into f acc path

(** Calls [f] on every OCaml implementation files in a directory tree. *)
let find_ml_files f path =
  let is_ml_file fname =
    match Filename.extension fname with
    | ".ml" | ".eliom" -> true
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
