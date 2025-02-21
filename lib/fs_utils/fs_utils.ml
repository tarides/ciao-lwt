let rec scan_dir ~descend_into f path =
  match Sys.is_directory path with
  | exception Sys_error msg -> Printf.eprintf "%s\n%!" msg
  | true -> if descend_into path then _scan_dir ~descend_into f path
  | false -> f path

and _scan_dir ~descend_into f parent =
  match Sys.readdir parent with
  | exception Sys_error msg -> Printf.eprintf "%s\n%!" msg
  | files ->
      Array.sort String.compare files;
      Array.iter
        (fun child -> scan_dir ~descend_into f (Filename.concat parent child))
        files

(** Call [f] on every files found recursively into [path]. [descend_into] is
    called on every directories. Directories for which [descend_into] is [false]
    are not iterated. *)
let scan_dir ?(descend_into = fun _ -> true) f path =
  scan_dir ~descend_into f path

(** Like [Out_channel.with_open_bin] but write to a temporary file and
    atomically move it to the destination. If [file] already exists, its
    permissions are used for the new file unless [perms] is passed. *)
let with_output_to_file ?mode ?perms file f =
  let perms =
    match perms with
    | Some p -> p
    | None -> (
        try (Unix.stat file).Unix.st_perm with Unix.Unix_error _ -> 0o644)
  in
  let temp_file, oc =
    Filename.open_temp_file ?mode ~perms "ocaml" (Filename.basename file)
  in
  Fun.protect ~finally:(fun () -> Out_channel.close_noerr oc) (fun () -> f oc);
  Sys.rename temp_file file
