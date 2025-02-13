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
