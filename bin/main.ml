open Lwt_to_eio

let modify_ast ast = ast
let is_ml_file fname = Filename.extension fname = ".ml"

let main () =
  let errors = ref 0 in
  let i = ref 0 in
  Fs_utils.scan_dir
    ~descend_into:(fun path ->
      match Filename.basename path with "_build" | ".git" -> false | _ -> true)
    (fun file ->
      if is_ml_file file then (
        try
          Ocamlformat_utils.format_structure_in_place ~file ~modify_ast;
          incr i
        with Failure msg | Sys_error msg ->
          Printf.eprintf "%s: %s\n%!" file msg;
          incr errors))
    ".";
  Printf.printf "Formatted %d files, %d errors\n%!" !i !errors;
  if !errors > 0 then exit 1

open Cmdliner

let cmd =
  let doc = "Convert your codebase from Lwt to Eio" in
  let info = Cmd.info "lwt-to-eio" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(const main $ const ())

let () = exit (Cmd.eval cmd)
