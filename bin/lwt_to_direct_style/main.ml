let modify_ast ~fname =
  Ast_rewrite.rewrite_lwt_uses ~fname ~backend:Concurrency_backend.eio

let main migrate =
  let units = function
    | "Lwt" -> true
    | unit -> String.starts_with ~prefix:"Lwt_" unit
  in
  let packages = [ "lwt"; "lwt.unix" ] in
  if migrate then Migrate_utils.migrate ~packages ~units ~modify_ast
  else Migrate_utils.print_occurrences ~packages ~units

open Cmdliner

let opt_migrate =
  let doc = "Modify the source code instead of printing occurrences of Lwt." in
  Arg.(value & flag & info ~doc [ "migrate" ])

let cmd =
  let doc =
    "Migrate your codebase from Lwt to direct-style concurrency libraries."
  in
  let info = Cmd.info "lwt-to-direct-style" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(term_result (const main $ opt_migrate))

let () = exit (Cmd.eval cmd)
