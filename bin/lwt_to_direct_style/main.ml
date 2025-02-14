let main _paths = ()

open Cmdliner

let pos_inputs =
  let doc = "Path to files or directories to migrate." in
  Arg.(non_empty & pos_all file [] & info ~doc ~docv:"PATH" [])

let cmd =
  let doc =
    "Migrate your codebase from Lwt to direct-style concurrency libraries."
  in
  let info = Cmd.info "lwt-to-direct-style" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(const main $ pos_inputs)

let () = exit (Cmd.eval cmd)
