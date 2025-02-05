open Cmdliner

let main () = print_endline "Hello world"

let cmd =
  let doc = "Convert your codebase from Lwt to Eio" in
  let info = Cmd.info "lwt-to-eio" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(const main $ const ())

let () = exit (Cmd.eval cmd)
