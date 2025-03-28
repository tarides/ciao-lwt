let migrate_file ~formatted ~errors ~modify_ast file =
  match Ocamlformat_utils.format_in_place ~file ~modify_ast with
  | Ok () -> incr formatted
  | Error (`Msg msg) ->
      Format.eprintf "%s: %s\n%!" file msg;
      incr errors

let main use_lwt_bind paths =
  let errors = ref 0 in
  let formatted = ref 0 in
  let modify_ast = Ast_transforms.remove_lwt_ppx ~use_lwt_bind in
  List.iter
    (Fs_utils.find_ml_files (migrate_file ~formatted ~errors ~modify_ast))
    paths;
  Format.printf "Formatted %d files, %d errors\n%!" !formatted !errors;
  if !errors > 0 then exit 1

open Cmdliner

let opt_use_lwt_bind =
  let doc =
    "Use 'Lwt.bind' instead of 'let*'. This can help migrate modules that \
     already use 'let*' for an other purpose."
  in
  Arg.(value & flag & info ~doc [ "use-lwt-bind" ])

let pos_inputs =
  let doc = "Path to files or directories to migrate." in
  Arg.(non_empty & pos_all file [] & info ~doc ~docv:"PATH" [])

let cmd =
  let doc = "Migrate your codebase from lwt_ppx to plain Lwt." in
  let info = Cmd.info "lwt-ppx-to-let-syntax" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(const main $ opt_use_lwt_bind $ pos_inputs)

let () = exit (Cmd.eval cmd)
