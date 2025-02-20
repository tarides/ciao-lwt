let is_ml_file fname =
  match Filename.extension fname with
  | ".ml" | ".eliom" -> true
  | ext ->
      (* Accept extensions of the form [foo.client.ml] *)
      String.ends_with ~suffix:".ml" ext

let pp_format_exn ppf = function
  | Failure msg | Sys_error msg -> Format.fprintf ppf "%s" msg
  | Ocamlformat_utils.Syntax_error loc ->
      Format.fprintf ppf "Syntax error at %a"
        Ocamlformat_utils.Parsing.Location.print_loc loc
  | exn -> Format.fprintf ppf "Unhandled exception: %s" (Printexc.to_string exn)

let migrate_file ~formatted ~errors ~modify_ast file =
  if is_ml_file file then (
    try
      Ocamlformat_utils.format_structure_in_place ~file ~modify_ast;
      incr formatted
    with exn ->
      Format.eprintf "%s: %a\n%!" file pp_format_exn exn;
      incr errors)

let main use_lwt_bind paths =
  let errors = ref 0 in
  let formatted = ref 0 in
  let modify_ast = Ast_transforms.remove_lwt_ppx ~use_lwt_bind in
  let descend_into path =
    match Filename.basename path with
    | "_build" | "_opam" | ".git" -> false
    | _ -> true
  in
  List.iter
    (Fs_utils.scan_dir ~descend_into
       (migrate_file ~formatted ~errors ~modify_ast))
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
