open Lwt_ppx_to_let_syntax

let modify_ast = Ast_transforms.remove_lwt_ppx

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
        with exn ->
          Format.eprintf "%s: %a\n%!" file pp_format_exn exn;
          incr errors))
    ".";
  Format.printf "Formatted %d files, %d errors\n%!" !i !errors;
  if !errors > 0 then exit 1

open Cmdliner

let cmd =
  let doc = "Convert your codebase from Lwt to Eio" in
  let info = Cmd.info "lwt-ppx-to-let-syntax" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(const main $ const ())

let () = exit (Cmd.eval cmd)
