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
