open Ocamlformat_utils.Parsing

(** Call [f] for every files that contain occurrences of [Lwt]. *)
let group_occurrences_by_file lids f =
  let module Tbl = Hashtbl.Make (String) in
  let tbl = Tbl.create 64 in
  List.iter
    (fun ((_ident, lid) as occ) ->
      let file = lid.Location.loc.loc_start.pos_fname in
      Tbl.replace tbl file
        (match Tbl.find_opt tbl file with
        | Some lids -> occ :: lids
        | None -> [ occ ]))
    lids;
  Tbl.iter f tbl

let migrate_file ~formatted ~errors ~modify_ast file =
  match Ocamlformat_utils.format_structure_in_place ~file ~modify_ast with
  | Ok () -> incr formatted
  | Error (`Msg msg) ->
      Format.eprintf "%s: %s\n%!" file msg;
      incr errors

let do_migration occurs =
  let errors = ref 0 in
  let formatted = ref 0 in
  let backend = Concurrency_backend.eio in
  group_occurrences_by_file occurs (fun file occurrences ->
      let modify_ast = Ast_rewrite.rewrite_lwt_uses ~occurrences ~backend in
      migrate_file ~formatted ~errors ~modify_ast file);
  Format.printf "Formatted %d files, %d errors\n%!" !formatted !errors;
  if !errors > 0 then exit 1

let print_occurrences occurs =
  let pp_occurrence ppf (ident, lid) =
    Format.fprintf ppf "%S %a" ident Printast.fmt_location lid.Location.loc
  in
  group_occurrences_by_file occurs (fun file occurrences ->
      Format.printf "@[<v 2>%s: (%d occurrences)@ %a@]@\n" file
        (List.length occurrences)
        (Format.pp_print_list pp_occurrence)
        occurrences)

let main migrate =
  let all_occurrences =
    Ocaml_index_utils.occurrences ~dune_build_dir:"_build" ~package:"lwt"
      ~unit:"Lwt"
  in
  if migrate then do_migration all_occurrences
  else print_occurrences all_occurrences

open Cmdliner

let opt_migrate =
  let doc = "Modify the source code instead of printing occurrences of Lwt." in
  Arg.(value & flag & info ~doc [ "migrate" ])

let cmd =
  let doc =
    "Migrate your codebase from Lwt to direct-style concurrency libraries."
  in
  let info = Cmd.info "lwt-to-direct-style" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(const main $ opt_migrate)

let () = exit (Cmd.eval cmd)
