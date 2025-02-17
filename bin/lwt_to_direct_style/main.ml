open Ocamlformat_utils.Parsing

let scan_dune_build_path () =
  if not (Sys.is_directory "_build") then
    failwith "Directory '_build' not found."
  else
    let collect_index_files acc path =
      if Filename.extension path = ".ocaml-index" then path :: acc else acc
    in
    match Fs_utils.scan_dir collect_index_files [] "_build" with
    | [] -> failwith "No index found. Please run 'dune build @ocaml-index'."
    | p -> p

(** Convert from OCaml [Parsing] values that Merlin uses to the corresponding
    Ocamlformat type. *)
module Ocaml_to_ocamlformat = struct
  let location_t { Ocaml_parsing.Location.loc_start; loc_end; loc_ghost } =
    { Location.loc_start; loc_end; loc_ghost }

  let location_loc f { Ocaml_parsing.Location.txt; loc } =
    { Location.txt = f txt; loc = location_t loc }

  let rec longident =
    let open Longident in
    function
    | Ocaml_parsing.Longident.Lident s -> Lident s
    | Ldot (a, b) -> Ldot (longident a, b)
    | Lapply (a, b) -> Lapply (longident a, longident b)

  let lid = location_loc longident
end

(** Lookup every occurrences of [Lwt] in every [.ocaml-index] files in the
    [_build] directory. *)
let find_lwt_occurrences () =
  let open Ocaml_index_utils in
  let index_files = scan_dune_build_path () in
  let index = of_paths index_files in
  let lwt_occurrences = locs_from_comp_unit index "Lwt" in
  List.map Ocaml_to_ocamlformat.lid lwt_occurrences

(** Call [f] for every files that contain occurrences of [Lwt]. *)
let group_occurrences_by_file lids f =
  let module Tbl = Hashtbl.Make (String) in
  let tbl = Tbl.create 64 in
  List.iter
    (fun lid ->
      let file = lid.Location.loc.loc_start.pos_fname in
      Tbl.replace tbl file
        (match Tbl.find_opt tbl file with
        | Some lids -> lid :: lids
        | None -> [ lid ]))
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
  group_occurrences_by_file occurs (fun file occurrences ->
      let modify_ast = Ast_rewrite.rewrite_lwt_uses ~occurrences in
      migrate_file ~formatted ~errors ~modify_ast file);
  Format.printf "Formatted %d files, %d errors\n%!" !formatted !errors;
  if !errors > 0 then exit 1

let print_occurrences occurs =
  group_occurrences_by_file occurs (fun file occurrences ->
      Format.printf "@[<v 2>%s: (%d occurrences)@,%a@]@\n" file
        (List.length occurrences)
        (Format.pp_print_list
           Ocamlformat_utils.Parsing.Printast.fmt_longident_loc)
        occurrences)

let main migrate =
  let all_occurrences = find_lwt_occurrences () in
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
