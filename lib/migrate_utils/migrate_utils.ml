open Ocamlformat_utils.Parsing

type occurrences = ((string * string) * Longident.t Location.loc) list

let errorf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

(** Map from file names to their path. This is used when locations point to the
    wrong path (eg. it only contains the basename), which is often the case with
    [.eliom] files. *)
let lookup_filename_map () =
  let tbl = Hashtbl.create 64 in
  Fs_utils.find_ml_files
    (fun path -> Hashtbl.add tbl (Filename.basename path) path)
    ".";
  tbl

let resolve_file_name ~filename_map path =
  if Sys.file_exists path then Ok path
  else
    match Hashtbl.find_all filename_map (Filename.basename path) with
    | [] -> errorf "Couldn't find file %S" path
    | _ :: _ :: _ -> errorf "Ambiguous location in index for file %S" path
    | [ f ] -> Ok f

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

let migrate_file ~filename_map ~formatted ~errors ~modify_ast file =
  let ( >>= ) = Result.bind in
  match
    resolve_file_name ~filename_map file >>= fun file ->
    Ocamlformat_utils.format_in_place ~file ~modify_ast
  with
  | Ok () -> incr formatted
  | Error (`Msg msg) ->
      Format.eprintf "%s: %s\n%!" file msg;
      incr errors

let dune_build_dir = "_build"

let migrate ~packages ~units ~modify_ast =
  let occurs = Ocaml_index_utils.occurrences ~dune_build_dir ~packages ~units in
  let errors = ref 0 in
  let formatted = ref 0 in
  let filename_map = lookup_filename_map () in
  group_occurrences_by_file occurs (fun fname occurrences ->
      let modify_ast = modify_ast ~fname occurrences in
      migrate_file ~filename_map ~formatted ~errors ~modify_ast fname);
  Format.printf "Formatted %d files, %d errors\n%!" !formatted !errors;
  if !errors > 0 then exit 1

let print_occurrences ~packages ~units =
  let occurs = Ocaml_index_utils.occurrences ~dune_build_dir ~packages ~units in
  let pp_occurrence ppf ((unit_name, ident), lid) =
    Format.fprintf ppf "%s.%s %a" unit_name ident Printast.fmt_location
      lid.Location.loc
  in
  group_occurrences_by_file occurs (fun file occurrences ->
      Format.printf "@[<v 2>%s: (%d occurrences)@ %a@]@\n" file
        (List.length occurrences)
        (Format.pp_print_list pp_occurrence)
        occurrences)
