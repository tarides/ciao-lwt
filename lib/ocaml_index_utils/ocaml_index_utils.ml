open Ocaml_parsing
open Ocaml_typing
open Merlin_index_format.Index_format

type t = Lid_set.t Uid_map.t

let fail fmt = Printf.ksprintf failwith fmt

(* Find all the [.ocaml-index] files. *)
let scan_index_files ~dune_build_dir =
  match Sys.is_directory dune_build_dir with
  | (exception Sys_error _) | false ->
      fail "Directory %S not found." dune_build_dir
  | true -> (
      let collect_index_files acc path =
        if Filename.extension path = ".ocaml-index" then path :: acc else acc
      in
      match Fs_utils.scan_dir collect_index_files [] dune_build_dir with
      | [] -> failwith "No index found. Please run 'dune build @ocaml-index'."
      | p -> p)

let scan_dune_build_dir ~dune_build_dir =
  let open Merlin_index_format.Index_format in
  let paths = scan_index_files ~dune_build_dir in
  if paths = [] then failwith "Found no index files in '_build'.";
  List.fold_left
    (fun acc file ->
      match read ~file with
      | Index index ->
          Uid_map.fold (fun uid locs acc -> add acc uid locs) index.defs acc
      | _ -> acc)
    (Uid_map.empty ()) paths

let lookup_occurrences t uid =
  try List.map Lid.to_lid (Lid_set.elements (Uid_map.find uid t))
  with Not_found -> []

let pp_lids =
  let pp_lid ppf lid =
    let lid = Lid.to_lid lid in
    let l = lid.Location.loc.loc_start in
    Format.fprintf ppf "%s:%d" l.pos_fname l.pos_lnum
  in
  let pp_sep ppf () = Format.fprintf ppf ",@ " in
  Format.pp_print_list ~pp_sep pp_lid

let pp ppf t =
  Uid_map.iter
    (fun uid lids ->
      Format.fprintf ppf "@[<hov 2>%a (%d):@ %a@]@\n" Shape.Uid.print uid
        (Lid_set.cardinal lids) pp_lids (Lid_set.elements lids))
    t

type occurrences = ((string * string) * Longident.t Location.loc) list

let uid_of_unit_name u =
  let open Ocaml_typing in
  Shape.Uid.of_compilation_unit_id (Ident.create_persistent u)

(** For a UID, lookup the corresponding unit and identifier. *)
let lookup_ident ~cmts =
  let open Ocaml_typing in
  let module Tbl = Shape.Uid.Tbl in
  let tbl = Tbl.create 256 in
  List.iter
    (fun cmt ->
      let unit_name = cmt.Ocaml_shape_utils.unit_name in
      Tbl.replace tbl (uid_of_unit_name unit_name) (`Found (unit_name, ""));
      List.iter
        (fun (uid, ident, _decl) ->
          let res =
            match ident with
            | Some ident -> `Found (unit_name, Ident.name ident)
            | None -> `Ignore
          in
          Tbl.replace tbl uid res)
        cmt.Ocaml_shape_utils.decls)
    cmts;
  fun uid ->
    match Tbl.find_opt tbl uid with
    | Some ((`Found _ | `Ignore) as r) -> r
    | None -> `Not_found

let pp_lid_without_location ppf lid =
  let { Location.txt; _ } = Lid.to_lid lid in
  Pprintast.longident ppf txt

(* Read the index files in [paths] and extract occurrences informations for
   declarations that match [lookup_ident]. *)
let extract_occurrences_of_unit ~lookup_ident paths =
  let open Merlin_index_format.Index_format in
  let is_unit_we_care_about u =
    match lookup_ident (uid_of_unit_name u) with `Found _ -> true | _ -> false
  in
  if paths = [] then failwith "Found no index files in '_build'.";
  List.fold_left
    (fun acc file ->
      match read ~file with
      | Index index ->
          Uid_map.fold
            (fun uid locs acc ->
              match lookup_ident uid with
              | `Found ident ->
                  Lid_set.elements locs
                  |> List.fold_left
                       (fun acc lid -> (ident, Lid.to_lid lid) :: acc)
                       acc
              | `Ignore -> (* Not a value *) acc
              | `Not_found -> (
                  match uid with
                  | Item it when is_unit_we_care_about it.comp_unit ->
                      Format.eprintf
                        "@[<hov 2>Warning:@ No ident for uid %s.%d@ (%d \
                         occurrences):@ @[<hov>%a@]@]@\n\
                         %!"
                        it.comp_unit it.id (Lid_set.cardinal locs)
                        Format.(
                          pp_print_list ~pp_sep:pp_print_space
                            pp_lid_without_location)
                        (Lid_set.elements locs);
                      acc
                  | _ -> acc))
            index.defs acc
      | _ -> acc)
    [] paths

let occurrences ~dune_build_dir ~cmts =
  let paths = scan_index_files ~dune_build_dir in
  let lookup_ident = lookup_ident ~cmts in
  extract_occurrences_of_unit ~lookup_ident paths
