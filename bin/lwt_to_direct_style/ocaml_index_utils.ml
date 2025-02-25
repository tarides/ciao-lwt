let fail fmt = Printf.ksprintf failwith fmt

(** Convert from OCaml [Parsing] values that Merlin uses to the corresponding
    Ocamlformat type. *)
module Ocaml_to_ocamlformat = struct
open Ocamlformat_utils.Parsing

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

(* Find all the [.ocaml-index] files. *)
let scan_dune_build_path ~dune_build_dir =
  if not (Sys.is_directory dune_build_dir) then
    fail "Directory %S not found." dune_build_dir
  else
    let collect_index_files acc path =
      if Filename.extension path = ".ocaml-index" then path :: acc else acc
    in
    match Fs_utils.scan_dir collect_index_files [] dune_build_dir with
    | [] -> failwith "No index found. Please run 'dune build @ocaml-index'."
    | p -> p

(* Lookup the uid_map for a unit. *)
let uid_map_of_unit ~package ~unit =
  let open Ocaml_typing in
  let find_cmt ~lib_path =
    match
      List.find_map
        (fun fname ->
          let p = Filename.concat lib_path fname in
          if Sys.file_exists p then Some p else None)
        [ unit ^ ".cmt"; String.uncapitalize_ascii unit ^ ".cmt" ]
    with
    | Some p -> p
    | None -> fail "Couldn't find unit %S in path %S." unit lib_path
  in
  let cmd = "ocamlfind query " ^ package in
  let ic = Unix.open_process_in cmd in
  let lib_path = String.trim (In_channel.input_all ic) in
  match Unix.close_process_in ic with
  | WEXITED 0 -> (
      let cmt = Cmt_format.read_cmt (find_cmt ~lib_path) in
      fun uid ->
        match Shape.Uid.Tbl.find_opt cmt.cmt_uid_to_decl uid with
        | Some (Typedtree.Value { val_id = ident; _ })
        | Some
            (Value_binding
               { vb_pat = { pat_desc = Tpat_var (ident, _, _); _ }; _ }) ->
            `Found (Ident.name ident)
        | Some _ -> `Ignore
        | None -> `Not_found)
  | _ -> fail "Command %S failed." cmd

let pp_ocaml_lid ppf { Ocaml_parsing.Location.txt; loc } =
  let open Ocaml_parsing in
  Format.fprintf ppf "@[<hv 2>%a:@ %a@]" Pprintast.longident txt
    Location.print_loc loc

(* Read the index files and extract all occurrences of [unit]. *)
let extract_occurrences_of_unit ~unit ~lookup_ident paths =
  let open Merlin_index_format.Index_format in
  List.fold_left
    (fun acc file ->
      match read ~file with
      | Index index ->
          Uid_map.fold
            (fun uid locs acc ->
              match lookup_ident uid with
              | `Found ident ->
                  Lid_set.fold
                    (fun loc acc ->
                      (ident, Ocaml_to_ocamlformat.lid loc) :: acc)
                    locs acc
              | `Ignore -> (* Not a value *) acc
              | `Not_found -> (
                  match uid with
                  | Item { comp_unit; id } when comp_unit = unit ->
                      Format.eprintf "@[<v 2>No ident for uid %s.%d:@ %a@]@\n%!"
                        comp_unit id
                        (Format.pp_print_list pp_ocaml_lid)
                        (Lid_set.elements locs);
                      acc
                  | _ -> acc))
            index.defs acc
      | _ -> acc)
    [] paths

let occurrences ~dune_build_dir ~package ~unit =
  let paths = scan_dune_build_path ~dune_build_dir in
  let lookup_ident =
    uid_map_of_unit ~package ~unit
  in
  extract_occurrences_of_unit ~unit ~lookup_ident paths
