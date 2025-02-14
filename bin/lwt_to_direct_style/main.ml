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

module Ocaml_to_ocamlformat = struct
  open Ocamlformat_utils.Parsing

  (** Convert from OCaml [Parsing] values to Ocamlformat types. *)

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

let find_lwt_occurrences () =
  let open Ocaml_index_utils in
  let index_files = scan_dune_build_path () in
  let index = of_paths index_files in
  let lwt_occurrences = locs_from_comp_unit index "Lwt" in
  List.map Ocaml_to_ocamlformat.lid lwt_occurrences

let main _paths =
  try
    let lwt_occurrences = find_lwt_occurrences () in
    List.iter
      (Format.printf "%a@\n"
         Ocamlformat_utils.Parsing.Printast.fmt_longident_loc)
      lwt_occurrences
  with Failure msg ->
    Format.eprintf "Error: %s\n%!" msg;
    exit 1

open Cmdliner

let pos_inputs =
  let doc = "Path to files or directories to migrate." in
  Arg.(non_empty & pos_all file [] & info ~doc ~docv:"PATH" [])

let cmd =
  let doc =
    "Migrate your codebase from Lwt to direct-style concurrency libraries."
  in
  let info = Cmd.info "lwt-to-direct-style" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(const main $ pos_inputs)

let () = exit (Cmd.eval cmd)
