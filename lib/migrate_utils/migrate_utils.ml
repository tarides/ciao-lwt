open Ocamlformat_utils.Parsing
open Parsetree

module Loc = struct
  type t = {
    line : int;
    col : int;
    len : int; [@warning "-69"]
        (* Silent unused-field warning. This field is used implicitly by
           polymorphic compare in [Hashtbl]. *)
  }
  (** Remove information from the [Location.t] to avoid problems with different
      filenames due to custom Dune rules and offset positions due to PPXes. *)

  let of_location { Location.loc_start; loc_end; _ } =
    {
      line = loc_start.pos_lnum;
      col = loc_start.pos_cnum - loc_start.pos_bol;
      len = loc_end.pos_cnum - loc_start.pos_cnum;
    }

  let of_location_ocaml { Ocaml_parsing.Location.loc_start; loc_end; _ } =
    {
      line = loc_start.pos_lnum;
      col = loc_start.pos_cnum - loc_start.pos_bol;
      len = loc_end.pos_cnum - loc_start.pos_cnum;
    }

  let pp ppf loc = Format.fprintf ppf "line %d column %d" loc.line (loc.col + 1)
end

type state = {
  occ : (Loc.t, string * string) Hashtbl.t;
  mutable comments : Ocamlformat_utils.Cmt.t list;
  mutable comment_default_loc : Location.t;
}

let add_comment state ?(loc = state.comment_default_loc) text =
  let cmt = " " ^ text ^ " " in
  state.comments <-
    Ocamlformat_utils.Cmt.create_comment cmt loc :: state.comments

let set_default_comment_loc state loc = state.comment_default_loc <- loc

module Occ = struct
  open Location

  (** With OCaml 5.4, Merlin's index contains locations for node of longidents,
      for example for [Lwt.bind], it contains a location for [Lwt] and one for
      [Lwt.bind]. Unfortunately, the location at every node of the longidents
      are not stored in the index (they are equal to [Location.none]) and this
      extra step is needed to detect these. *)
  let filter_sub_locs lids f =
    let locs =
      Array.of_list lids
      |> Array.map (fun (ident, lid) ->
             (ident, Loc.of_location_ocaml lid.Ocaml_parsing.Location.loc))
    in
    Array.sort (fun (_, a) (_, b) -> compare a b) locs;
    let len = Array.length locs in
    (* Iterate on the sorted locations and for consecutive locations with same
       [line] and [col], keep the last one (the one with the bigger [len]). *)
    let rec loop ((ident, loc0) : _ * Loc.t) i =
      if i >= len then f loc0 ident
      else
        let ((_, loc1) as loc1') = locs.(i) in
        if loc0.line <> loc1.line || loc0.col <> loc1.col then f loc0 ident;
        loop loc1' (i + 1)
    in
    if len > 0 then loop locs.(0) 1

  let init lids =
    let new_tbl = Hashtbl.create (List.length lids) in
    filter_sub_locs lids (Hashtbl.replace new_tbl);
    new_tbl

  let remove_loc state loc = Hashtbl.remove state.occ (Loc.of_location loc)
  let remove state lid = remove_loc state lid.loc
  let remove_s = remove

  let pop state lid =
    let loc = Loc.of_location lid.loc in
    if Hashtbl.mem state.occ loc then (
      Hashtbl.remove state.occ loc;
      true)
    else false

  let get state lid = Hashtbl.find_opt state.occ (Loc.of_location lid.loc)

  let may_rewrite' state lid f =
    let loc = Loc.of_location lid.loc in
    match Hashtbl.find_opt state.occ loc with
    | Some ident ->
        let r = f ident in
        if Option.is_some r then Hashtbl.remove state.occ loc;
        r
    | None -> None

  let may_rewrite state lid f =
    let r = may_rewrite' state lid f in
    if Option.is_some r then remove state lid;
    r

  let may_rewrite_s = may_rewrite'

  let pp_occurrences =
    let is_word = function
      | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
      | _ -> false
    in
    let pp_ident ppf = function
      | "" -> ()
      | ident when not (is_word ident.[0]) -> Format.fprintf ppf ".(%s)" ident
      | ident -> Format.fprintf ppf ".%s" ident
    in
    let pp_occurrence ppf (loc, (unit, ident)) =
      Format.fprintf ppf "%s%a (%a)" unit pp_ident ident Loc.pp loc
    in
    fun fmt occurs ->
      (* Sort for a reproducible output. *)
      let occurs = List.sort compare occurs in
      Format.pp_print_list pp_occurrence fmt occurs

  let occurs state =
    Hashtbl.fold (fun loc occ acc -> (loc, occ) :: acc) state.occ []

  (** Warn about locations that have not been rewritten so far. *)
  let warn_missing_locs state fname =
    let missing = Hashtbl.length state.occ in
    if missing > 0 then
      let occurs = occurs state in
      Format.eprintf
        "@[<v 2>Warning: %s: %d occurrences have not been rewritten.@ %a@]@\n"
        fname missing pp_occurrences occurs

  let print_occurrences state fname =
    let occurs = occurs state in
    Format.printf "@[<v 2>%s: (%d occurrences)@ %a@]@\n" fname
      (List.length occurs) pp_occurrences occurs
end

type modify_ast = {
  structure : state -> structure -> structure;
  signature : state -> signature -> signature;
}

let make_state occurrences =
  let occ = Occ.init occurrences in
  { occ; comments = []; comment_default_loc = Location.none }

let make_modify_ast ~modify_ast ~fname occurrences =
  let modify_ast = modify_ast ~fname in
  let rewrite f x =
    let state = make_state occurrences in
    let r = f state x in
    Occ.warn_missing_locs state fname;
    (r, state.comments)
  in
  {
    Ocamlformat_utils.structure = rewrite modify_ast.structure;
    signature = rewrite modify_ast.signature;
  }

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
      let file = lid.Ocaml_parsing.Location.loc.loc_start.pos_fname in
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

let occurrences ~packages ~units =
  let cmts = Ocaml_shape_utils.cmts_of_packages ~packages ~units in
  match Ocaml_index_utils.occurrences ~dune_build_dir ~cmts with
  | [] ->
      Format.eprintf "Found no occurrences.\n%!";
      exit 1
  | occ -> occ

let migrate ~packages ~units ~modify_ast ~errors ~formatted =
  let occurs = occurrences ~packages ~units in
  let filename_map = lookup_filename_map () in
  group_occurrences_by_file occurs (fun fname occurrences ->
      let modify_ast = make_modify_ast ~modify_ast ~fname occurrences in
      migrate_file ~filename_map ~formatted ~errors ~modify_ast fname)

let print_occurrences ~packages ~units =
  let occurs = occurrences ~packages ~units in
  group_occurrences_by_file occurs (fun file occurs ->
      let state = make_state occurs in
      Occ.print_occurrences state file)

let migrate ~packages ~units ~modify_ast =
  let formatted = ref 0 and errors = ref 0 in
  try
    migrate ~packages ~units ~modify_ast ~errors ~formatted;
    Format.printf "Formatted %d files\n%!" !formatted;
    if !errors > 0 then
      Error (`Msg (Format.asprintf "%d errors were generated" !errors))
    else Ok ()
  with Failure msg -> Error (`Msg msg)

let print_occurrences ~packages ~units =
  try
    print_occurrences ~packages ~units;
    Ok ()
  with Failure msg -> Error (`Msg msg)
