open Ocamlformat_utils.Parsing

module Occ = struct
  (** Manage occurrences of Lwt calls that should be migrated. *)

  type lid = Longident.t Location.loc

  let _tbl : (lid, unit) Hashtbl.t ref = ref (Hashtbl.create 0)

  let init lids =
    let new_tbl = Hashtbl.create (List.length lids) in
    List.iter (fun lid -> Hashtbl.replace new_tbl lid ()) lids;
    _tbl := new_tbl

  (** Whether the given longident is an occurrence of an Lwt function. This will
      change the internal table and any subsequent calls for the same longident
      will return [false]. *)
  let pop lid =
    if Hashtbl.mem !_tbl lid then (
      Hashtbl.remove !_tbl lid;
      true)
    else false

  (** Warn about locations that have not been rewritten so far. *)
  let warn_missing_locs () =
    let missing = Hashtbl.length !_tbl in
    if missing > 0 then (
      Format.eprintf "Warning: %d occurrences have not been rewritten.@\n"
        missing;
      Hashtbl.iter
        (fun lid () -> Format.eprintf "  %a@\n" Printast.fmt_longident_loc lid)
        !_tbl;
      Format.eprintf "%!")
end

let rewrite_lwt_uses ~occurrences str =
  Occ.init occurrences;
  Occ.warn_missing_locs ();
  str
