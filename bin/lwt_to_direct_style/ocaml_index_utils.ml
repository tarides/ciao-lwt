open Merlin_index_format.Index_format

type t = Lid_set.t Uid_map.t

let of_paths paths =
  List.fold_left
    (fun acc file ->
      match read ~file with
      | Index index ->
          (* Format.printf "@[<2>%s:@ %a@]@\n" file pp index; *)
          Uid_map.union
            (fun _uid locs locs' -> Some (Lid_set.union locs locs'))
            acc index.defs
      | _ -> acc)
    Uid_map.empty paths

let locs_from_comp_unit t unit =
  Uid_map.fold
    (fun uid locs acc ->
      match uid with
      | Item { comp_unit; _ } when comp_unit = unit ->
          Lid_set.fold List.cons locs acc
      | _ -> acc)
    t []
