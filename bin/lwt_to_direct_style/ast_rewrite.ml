open Ocamlformat_utils.Parsing
open Asttypes
open Parsetree

(* open Ast_helper *)
open Ocamlformat_utils.Ast_utils

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

  let check lid = Hashtbl.mem !_tbl lid
  let remove lid = Hashtbl.remove !_tbl lid

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

let rewrite_apply_lwt lid args =
  match (Longident.flatten lid.txt, args) with
  (* [Lwt.bind $promise_arg (fun $fun_arg_pat -> $body)] *)
  | ( [ "Lwt"; "bind" ],
      [
        (Nolabel, promise_arg);
        ( Nolabel,
          {
            pexp_desc =
              Pexp_function
                ( [
                    { pparam_desc = Pparam_val (Nolabel, None, fun_arg_pat); _ };
                  ],
                  None,
                  Pfunction_body body );
            _;
          } );
      ] ) ->
      Some (mk_let fun_arg_pat promise_arg body)
  (* [Lwt.return $value_arg] *)
  | [ "Lwt"; "return" ], [ (Nolabel, value_arg) ] -> Some value_arg
  | _ -> None

let rewrite_expression exp =
  match exp.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident lid; _ }, args) when Occ.check lid ->
      let r = rewrite_apply_lwt lid args in
      if Option.is_some r then Occ.remove lid;
      r
  | _ -> None

let rewrite_lwt_uses ~occurrences str =
  Occ.init occurrences;
  let default = Ast_mapper.default_mapper in
  let expr m exp =
    default.expr m (Option.value (rewrite_expression exp) ~default:exp)
  in
  let m = { default with expr } in
  let str = m.structure m str in
  Occ.warn_missing_locs ();
  str
