open Ocamlformat_utils.Parsing
open Asttypes
open Parsetree

(* open Ast_helper *)
open Ocamlformat_utils.Ast_utils

module Occ = struct
  (** Manage occurrences of Lwt calls that should be migrated. *)

  let _tbl : (Location.t, Longident.t) Hashtbl.t ref = ref (Hashtbl.create 0)

  let init lids =
    let new_tbl = Hashtbl.create (List.length lids) in
    List.iter (fun lid -> Hashtbl.replace new_tbl lid.loc lid.txt) lids;
    _tbl := new_tbl

  (** Whether the given longident is an occurrence of an Lwt function. This will
      change the internal table and any subsequent calls for the same longident
      will return [false]. *)
  let pop lid =
    if Hashtbl.mem !_tbl lid.loc then (
      Hashtbl.remove !_tbl lid.loc;
      true)
    else false

  let check lid = Hashtbl.mem !_tbl lid.loc
  let remove lid = Hashtbl.remove !_tbl lid.loc

  (** Warn about locations that have not been rewritten so far. *)
  let warn_missing_locs () =
    let missing = Hashtbl.length !_tbl in
    if missing > 0 then (
      Format.eprintf "Warning: %d occurrences have not been rewritten.@\n"
        missing;
      Hashtbl.iter
        (fun loc lident ->
          Format.eprintf "  %a %a@\n" Printast.fmt_longident lident
            Printast.fmt_location loc)
        !_tbl;
      Format.eprintf "%!")
end

(** Whether an expression is a [fun] with one argument that can safely be
    translated into a [let] binding. Returns [None] if that's not the case. *)
let is_fun_with_one_argument = function
  | {
      pexp_desc =
        Pexp_function
          ( [ { pparam_desc = Pparam_val (Nolabel, None, arg_pat); _ } ],
            None,
            Pfunction_body body );
      pexp_attributes = [];
      _;
    } ->
      Some (arg_pat, body)
  | _ -> None

let rewrite_apply_lwt lid args =
  match (Longident.flatten lid.txt, args) with
  (* [Lwt.bind $promise_arg (fun $fun_arg_pat -> $body)] *)
  | [ "Lwt"; "bind" ], [ (Nolabel, promise_arg); (Nolabel, fun_arg) ] -> (
      match is_fun_with_one_argument fun_arg with
      | Some (fun_arg_pat, body) -> Some (mk_let fun_arg_pat promise_arg body)
      | None -> None)
  (* [Lwt.return $value_arg] *)
  | [ "Lwt"; "return" ], [ (Nolabel, value_arg) ] -> Some value_arg
  | _ -> None

let rewrite_infix_lwt op lhs rhs =
  match op.txt with
  | ">>=" | ">|=" -> (
      match is_fun_with_one_argument rhs with
      | Some (arg_pat, rhs_body) -> Some (mk_let arg_pat lhs rhs_body)
      | None -> None)
  | _ -> None

let rewrite_expression exp =
  match exp.pexp_desc with
  (* Rewrite a call to a [Lwt] function. *)
  | Pexp_apply ({ pexp_desc = Pexp_ident lid; _ }, args) when Occ.check lid ->
      let r = rewrite_apply_lwt lid args in
      if Option.is_some r then Occ.remove lid;
      r
  (* Rewrite the use of a [Lwt] infix operator. *)
  | Pexp_infix (op, lhs, rhs) when Occ.check op ->
      let r = rewrite_infix_lwt op lhs rhs in
      if Option.is_some r then Occ.remove op;
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
