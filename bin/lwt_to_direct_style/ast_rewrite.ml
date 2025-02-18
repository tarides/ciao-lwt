open Ocamlformat_utils.Parsing
open Asttypes
open Parsetree
open Ast_helper
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
      Hashtbl.fold (fun loc lident acc -> (loc, lident) :: acc) !_tbl []
      |> List.sort compare (* Sort for a reproducible output. *)
      |> List.iter (fun (loc, lident) ->
             Format.eprintf "  %a %a@\n" Printast.fmt_longident lident
               Printast.fmt_location loc);
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

(* Rewrite to a let binding or an apply. *)
let rewrite_continuation cont ~arg:cont_arg =
  match is_fun_with_one_argument cont with
  | Some (fun_arg_pat, body) -> Some (mk_let fun_arg_pat cont_arg body)
  | None -> Some (Exp.apply cont [ (Nolabel, cont_arg) ])

let rewrite_exp_into_cases = function
  | {
      pexp_desc = Pexp_function ([], None, Pfunction_cases (cases, _, []));
      pexp_attributes = [];
      _;
    } ->
      cases
  | {
      pexp_desc =
        Pexp_function
          ( [ { pparam_desc = Pparam_val (Nolabel, None, arg_pat); _ } ],
            None,
            Pfunction_body body );
      pexp_attributes = [];
      _;
    } ->
      [ Exp.case arg_pat body ]
  | exp ->
      [
        Exp.case
          (Pat.var (mk_loc "v"))
          (Exp.apply exp [ (Nolabel, mk_exp_var "v") ]);
      ]

let rewrite_try_bind thunk value_f exn_f =
  let body =
    match thunk with
    | {
     pexp_desc = Pexp_function ([ _ ], None, Pfunction_body body);
     pexp_attributes = [];
     _;
    } ->
        body
    | _ -> Exp.apply thunk [ (Nolabel, mk_unit_val) ]
  and value_cases = rewrite_exp_into_cases value_f
  and exn_cases =
    rewrite_exp_into_cases exn_f
    |> List.filter_map (fun case ->
           match case.pc_rhs.pexp_desc with
           (* Drop cases doing just a [Lwt.reraise]. *)
           | Pexp_apply
               ({ pexp_desc = Pexp_ident lid; pexp_attributes = []; _ }, [ _ ])
             when same_longident lid.txt [ "Lwt"; "reraise" ] ->
               None
           | _ -> Some { case with pc_lhs = Pat.exception_ case.pc_lhs })
  in
  Exp.match_ body (value_cases @ exn_cases)

let rewrite_apply_lwt lid args =
  match (Longident.flatten lid.txt, args) with
  | [ "Lwt"; "bind" ], [ (Nolabel, promise_arg); (Nolabel, fun_arg) ]
  | [ "Lwt"; "map" ], [ (Nolabel, fun_arg); (Nolabel, promise_arg) ] ->
      rewrite_continuation fun_arg ~arg:promise_arg
  | [ "Lwt"; "return" ], [ (Nolabel, value_arg) ] -> Some value_arg
  | ( [ "Lwt"; "try_bind" ],
      [ (Nolabel, thunk); (Nolabel, value_f); (Nolabel, exn_f) ] ) ->
      Some (rewrite_try_bind thunk value_f exn_f)
  | [ "Lwt"; "return_some" ], [ (Nolabel, value_arg) ] ->
      Some (mk_constr_exp ~arg:value_arg "Some")
  | [ "Lwt"; "return_ok" ], [ (Nolabel, value_arg) ] ->
      Some (mk_constr_exp ~arg:value_arg "Ok")
  | [ "Lwt"; "return_error" ], [ (Nolabel, value_arg) ] ->
      Some (mk_constr_exp ~arg:value_arg "Error")
  | _ -> None

let rewrite_infix_lwt op lhs rhs =
  match op.txt with
  | ">>=" | ">|=" -> rewrite_continuation rhs ~arg:lhs
  | _ -> None

let rewrite_ident_lwt lid =
  let cstr c = Some (mk_constr_exp c) in
  match Longident.last lid.txt with
  | "return_unit" -> cstr "()"
  | "return_none" -> cstr "None"
  | "return_nil" -> cstr "[]"
  | "return_true" -> cstr "true"
  | "return_false" -> cstr "false"
  | _ -> None

(** Flatten pipelines before applying rewrites. *)
let rec flatten_apply exp =
  let flatten callee arg =
    let callee, prefix_args =
      match (flatten_apply callee).pexp_desc with
      | Pexp_apply (callee, prefix_args) -> (callee, prefix_args)
      | _ -> (callee, [])
    in
    Exp.apply callee (prefix_args @ [ (Nolabel, arg) ])
  in
  match exp.pexp_desc with
  | Pexp_infix ({ txt = "@@"; _ }, lhs, rhs) -> flatten lhs rhs
  | Pexp_infix ({ txt = "|>"; _ }, lhs, rhs) -> flatten rhs lhs
  | _ -> exp

let rewrite_expression exp =
  match (flatten_apply exp).pexp_desc with
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
  (* Rewrite expressions such as [Lwt.return_unit]. *)
  | Pexp_ident lid when Occ.check lid ->
      let r = rewrite_ident_lwt lid in
      if Option.is_some r then Occ.remove lid;
      r
  | _ -> None

let remove_lwt_opens stri =
  match stri.pstr_desc with
  | Pstr_open { popen_expr = { pmod_desc = Pmod_ident lid; _ }; _ }
    when Occ.pop lid ->
      false
  | _ -> true

let rewrite_lwt_uses ~occurrences str =
  Occ.init occurrences;
  let default = Ast_mapper.default_mapper in
  let expr m exp =
    default.expr m (Option.value (rewrite_expression exp) ~default:exp)
  in
  let structure m str =
    default.structure m (List.filter remove_lwt_opens str)
  in
  let m = { default with expr; structure } in
  let str = m.structure m str in
  Occ.warn_missing_locs ();
  str
