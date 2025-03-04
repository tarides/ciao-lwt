open Ocamlformat_utils.Parsing
open Asttypes
open Parsetree
open Ast_helper
open Ocamlformat_utils.Ast_utils

module Occ = struct
  (** Manage occurrences of Lwt calls that should be migrated. *)

  let _tbl : (Location.t, string) Hashtbl.t ref = ref (Hashtbl.create 0)

  let init lids =
    let new_tbl = Hashtbl.create (List.length lids) in
    List.iter (fun (ident, lid) -> Hashtbl.replace new_tbl lid.loc ident) lids;
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

  (** Calls [f] if [lid] should be rewritten. [f ident] is [Some ast] if a
      rewrite happened, [None] otherwise. [ident] is the basename of the
      longident at [lid]. *)
  let may_rewrite lid f =
    match Hashtbl.find_opt !_tbl lid.loc with
    | Some ident ->
        let r = f ident in
        if Option.is_some r then Hashtbl.remove !_tbl lid.loc;
        r
    | None -> None

  (** Warn about locations that have not been rewritten so far. *)
  let warn_missing_locs () =
    let missing = Hashtbl.length !_tbl in
    if missing > 0 then (
      Format.eprintf "Warning: %d occurrences have not been rewritten.@\n"
        missing;
      Hashtbl.fold (fun loc ident acc -> (loc, ident) :: acc) !_tbl []
      |> List.sort compare (* Sort for a reproducible output. *)
      |> List.iter (fun (loc, ident) ->
             Format.eprintf "  %s %a@\n" ident Printast.fmt_location loc);
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

let mk_cstr c = Some (mk_constr_exp c)

module Unpack_apply : sig
  type t

  val unapply : (arg_label * expression) list -> t -> expression option
  (** Decode a list of arguments matching a sequence of [take] instructions
      ending in a [return]. If the argument list is smaller than expected, a
      function node is generated to respect the arity and allow the expression
      to be rewritten. Return [None] if the argument list couldn't be unapplied.
  *)

  val take : (expression -> t) -> t
  (** Accept one argument. *)

  val return : expression option -> t
  (** Stop accepting arguments. [return (Some exp)] rewrites the apply
      expression to [exp] while [return None] leaves the original expression.
      The "too many arguments" warning is not triggered with [return None]. *)
end = struct
  type t = Arg of (expression -> t) | End of expression option

  let rec apply_remaining_args params i k =
    let ident = "x" ^ string_of_int i in
    let params = mk_function_param (Pat.var (mk_loc ident)) :: params in
    match k (Exp.ident (mk_longident [ ident ])) with
    | Arg k -> apply_remaining_args params (i + 1) k
    | End None -> None
    | End (Some body) ->
        Some (Exp.function_ (List.rev params) None (Pfunction_body body))

  let rec unapply args t =
    match (args, t) with
    | (Nolabel, arg) :: args_tl, Arg k -> unapply args_tl (k arg)
    | [], End (Some _ as r) -> r
    | _, End None -> None
    | [], Arg k -> apply_remaining_args [] 1 k
    | ((Labelled lbl | Optional lbl), _) :: _, Arg _ ->
        Format.eprintf "Error: Unexpected label %S at %a\n%!" lbl.txt
          Printast.fmt_location lbl.loc;
        None
    | (_, arg) :: _, End (Some _) ->
        Format.eprintf "Error: Too many arguments at %a\n%!"
          Printast.fmt_location arg.pexp_loc;
        None

  let take k = Arg k
  let return r = End r
end

let rewrite_apply_lwt ident args =
  let open Unpack_apply in
  unapply args
  @@
  match ident with
  | "bind" ->
      take @@ fun promise_arg ->
      take @@ fun fun_arg ->
      return (rewrite_continuation fun_arg ~arg:promise_arg)
  | "map" ->
      take @@ fun fun_arg ->
      take @@ fun promise_arg ->
      return (rewrite_continuation fun_arg ~arg:promise_arg)
  | "return" -> take @@ fun value_arg -> return (Some value_arg)
  | "try_bind" ->
      take @@ fun thunk ->
      take @@ fun value_f ->
      take @@ fun exn_f -> return (Some (rewrite_try_bind thunk value_f exn_f))
  | "return_some" ->
      take @@ fun value_arg ->
      return (Some (mk_constr_exp ~arg:value_arg "Some"))
  | "return_ok" ->
      take @@ fun value_arg -> return (Some (mk_constr_exp ~arg:value_arg "Ok"))
  | "return_error" ->
      take @@ fun value_arg ->
      return (Some (mk_constr_exp ~arg:value_arg "Error"))
  | "return_unit" -> return (mk_cstr "()")
  | "return_none" -> return (mk_cstr "None")
  | "return_nil" -> return (mk_cstr "[]")
  | "return_true" -> return (mk_cstr "true")
  | "return_false" -> return (mk_cstr "false")
  | _ -> return None

let rewrite_infix_lwt op lhs rhs =
  match op with ">>=" | ">|=" -> rewrite_continuation rhs ~arg:lhs | _ -> None

let rewrite_letop let_ body = function
  | "let*" | "let+" ->
      Some
        (mk_let ~is_pun:let_.pbop_is_pun ?value_constraint:let_.pbop_typ
           let_.pbop_pat ~args:let_.pbop_args let_.pbop_exp body)
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
  | Pexp_apply ({ pexp_desc = Pexp_ident lid; _ }, args) ->
      Occ.may_rewrite lid (fun ident -> rewrite_apply_lwt ident args)
  (* Rewrite the use of a [Lwt] infix operator. *)
  | Pexp_infix (op, lhs, rhs) when Occ.check op ->
      Occ.may_rewrite op (fun op -> rewrite_infix_lwt op lhs rhs)
  (* Rewrite expressions such as [Lwt.return_unit], but also any partially
     applied [Lwt] function. *)
  | Pexp_ident lid ->
      Occ.may_rewrite lid (fun ident -> rewrite_apply_lwt ident [])
  (* Simple [let*]. *)
  | Pexp_letop { let_; ands = []; body; _ } ->
      Occ.may_rewrite let_.pbop_op (rewrite_letop let_ body)
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
  let rec expr m exp =
    match rewrite_expression exp with
    | Some exp -> expr m exp
    | None -> default.expr m exp
  in
  let structure m str =
    default.structure m (List.filter remove_lwt_opens str)
  in
  let m = { default with expr; structure } in
  let str = m.structure m str in
  Occ.warn_missing_locs ();
  str
