open Ocamlformat_utils.Parsing
open Asttypes
open Parsetree
open Ast_helper

let mk_loc ?(loc = !default_loc) txt = { Location.txt; loc }

let mk_function_param ?(loc = !default_loc) ?(lbl = Nolabel) ?def pat =
  { pparam_loc = loc; pparam_desc = Pparam_val (lbl, def, pat) }

let mk_let ?(loc_in = !default_loc) ?(rec_ = Nonrecursive) pat ?(args = []) lhs
    rhs =
  let binding = Vb.mk ~is_pun:false pat args (Pfunction_body lhs) in
  let bindings = { pvbs_bindings = [ binding ]; pvbs_rec = rec_ } in
  Exp.let_ ~loc_in bindings rhs

let mk_function_cases ?(loc = !default_loc) ?(attrs = []) cases =
  Pfunction_cases (cases, loc, attrs)

let mk_longident = function
  | [] -> assert false
  | hd :: tl ->
      let open Longident in
      mk_loc (List.fold_left (fun acc seg -> Ldot (acc, seg)) (Lident hd) tl)

let mk_exp_var s = Exp.ident (mk_longident [ s ])
let mk_unit_ident = mk_longident [ "()" ]
let mk_unit_arg = mk_function_param (Pat.construct mk_unit_ident None)
let mk_unit_val = Exp.construct mk_unit_ident None

let mk_if if_cond if_body else_body =
  let mk_if_cond ?(loc_then = !default_loc) ?(attrs = []) if_cond if_body =
    { if_cond; if_body; if_attrs = attrs; if_loc_then = loc_then }
  in
  Exp.ifthenelse [ mk_if_cond if_cond if_body ] (Some (else_body, !default_loc))

(** [Lwt.bind input cont_expr]. *)
let mk_lwt_bind_expr input cont_expr =
  Exp.apply
    (Exp.ident (mk_longident [ "Lwt"; "bind" ]))
    [ (Nolabel, input); (Nolabel, cont_expr) ]

(** [Lwt.bind input (fun param -> body)]. *)
let mk_lwt_bind input ?(param = mk_unit_arg) body =
  mk_lwt_bind_expr input (Exp.function_ [ param ] None (Pfunction_body body))

let mk_lwt_catch body input =
  let input_thunk = Exp.function_ [ mk_unit_arg ] None (Pfunction_body input) in
  Exp.apply
    (Exp.ident (mk_longident [ "Lwt"; "catch" ]))
    [ (Nolabel, input_thunk); (Nolabel, body) ]

let mk_lwt_fail_ident = Exp.ident (mk_longident [ "Lwt"; "fail" ])
let mk_lwt_return_unit = Exp.ident (mk_longident [ "Lwt"; "return_unit" ])

(** Rewrite an expression embedded in a [[%lwt ..]] or an expression like
    [match%lwt]. *)
let rewrite_lwt_extension_expression ~attrs exp =
  match exp.pexp_desc with
  (* [match%lwt]. *)
  | Pexp_match (input_exp, cases) ->
      let body = Exp.function_ [] None (mk_function_cases ~attrs cases) in
      Some (mk_lwt_bind_expr input_exp body)
  (* [try%lwt]. *)
  | Pexp_try (input_exp, cases) ->
      let body = Exp.function_ [] None (mk_function_cases ~attrs cases) in
      Some (mk_lwt_catch body input_exp)
  (* [for%lwt]. ppx_lwt doesn't work with other patterns. *)
  | Pexp_for
      (({ ppat_desc = Ppat_var p_var; _ } as pat), exp_from, exp_to, dir, body)
    ->
      let comp, op =
        match dir with Upto -> (">", "+") | Downto -> ("<", "-")
      in
      let op a b = Exp.apply (mk_exp_var op) [ (Nolabel, a); (Nolabel, b) ] in
      let comp a b =
        Exp.apply (mk_exp_var comp) [ (Nolabel, a); (Nolabel, b) ]
      in
      let p_exp = mk_exp_var p_var.txt in
      Some
        (mk_let (Pat.var (mk_loc "__ppx_lwt_bound")) exp_to
        @@ mk_let ~rec_:Recursive
             (Pat.var (mk_loc "__ppx_lwt_loop"))
             ~args:[ mk_function_param pat ]
             (mk_if
                (comp p_exp (mk_exp_var "__ppx_lwt_bound"))
                mk_lwt_return_unit
                (mk_lwt_bind body
                   (Exp.apply
                      (mk_exp_var "__ppx_lwt_loop")
                      [ (Nolabel, op p_exp (Exp.constant (Const.int 1))) ])))
        @@ Exp.apply (mk_exp_var "__ppx_lwt_loop") [ (Nolabel, exp_from) ])
  (* [while%lwt]. *)
  | Pexp_while (cond, body) ->
      Some
        (mk_let ~rec_:Recursive
           (Pat.var (mk_loc "__ppx_lwt_loop"))
           ~args:[ mk_unit_arg ]
           (mk_if cond
              (mk_lwt_bind_expr body (mk_exp_var "__ppx_lwt_loop"))
              mk_lwt_return_unit)
        @@ Exp.apply (mk_exp_var "__ppx_lwt_loop") [ (Nolabel, mk_unit_val) ])
  (* [e ;%lwt e'] *)
  | Pexp_sequence (e, e') -> Some (mk_lwt_bind e e')
  (* [assert%lwt e] *)
  | Pexp_assert _ -> Some (mk_lwt_catch mk_lwt_fail_ident exp)
  | _ -> None

let rewrite_expression exp =
  match exp.pexp_desc with
  (* Expressions like [match%lwt ..] and [[%lwt ..]]. *)
  | Pexp_extension
      ({ txt = "lwt"; _ }, PStr [ { pstr_desc = Pstr_eval (exp, attrs); _ } ])
    ->
      rewrite_lwt_extension_expression ~attrs exp
  (* Some expressions like [let%lwt] are not a [Pexp_extension] in OCamlformat's
     AST. *)
  (* [let%lwt pvb = body in] *)
  | Pexp_let
      ( {
          pvbs_bindings =
            [
              {
                pvb_attributes =
                  { attrs_extension = Some { txt = "lwt"; _ }; _ };
                pvb_args = [];
                pvb_is_pun = false;
                pvb_pat;
                pvb_body = Pfunction_body promise_exp;
                pvb_constraint;
                pvb_loc = _;
              };
            ];
          pvbs_rec = Nonrecursive;
        (* [let rec] is not handled by ppx_lwt. *)
        },
        body,
        _loc ) ->
      let param =
        let param_pat =
          match pvb_constraint with
          (* [locally_abstract_univars] are unlikely to present. *)
          | Some (Pvc_constraint { locally_abstract_univars = _ :: _; _ }) ->
              assert false
          (* Let binding coercion are unlikely to be used with %lwt in the
             wild. *)
          | Some (Pvc_coercion _) -> assert false
          | Some (Pvc_constraint { locally_abstract_univars = _; typ }) ->
              Pat.constraint_ pvb_pat typ
          | None -> pvb_pat
        in
        mk_function_param param_pat
      in
      Some (mk_lwt_bind promise_exp ~param body)
  | _ -> None

let remove_lwt_ppx =
  let default = Ast_mapper.default_mapper in
  let expr m exp =
    default.expr m (Option.value (rewrite_expression exp) ~default:exp)
  in
  let m = { default with expr } in
  m.structure m
