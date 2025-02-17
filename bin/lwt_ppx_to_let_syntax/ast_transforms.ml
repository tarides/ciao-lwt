open Ocamlformat_utils.Parsing
open Asttypes
open Parsetree
open Ast_helper
open Ocamlformat_utils.Ast_utils

(** Whether [let*] was used and an [open Lwt.Syntax] is required. *)
let letop_was_used = ref false

(** Whether to use [Lwt.bind] instead of [let*]. *)
let use_lwt_bind = ref false

(** [Lwt.bind input cont_expr]. *)
let mk_lwt_bind_expr input cont_expr =
  Exp.apply
    (Exp.ident (mk_longident [ "Lwt"; "bind" ]))
    [ (Nolabel, input); (Nolabel, cont_expr) ]

(** [Lwt.bind input (fun param -> body)]. *)
let mk_lwt_bind input ?(param = mk_unit_pat) body =
  if !use_lwt_bind then
    mk_lwt_bind_expr input
      (Exp.function_ [ mk_function_param param ] None (Pfunction_body body))
  else (
    letop_was_used := true;
    Exp.letop ~loc_in:!default_loc
      (mk_binding_op (mk_loc "let*") param input)
      [] body)

let mk_lwt_catch_exp input body =
  Exp.apply
    (Exp.ident (mk_longident [ "Lwt"; "catch" ]))
    [ (Nolabel, mk_thunk input); (Nolabel, body) ]

(** Makes sure that the exception matching is exhaustive by adding a catch-all
    case doing a reraise. *)
let add_exn_catch_all cases =
  let rec is_catchall_pat pat =
    match pat.ppat_desc with
    | Ppat_any | Ppat_var _ -> true
    | Ppat_alias (p, _) | Ppat_constraint (p, _) | Ppat_open (_, p) ->
        is_catchall_pat p
    | Ppat_or pats -> List.exists is_catchall_pat pats
    | _ -> false
  in
  let is_catchall case = is_catchall_pat case.pc_lhs in
  if List.exists is_catchall cases then cases
  else
    let catch_all =
      Exp.case
        (Pat.var (mk_loc "exc"))
        (Exp.apply
           (Exp.ident (mk_longident [ "Lwt"; "reraise" ]))
           [ (Nolabel, mk_exp_var "exc") ])
    in
    cases @ [ catch_all ]

let mk_lwt_catch input cases =
  let cases = add_exn_catch_all cases in
  let exp =
    match cases with
    (* Generate a [fun] instead of a [function] when there's a single case to make the formatting nicer. *)
    | [ { pc_lhs; pc_guard = None; pc_rhs } ] ->
        Exp.function_ [ mk_function_param pc_lhs ] None (Pfunction_body pc_rhs)
    | cases -> mk_function_cases cases
  in
  mk_lwt_catch_exp input exp

let mk_lwt_try_bind input value_f exn_cases =
  let exn_f = mk_function_cases (add_exn_catch_all exn_cases) in
  Exp.apply
    (Exp.ident (mk_longident [ "Lwt"; "try_bind" ]))
    [ (Nolabel, mk_thunk input); (Nolabel, value_f); (Nolabel, exn_f) ]

let mk_lwt_fail_ident = Exp.ident (mk_longident [ "Lwt"; "fail" ])
let mk_lwt_return_unit = Exp.ident (mk_longident [ "Lwt"; "return_unit" ])

let match_extract_exception_cases =
  List.partition_map (fun case ->
      match case.pc_lhs.ppat_desc with
      | Ppat_exception exn_pat ->
          Either.Right (Exp.case exn_pat ?guard:case.pc_guard case.pc_rhs)
      | _ -> Left case)

(** Rewrite a [let%lwt] using the same translation used by [lwt_ppx]. *)
let rewrite_lwt_let_expression_using_lwt_bind bindings body =
  let generate_parallel_binds bindings =
    let gen_name i = "__ppx_lwt_" ^ string_of_int i in
    let bindings = List.mapi (fun i b -> (gen_name i, b)) bindings in
    let generated_mangled_bindings =
      List.map
        (fun (name, (_pat, exp)) ->
          Vb.mk ~is_pun:false (Pat.var (mk_loc name)) [] (Pfunction_body exp))
        bindings
    in
    let rec generated_lwt_binds bindings acc =
      match bindings with
      | [] -> acc
      | (name, (pat, _exp)) :: tl ->
          generated_lwt_binds tl @@ mk_lwt_bind (mk_exp_var name) ~param:pat acc
    in
    mk_let' generated_mangled_bindings @@ generated_lwt_binds bindings @@ body
  in
  let apply_constraint pvb_pat promise_exp pvb_constraint =
    match pvb_constraint with
    (* [locally_abstract_univars] are unlikely to present. *)
    | Some (Pvc_constraint { locally_abstract_univars = _ :: _; _ }) ->
        assert false
    | Some (Pvc_coercion { ground; coercion }) ->
        (* Generate [Lwt.bind (promise_exp :> coercion) (fun pat -> body)]. *)
        (pvb_pat, Exp.coerce promise_exp ground coercion)
    | Some (Pvc_constraint { locally_abstract_univars = []; typ }) ->
        (* Generate [Lwt.bind promise_exp (fun (pat : typ) -> body)]. *)
        (Pat.constraint_ pvb_pat typ, promise_exp)
    | None -> (pvb_pat, promise_exp)
  in
  let decode_bindings =
    let exception Unsupported in
    let decode = function
      | {
          (* Bindings that define functions are not supported by lwt_ppx (with
             non-empty [pvb_args] or with [Pfunction_case] body). *)
          pvb_args = [];
          pvb_is_pun = _;
          pvb_pat;
          pvb_body = Pfunction_body promise_exp;
          pvb_constraint;
          pvb_loc = _;
          pvb_attributes = { attrs_before = []; attrs_after = []; _ };
        } ->
          apply_constraint pvb_pat promise_exp pvb_constraint
      | _ -> raise Unsupported
    in
    try Some (List.map decode bindings) with Unsupported -> None
  in
  match decode_bindings with
  | Some [ (param, promise_exp) ] ->
      (* Simple let binding. *)
      Some (mk_lwt_bind promise_exp ~param body)
  | Some bindings -> Some (generate_parallel_binds bindings)
  | None -> None

(** Rewrite a [let%lwt]. *)
let rewrite_lwt_let_expression bindings body =
  let decode_bindings =
    let exception Unsupported in
    let decode = function
      | {
          (* Bindings that define functions are not supported by lwt_ppx (with
             non-empty [pvb_args] or with [Pfunction_case] body). *)
          pvb_args = [];
          pvb_is_pun = is_pun;
          pvb_pat;
          pvb_body = Pfunction_body promise_exp;
          (* Other values for [pvb_constraint] are not handled correctly by
             ocamlformat 0.27.0. *)
          pvb_constraint =
            (Some (Pvc_constraint { locally_abstract_univars = []; _ }) | None)
            as typ;
          pvb_loc = _;
          pvb_attributes = { attrs_before = []; attrs_after = []; _ };
        } ->
          fun op -> mk_binding_op ~is_pun (mk_loc op) pvb_pat ~typ promise_exp
      | _ -> raise Unsupported
    in
    try List.map decode bindings with Unsupported -> []
  in
  if !use_lwt_bind then rewrite_lwt_let_expression_using_lwt_bind bindings body
  else
    match decode_bindings with
    | [] -> None
    | let_ :: ands ->
        letop_was_used := true;
        let let_ = let_ "let*"
        and ands = List.map (fun and_ -> and_ "and*") ands in
        Some (Exp.letop ~loc_in:!default_loc let_ ands body)

(** Rewrite an expression embedded in a [[%lwt ..]] or an expression like
    [match%lwt]. *)
let rewrite_lwt_extension_expression exp =
  match exp.pexp_desc with
  (* [match%lwt]. *)
  | Pexp_match (input_exp, cases) -> (
      match match_extract_exception_cases cases with
      | cases, [] ->
          (* No exception cases *)
          Some (mk_lwt_bind_expr input_exp (mk_function_cases cases))
      | [], exn_cases ->
          (* Only exception cases *)
          Some (mk_lwt_catch input_exp exn_cases)
      | value_cases, exn_cases ->
          (* Both value and exception cases *)
          Some
            (mk_lwt_try_bind input_exp
               (mk_function_cases value_cases)
               exn_cases))
  (* [try%lwt]. *)
  | Pexp_try (input_exp, cases) -> Some (mk_lwt_catch input_exp cases)
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
  | Pexp_assert _ -> Some (mk_lwt_catch_exp exp mk_lwt_fail_ident)
  (* [if%lwt c then a else b *)
  | Pexp_ifthenelse
      ({ if_cond; if_body; if_attrs; _ } :: elseif_branches, else_branch) ->
      let else_exp =
        (* OCamlformat encodes chains of [else if] as a list so we must construct
         the rhs expression in some cases. *)
        match (elseif_branches, else_branch) with
        | [], Some (e, _) -> e
        | [], None -> mk_lwt_return_unit
        | _ -> Exp.ifthenelse elseif_branches else_branch
      in
      let constr_case ident body =
        Exp.case (Pat.construct (mk_longident [ ident ]) None) body
      in
      let body =
        mk_function_cases ~attrs:if_attrs
          [ constr_case "true" if_body; constr_case "false" else_exp ]
      in
      Some (mk_lwt_bind_expr if_cond body)
  (* [[%lwt let a = b in ..]]. *)
  | Pexp_let ({ pvbs_bindings; pvbs_rec = Nonrecursive }, body, _) ->
      rewrite_lwt_let_expression pvbs_bindings body
  | _ -> None

let rewrite_expression exp =
  match exp.pexp_desc with
  (* Expressions like [match%lwt ..] and [[%lwt ..]]. *)
  | Pexp_extension
      ({ txt = "lwt"; _ }, PStr [ { pstr_desc = Pstr_eval (exp, []); _ } ]) ->
      rewrite_lwt_extension_expression exp
  (* Some expressions like [let%lwt] are not a [Pexp_extension] in OCamlformat's
     AST. *)
  (* [let%lwt pvb = body in] *)
  | Pexp_let
      ( {
          pvbs_bindings =
            {
              pvb_attributes = { attrs_extension = Some { txt = "lwt"; _ }; _ };
              _;
            }
            :: _ as bindings;
          (* [let rec] is not handled by ppx_lwt. *)
          pvbs_rec = Nonrecursive;
        },
        body,
        _ ) ->
      rewrite_lwt_let_expression bindings body
  (* [exp [%finally f]] *)
  | Pexp_apply
      ( lhs,
        [
          ( Nolabel,
            {
              pexp_desc =
                Pexp_extension
                  ( { txt = "finally" | "lwt.finally"; _ },
                    PStr [ { pstr_desc = Pstr_eval (rhs, []); _ } ] );
              _;
            } );
        ] ) ->
      Some
        (Exp.apply
           (Exp.ident (mk_longident [ "Lwt"; "finalize" ]))
           [ (Nolabel, mk_thunk lhs); (Nolabel, mk_thunk rhs) ])
  | _ -> None

let mk_lwt_syntax_ident = mk_longident [ "Lwt"; "Syntax" ]
let mk_open_lwt_syntax = Str.open_ (Opn.mk (Mod.ident mk_lwt_syntax_ident))

let structure_has_open_lwt_syntax =
  List.exists (function
    | {
        pstr_desc =
          Pstr_open { popen_expr = { pmod_desc = Pmod_ident opn_ident; _ }; _ };
        _;
      } ->
        opn_ident.txt = mk_lwt_syntax_ident.txt
    | _ -> false)

let remove_lwt_ppx ~use_lwt_bind:use_lwt_bind_ str =
  use_lwt_bind := use_lwt_bind_;
  letop_was_used := false;
  let default = Ast_mapper.default_mapper in
  let expr m exp =
    default.expr m (Option.value (rewrite_expression exp) ~default:exp)
  in
  let m = { default with expr } in
  let str = m.structure m str in
  if !letop_was_used && not (structure_has_open_lwt_syntax str) then
    mk_open_lwt_syntax :: str
  else str
