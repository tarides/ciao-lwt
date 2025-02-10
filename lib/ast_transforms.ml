open Ocamlformat_utils.Parsing
open Asttypes
open Parsetree

let mk_loc txt = { Location.txt; loc = !Ast_helper.default_loc }

let mk_function_param ?(loc = !Ast_helper.default_loc) ?(lbl = Nolabel) ?def pat
    =
  { pparam_loc = loc; pparam_desc = Pparam_val (lbl, def, pat) }

let lwt_bind_exp f v =
  let open Ast_helper in
  Exp.apply
    (Exp.ident (mk_loc (Longident.Ldot (Longident.Lident "Lwt", "bind"))))
    [ (Nolabel, f); (Nolabel, v) ]

let rewrite_expression exp =
  let open Ast_helper in
  match exp.pexp_desc with
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
        },
        body,
        _loc ) ->
      let body_fun =
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
        Exp.function_ [ mk_function_param param_pat ] None (Pfunction_body body)
      in
      Some (lwt_bind_exp body_fun promise_exp)
  | _ -> None

let remove_lwt_ppx =
  let default = Ast_mapper.default_mapper in
  let expr m exp =
    default.expr m (Option.value (rewrite_expression exp) ~default:exp)
  in
  let m = { default with expr } in
  m.structure m
