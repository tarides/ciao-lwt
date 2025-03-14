open Ocamlformat_ocaml_common
open Ocamlformat_parser_extended
open Asttypes
open Parsetree
open Ast_helper

let mk_loc ?(loc = !default_loc) txt = { Location.txt; loc }

let mk_function_param ?(loc = !default_loc) ?(lbl = Nolabel) ?def pat =
  { pparam_loc = loc; pparam_desc = Pparam_val (lbl, def, pat) }

let mk_let' ?(loc_in = !default_loc) ?(rec_ = Nonrecursive) bindings rhs =
  let bindings = { pvbs_bindings = bindings; pvbs_rec = rec_ } in
  Exp.let_ ~loc_in bindings rhs

let mk_let ?loc_in ?rec_ ?(is_pun = false) ?value_constraint pat ?(args = [])
    lhs rhs =
  let binding = Vb.mk ~is_pun ?value_constraint pat args (Pfunction_body lhs) in
  mk_let' ?loc_in ?rec_ [ binding ] rhs

let mk_function_cases ?(loc = !default_loc) ?(attrs = []) cases =
  Exp.function_ [] None (Pfunction_cases (cases, loc, attrs))

let mk_longident' = function
  | [] -> assert false
  | hd :: tl ->
      let open Longident in
      List.fold_left (fun acc seg -> Ldot (acc, seg)) (Lident hd) tl

let mk_longident ident = mk_loc (mk_longident' ident)
let mk_constr_exp ?arg cstr = Exp.construct (mk_longident [ cstr ]) arg
let same_longident a b = Longident.flatten a = b
let mk_exp_ident ident = Exp.ident (mk_longident ident)
let mk_exp_var s = mk_exp_ident [ s ]
let mk_unit_ident = mk_longident [ "()" ]
let mk_unit_pat = Pat.construct mk_unit_ident None
let mk_unit_arg = mk_function_param mk_unit_pat
let mk_unit_val = Exp.construct mk_unit_ident None
let mk_thunk body = Exp.function_ [ mk_unit_arg ] None (Pfunction_body body)
let mk_some_ident = mk_longident [ "Some" ]
let mk_none_ident = mk_longident [ "None" ]
let mk_exp_some x = Exp.construct mk_some_ident (Some x)
let mk_exp_none = Exp.construct mk_none_ident None

let is_unit_val = function
  | { pexp_desc = Pexp_construct (ident, None); _ } ->
      same_longident ident.txt [ "()" ]
  | _ -> false

let mk_if if_cond if_body else_body =
  let mk_if_cond ?(loc_then = !default_loc) ?(attrs = []) if_cond if_body =
    { if_cond; if_body; if_attrs = attrs; if_loc_then = loc_then }
  in
  Exp.ifthenelse [ mk_if_cond if_cond if_body ] (Some (else_body, !default_loc))

let mk_binding_op ?(loc = !default_loc) ?(is_pun = false) op pat ?(args = [])
    ?(typ = None) exp =
  Exp.binding_op op pat args typ exp is_pun loc

let mk_lbl s = Labelled (mk_loc s)
let mk_apply_ident ident args = Exp.apply (mk_exp_ident ident) args

let mk_apply_simple f_ident args =
  mk_apply_ident f_ident (List.map (fun x -> (Nolabel, x)) args)

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
