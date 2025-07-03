(* SPDX-License-Identifier: MIT
 * Copyright (c) 2025 Jules Aguillon <jules@j3s.fr>
 *)

open Ocamlformat_utils.Parsing
open Parsetree

let error_count = ref 0

let warn ~loc fmt =
  let { Location.loc_start = p; _ } = loc in
  incr error_count;
  Format.eprintf
    ("@[<hv 2>%s:%d:%d@;<2 0>" ^^ fmt ^^ "@]@\n")
    p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

let rec is_ignore_pat pat =
  match pat.ppat_desc with
  | Ppat_any -> Some ("_", pat.ppat_loc)
  | Ppat_var var when String.starts_with ~prefix:"_" var.txt ->
      Some (var.txt, pat.ppat_loc)
  | Ppat_tuple pats -> List.find_map is_ignore_pat pats
  | _ -> None

let lint_value_binding pvb =
  match (is_ignore_pat pvb.pvb_pat, pvb.pvb_constraint, pvb.pvb_args) with
  | Some (ignore_ident, loc), None, [] ->
      warn ~loc
        "Ignored value without a type annotation. Pattern %S ignores a value \
         that might be a Lwt promise."
        ignore_ident
  | _ -> ()

let lint_structure_item stri =
  match stri.pstr_desc with
  | Pstr_value { pvbs_bindings; _ } ->
      List.iter lint_value_binding pvbs_bindings
  | _ -> ()

let lint_expression exp =
  match exp.pexp_desc with
  | Pexp_let ({ pvbs_bindings; _ }, _, _) ->
      List.iter lint_value_binding pvbs_bindings
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { txt = Lident "ignore"; _ }; _ },
        [ (Nolabel, arg) ] ) -> (
      match arg.pexp_desc with
      | Pexp_constraint _ | Pexp_coerce _ -> ()
      | _ ->
          warn ~loc:arg.pexp_loc
            "Ignored value without a type annotation. Argument of \"ignore\" \
             should have a type annotation.")
  | _ -> ()

let lint_mapper =
  let default = Ast_mapper.default_mapper in
  let structure_item m stri =
    lint_structure_item stri;
    default.structure_item m stri
  in
  let expr m exp =
    lint_expression exp;
    default.expr m exp
  in
  { default with structure_item; expr }

let lint_file file =
  match Ocamlformat_utils.parse ~file with
  | Ok (`Structure str) -> ignore (lint_mapper.structure lint_mapper str)
  | Ok (`Signature sg) -> ignore (lint_mapper.signature lint_mapper sg)
  | Error (`Msg msg) ->
      Format.eprintf "%s: %s\n%!" file msg;
      incr error_count

let main paths =
  error_count := 0;
  List.iter (Fs_utils.find_ml_files lint_file) paths;
  Format.eprintf "%d errors\n%!" !error_count;
  if !error_count > 0 then exit 1

open Cmdliner

let pos_inputs =
  let doc = "Path to files or directories to lint." in
  Arg.(non_empty & pos_all file [] & info ~doc ~docv:"PATH" [])

let cmd =
  let doc = "Lint code that might cause implicit forking in Lwt." in
  let info = Cmd.info "lwt-lint" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(const main $ pos_inputs)

let () = exit (Cmd.eval cmd)
