open Ocamlformat_utils.Parsing
open Asttypes
open Parsetree
open Ast_helper
open Ocamlformat_utils.Ast_utils
module Occ = Migrate_utils.Occ

(* let add_comment state ?loc text = *)
(*   Migrate_utils.add_comment state ?loc ("TODO: lwt-log-to-logs: " ^ text) *)

let mk_log section cmd args =
  (* Logs.$cmd ~src:$section (fun fmt -> fmt $arg) *)
  let msgf =
    let fmt_pat = Pat.var (mk_loc "fmt") and fmt_exp = mk_exp_var "fmt" in
    let body = Exp.apply fmt_exp args in
    Exp.function_ [ mk_function_param fmt_pat ] None (Pfunction_body body)
  in
  let src_arg =
    match section with
    | Some (section, `Lbl) -> [ (mk_lbl "src", section) ]
    | Some (section, `Opt) -> [ (mk_lblopt "src", section) ]
    | None -> []
  in
  mk_apply_ident [ "Logs"; cmd ] (src_arg @ [ (Nolabel, msgf) ])

let rewrite_apply_lwt_log_core ~state:_ ident args =
  let open Unpack_apply in
  unapply args
  @@
  match ident with
  | "ign_info" | "ign_info_f" ->
      take_lblopt "section" @@ fun section ->
      take @@ fun fmt_arg ->
      take_all @@ fun args ->
      Some (mk_log section "info" ((Nolabel, fmt_arg) :: args))
  | _ -> return None

let rewrite_expression ~state exp =
  (* Flatten pipelines before applying rewrites. *)
  match (flatten_apply exp).pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident lid; _ }, args) ->
      Occ.may_rewrite state lid (function
        | "Lwt_log_core", ident -> rewrite_apply_lwt_log_core ~state ident args
        | _ -> None)
  | _ -> None

let mapper ~state =
  let default = Ast_mapper.default_mapper in
  let rec call_rewrite ~default ~loc f m x =
    Migrate_utils.set_default_comment_loc state loc;
    (* Apply the rewrite again if it succeed *)
    match f x with
    | Some x -> call_rewrite ~default ~loc f m x
    | None -> default m x
  in
  let expr m x =
    call_rewrite ~default:default.expr ~loc:x.pexp_loc
      (rewrite_expression ~state)
      m x
  in
  { default with expr }

let modify_ast ~fname:_ =
  let structure state str =
    let m = mapper ~state in
    m.structure m str
  in
  let signature state sg =
    let m = mapper ~state in
    m.signature m sg
  in
  { Migrate_utils.structure; signature }

let main migrate =
  let units = function
    | "Lwt_log" | "Lwt_daemon" | "Lwt_log_core" | "Lwt_log_rules" -> true
    | _ -> false
  in
  let packages = [ "lwt_log"; "lwt_log.core" ] in
  if migrate then Migrate_utils.migrate ~packages ~units ~modify_ast
  else Migrate_utils.print_occurrences ~packages ~units

open Cmdliner

let opt_migrate =
  let doc =
    "Modify the source code instead of printing occurrences of Lwt_log."
  in
  Arg.(value & flag & info ~doc [ "migrate" ])

let cmd =
  let doc = "Migrate your codebase from Lwt_log to Logs." in
  let info = Cmd.info "lwt-log-to-logs" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(const main $ opt_migrate)

let () = exit (Cmd.eval cmd)
