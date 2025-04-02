open Ocamlformat_utils.Parsing
open Asttypes
open Parsetree
open Ast_helper
open Ocamlformat_utils.Ast_utils
module Occ = Migrate_utils.Occ

let add_comment state ?loc text =
  Migrate_utils.add_comment state ?loc ("TODO: lwt-log-to-logs: " ^ text)

let mk_lwt_return_unit = Exp.ident (mk_longident [ "Lwt"; "return_unit" ])

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

let mk_set_level section lvl =
  mk_apply_simple [ "Logs"; "Src"; "set_level" ] [ section; lvl ]

let mk_format_reporter ~state ~template ~close_mode channel =
  if Option.is_some template then
    add_comment state
      "Lwt_log.channel: The [~template] argument is unsupported. Use \
       [~pp_header] instead.";
  (match close_mode.pexp_desc with
  | Pexp_variant (cstr, None) when cstr.txt.txt = "Keep" -> ()
  | _ ->
      add_comment state
        "Lwt_log.channel: The [~close_mode] argument has been dropped. The \
         behavior is always [`Keep].");
  let fmt_ident = "logs_formatter" in
  let fmt_val = mk_exp_var fmt_ident in
  add_comment state
    "Format.formatter_of_out_channel: Argument is a [Lwt_io.output_channel] \
     but a [out_channel] is expected.";
  mk_let
    (Pat.var (mk_loc fmt_ident))
    (mk_apply_simple [ "Format"; "formatter_of_out_channel" ] [ channel ])
    (mk_apply_ident
       [ "Logs"; "format_reporter" ]
       [
         (mk_lbl "app", fmt_val); (mk_lbl "dst", fmt_val); (Nolabel, mk_unit_val);
       ])

let rewrite_apply_lwt_log ~state (unit, ident) args =
  let open Unpack_apply in
  let ignore_lblarg arg k =
    take_lblopt arg @@ fun value ->
    (match value with
    | Some (_, kind) ->
        let prefix = match kind with `Lbl -> '~' | `Opt -> '?' in
        Printf.ksprintf (add_comment state)
          "Labelled argument %c%s was dropped." prefix arg
    | None -> ());
    k
  in
  let logf ~mk_log logs_name =
    take_lblopt "section" @@ fun section ->
    ignore_lblarg "exn" @@ ignore_lblarg "location" @@ ignore_lblarg "logger"
    @@ take
    @@ fun fmt_arg ->
    take_all @@ fun args ->
    Some (mk_log section logs_name ((Nolabel, fmt_arg) :: args))
  in
  let log_unit n = logf ~mk_log n in
  let log_lwt n =
    let mk_log section n args =
      Exp.sequence (mk_log section n args) mk_lwt_return_unit
    in
    logf ~mk_log n
  in
  let mk_level cstr = return (Some (mk_constr_exp [ "Logs"; cstr ])) in

  unapply args
  @@
  match unit with
  | "Lwt_log_core" -> (
      match ident with
      | "ign_debug" | "ign_debug_f" -> log_unit "debug"
      | "ign_info" | "ign_info_f" -> log_unit "info"
      | "ign_notice" | "ign_notice_f" -> log_unit "app"
      | "ign_warning" | "ign_warning_f" -> log_unit "warn"
      | "ign_error" | "ign_error_f" -> log_unit "err"
      | "ign_fatal" | "ign_fatal_f" ->
          add_comment state "This message was previously on the [fatal] level.";
          log_unit "err"
      | "debug" | "debug_f" -> log_lwt "debug"
      | "info" | "info_f" -> log_lwt "info"
      | "notice" | "notice_f" -> log_lwt "app"
      | "warning" | "warning_f" -> log_lwt "warn"
      | "error" | "error_f" -> log_lwt "err"
      | "fatal" | "fatal_f" ->
          add_comment state "This message was previously on the [fatal] level.";
          log_lwt "err"
      | "make" ->
          (* [Lwt_log.Section.make] is detected as [("Lwt_log_core", "make")]. *)
          take @@ fun name ->
          return (Some (mk_apply_simple [ "Logs"; "Src"; "create" ] [ name ]))
      | "set_level" ->
          take @@ fun section ->
          take @@ fun lvl ->
          return (Some (mk_set_level section (mk_exp_some lvl)))
      | "reset_level" ->
          take @@ fun section ->
          return (Some (mk_set_level section mk_exp_none))
      | "null" -> return (Some (mk_exp_ident [ "Logs"; "nop_reporter" ]))
      | "!default" ->
          return (Some (mk_apply_simple [ "Logs"; "reporter" ] [ mk_unit_val ]))
      | "default :=" ->
          take @@ fun r ->
          return (Some (mk_apply_simple [ "Logs"; "set_reporter" ] [ r ]))
      | "default" ->
          add_comment state "Use [Logs.set_reporter : reporter -> unit].";
          return None
      | "Debug" -> mk_level "Debug"
      | "Info" -> mk_level "Info"
      | "Notice" -> mk_level "App"
      | "Warning" -> mk_level "Warning"
      | "Error" -> mk_level "Error"
      | "Fatal" -> mk_level "Error"
      | _ -> return None)
  | "Lwt_log" -> (
      match ident with
      | "channel" ->
          take_lblopt "template" @@ fun template ->
          take_lbl "close_mode" @@ fun close_mode ->
          take_lbl "channel" @@ fun channel ->
          take @@ fun _unit ->
          return
            (Some (mk_format_reporter ~state ~template ~close_mode channel))
      | _ -> return None)
  | _ -> return None

let rewrite_expression ~state exp =
  match
    rewrite_apply exp (fun lid args ->
        Occ.may_rewrite state lid (fun ident ->
            rewrite_apply_lwt_log ~state ident args))
  with
  | Some _ as x -> x
  | None -> (
      (* Rewrite uses of [Lwt_log.default]. *)
      match exp.pexp_desc with
      | Pexp_prefix ({ txt = "!"; _ }, { pexp_desc = Pexp_ident lid; _ }) ->
          Occ.may_rewrite state lid (fun (unit, ident) ->
              rewrite_apply_lwt_log ~state (unit, "!" ^ ident) [])
      | Pexp_infix ({ txt = ":="; _ }, { pexp_desc = Pexp_ident lid; _ }, rhs)
        ->
          Occ.may_rewrite state lid (fun (unit, ident) ->
              rewrite_apply_lwt_log ~state
                (unit, ident ^ " :=")
                [ (Nolabel, rhs) ])
      | _ -> None)

let rewrite_type ~state typ =
  match typ.ptyp_desc with
  | Ptyp_constr (lid, params) ->
      Occ.may_rewrite state lid (fun ident ->
          match (ident, params) with
          | ("Lwt_log_core", "section"), [] ->
              Some (mk_typ_constr [ "Logs"; "src" ])
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
  let typ m x =
    call_rewrite ~default:default.typ ~loc:x.ptyp_loc (rewrite_type ~state) m x
  in
  { default with expr; typ }

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
