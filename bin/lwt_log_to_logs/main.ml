open Ocamlformat_utils.Parsing
open Asttypes
open Parsetree
open Ast_helper
open Ocamlformat_utils.Ast_utils
module Occ = Migrate_utils.Occ

let add_comment state ?loc text =
  Migrate_utils.add_comment state ?loc ("TODO: lwt-log-to-logs: " ^ text)

let mk_lwt_return_unit = Exp.ident (mk_longident [ "Lwt"; "return_unit" ])

let mk_log section cmd ~extra_args args =
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
  mk_apply_ident [ "Logs"; cmd ] (src_arg @ extra_args @ [ (Nolabel, msgf) ])

let mk_set_level section lvl =
  mk_apply_simple [ "Logs"; "Src"; "set_level" ] [ section; lvl ]

let mk_format_reporter_of_channel channel =
  mk_let_var "logs_formatter"
    (mk_apply_simple [ "Format"; "formatter_of_out_channel" ] [ channel ])
  @@ fun fmt ->
  mk_apply_ident
    [ "Logs"; "format_reporter" ]
    [ (mk_lbl "app", fmt); (mk_lbl "dst", fmt); (Nolabel, mk_unit_val) ]

let mk_channel ~state ~template ~close_mode channel =
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
  add_comment state
    "Format.formatter_of_out_channel: Argument is a [Lwt_io.output_channel] \
     but a [out_channel] is expected.";
  mk_format_reporter_of_channel channel

let mk_file ~state ~mode ~perm ~file_name =
  let mk_mode append_mode =
    Exp.list
      [
        append_mode;
        mk_constr_exp [ "Open_wronly" ];
        mk_constr_exp [ "Open_creat" ];
        mk_constr_exp [ "Open_text" ];
      ]
  in
  let mode =
    mk_let_var "append_mode"
      (Exp.match_
         (value_of_lblopt ~default:(mk_variant_exp "Append") mode)
         [
           Exp.case (mk_variant_pat "Append") (mk_constr_exp [ "Open_append" ]);
           Exp.case (mk_variant_pat "Truncate") (mk_constr_exp [ "Open_trunc" ]);
         ])
      mk_mode
  in
  let perm = value_of_lblopt ~default:(mk_const_int "0o640") perm in
  add_comment state "[file]: Channel is never closed.";
  mk_format_reporter_of_channel
    (mk_apply_simple [ "open_out_gen" ] [ mode; perm; file_name ])

let mk_reporter report_f =
  let report =
    let open Mk_function in
    mk_function
      (return report_f $ arg "src" $ arg "level" $ arg ~lbl:`Lbl "over"
     $ arg "k" $ arg "msgf")
  in
  Exp.record [ (mk_longident [ "Logs"; "report" ], None, Some report) ] None

let call_reporter r src level over k msgf =
  Exp.apply
    (Exp.field r (mk_longident [ "Logs"; "report" ]))
    [
      (Nolabel, src);
      (Nolabel, level);
      (mk_lbl "over", over);
      (Nolabel, k);
      (Nolabel, msgf);
    ]

let mk_broadcast ~state:_ loggers =
  mk_let_var "broadcast_reporters" loggers @@ fun reporters_var ->
  mk_reporter (fun src level over k msgf ->
      let f k r _unit = call_reporter r src level over k msgf in
      let f =
        let open Mk_function in
        mk_function (return f $ arg "k" $ arg "r" $ arg "_unit")
      in
      mk_apply_simple [ "List"; "fold_left" ]
        [ f; k; reporters_var; mk_unit_val ])

let mk_dispatch ~state:_ dispatch_f =
  mk_let_var "dispatch_f" dispatch_f @@ fun dispatch_f ->
  mk_reporter (fun src level ->
      call_reporter
        (Exp.apply dispatch_f [ (Nolabel, src); (Nolabel, level) ])
        src level)

let mk_syslog ~state ~paths ~facility =
  let socket_arg =
    match paths with
    | Some (paths, lbl) ->
        let lbl = match lbl with `Lbl -> mk_lbl | `Opt -> mk_lblopt in
        (match paths.pexp_desc with
        | Pexp_list [ _ ] -> ()
        | _ ->
            add_comment state
              "[Logs_syslog_unix.unix_reporter] take a single path but a list \
               is passed.");
        [ (lbl "socket", paths) ]
    | None -> []
  in
  add_comment state
    "Add dependency on library [logs-syslog.unix] and package [logs-syslog].";
  add_comment state "The [~facility] argument is not of the right type.";
  Exp.apply
    (mk_exp_ident [ "Logs_syslog_unix"; "unix_reporter" ])
    (socket_arg @ [ (mk_lbl "facility", facility); (Nolabel, mk_unit_val) ])

(** Translate [Lwt_log_js.console] using the [logs.browser] library. *)
let mk_console ~state:_ =
  mk_apply_simple [ "Logs_browser"; "console_reporter" ] [ mk_unit_val ]

(** Whether an expression can be used as a format spec. *)
let format_safe exp =
  match exp.pexp_desc with
  | Pexp_constant { pconst_desc = Pconst_string (s, _, _); _ } ->
      String.for_all (function '%' | '@' -> false | _ -> true) s
  | _ -> false

let rewrite_apply_lwt_log ~state (unit, ident) args =
  let open Unpack_apply in
  let ignore_lblarg ?(cmt = "") arg k =
    take_lblopt arg @@ fun value ->
    (match value with
    | Some (_, kind) ->
        let prefix = match kind with `Lbl -> '~' | `Opt -> '?' in
        Printf.ksprintf (add_comment state)
          "Labelled argument %c%s was dropped.%s" prefix arg cmt
    | None -> ());
    k
  in
  let logf ?(extra_args = []) ~ident logs_name =
    let wrap_lwt exp =
      if String.starts_with ~prefix:"ign_" ident then exp
      else Exp.sequence exp mk_lwt_return_unit
    in
    ignore_lblarg "inspect" @@ take_lblopt "section"
    @@ fun section ->
    take_lblopt "exn" @@ fun exn ->
    ignore_lblarg "location" @@ ignore_lblarg "logger" @@ take
    @@ fun fmt_arg ->
    take_all @@ fun args ->
    let fmt_arg, args =
      (* Log calls that don't end in [_f] use a ["%s"] format string to avoid
         any typing and escaping issues. *)
      if String.ends_with ~suffix:"_f" ident || format_safe fmt_arg then
        (fmt_arg, args)
      else (mk_const_string "%s", (Nolabel, fmt_arg) :: args)
    in
    let fmt_arg, args =
      (* Print the [exn] argument. *)
      match exn with
      | Some (exn, lbl) ->
          if lbl = `Opt then
            add_comment state
              "Last argument is a [exception option] while [exception] is \
               expected.";
          ( Exp.infix (mk_loc "^^") fmt_arg (mk_const_string "@\n%s"),
            args
            @ [ (Nolabel, mk_apply_simple [ "Printexc"; "to_string" ] [ exn ]) ]
          )
      | None -> (fmt_arg, args)
    in
    Some
      (wrap_lwt
         (mk_log section logs_name ~extra_args ((Nolabel, fmt_arg) :: args)))
  in
  let mk_level cstr = return (Some (mk_constr_exp [ "Logs"; cstr ])) in

  unapply args
  @@
  match (unit, ident) with
  (* Logging functions are defined in [Lwt_log_core] and [Lwt_log_js]. *)
  | _, ("log" | "log_f" | "ign_log" | "ign_log_f") ->
      take_lbl "level" @@ fun level ->
      logf ~extra_args:[ (Nolabel, level) ] ~ident "msg"
  | _, ("debug" | "debug_f" | "ign_debug" | "ign_debug_f") ->
      logf ~ident "debug"
  | _, ("info" | "info_f" | "ign_info" | "ign_info_f") -> logf ~ident "info"
  | _, ("notice" | "notice_f" | "ign_notice" | "ign_notice_f") ->
      logf ~ident "app"
  | _, ("warning" | "warning_f" | "ign_warning" | "ign_warning_f") ->
      logf ~ident "warn"
  | _, ("error" | "error_f" | "ign_error" | "ign_error_f") -> logf ~ident "err"
  | _, ("fatal" | "fatal_f" | "ign_fatal" | "ign_fatal_f") ->
      add_comment state "This message was previously on the [fatal] level.";
      logf ~ident "err"
  | "Lwt_log_core", _ -> (
      match ident with
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
      | "broadcast" ->
          take @@ fun loggers -> return (Some (mk_broadcast ~state loggers))
      | "dispatch" -> take @@ fun f -> return (Some (mk_dispatch ~state f))
      | "Debug" -> mk_level "Debug"
      | "Info" -> mk_level "Info"
      | "Notice" -> mk_level "App"
      | "Warning" -> mk_level "Warning"
      | "Error" -> mk_level "Error"
      | "Fatal" -> mk_level "Error"
      | "add_rule" | "close" ->
          add_comment state (ident ^ " is no longer supported.");
          return None
      | _ -> return None)
  | "Lwt_log", _ -> (
      match ident with
      | "channel" ->
          take_lblopt "template" @@ fun template ->
          take_lbl "close_mode" @@ fun close_mode ->
          take_lbl "channel" @@ fun channel ->
          take @@ fun _unit ->
          return (Some (mk_channel ~state ~template ~close_mode channel))
      | "syslog" ->
          ignore_lblarg "template" @@ take_lblopt "paths"
          @@ fun paths ->
          take_lbl "facility" @@ fun facility ->
          take @@ fun _unit -> return (Some (mk_syslog ~state ~paths ~facility))
      | "file" ->
          ignore_lblarg "template" @@ take_lblopt "mode"
          @@ fun mode ->
          take_lblopt "perm" @@ fun perm ->
          take_lbl "file_name" @@ fun file_name ->
          take @@ fun _unit ->
          return (Some (mk_file ~state ~mode ~perm ~file_name))
      | _ -> return None)
  | "Lwt_log_js", "console" -> return (Some (mk_console ~state))
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
          | ("Lwt_log_core", "section"), [] | ("Lwt_log_core", "t"), [] ->
              (* Type [Lwt_log.Section.t] is detected as ["Lwt_log_core",
                 "t"] *)
              Some (mk_typ_constr [ "Logs"; "src" ])
          | ("Lwt_log_core", "level"), [] ->
              Some (mk_typ_constr [ "Logs"; "level" ])
          | ("Lwt_log_core", "logger"), [] ->
              Some (mk_typ_constr [ "Logs"; "reporter" ])
          | ("Lwt_log_core", "template"), [] ->
              add_comment state "Templates are no longer supported";
              Some (mk_typ_constr [ "string" ])
          | _ -> None)
  | _ -> None

let rewrite_pat ~state pat =
  let mk_level cstr =
    Some (Pat.construct (mk_longident [ "Logs"; cstr ]) None)
  in
  match pat.ppat_desc with
  | Ppat_construct (lid, arg) ->
      Occ.may_rewrite state lid (fun (unit, ident) ->
          match unit with
          | "Lwt_log_core" -> (
              match (ident, arg) with
              | "Debug", None -> mk_level "Debug"
              | "Info", None -> mk_level "Info"
              | "Notice", None -> mk_level "App"
              | "Warning", None -> mk_level "Warning"
              | "Error", None -> mk_level "Error"
              | "Fatal", None -> mk_level "Error"
              | _ -> None)
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
  let pat m x =
    call_rewrite ~default:default.pat ~loc:x.ppat_loc (rewrite_pat ~state) m x
  in
  { default with expr; typ; pat }

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
    | "Lwt_log" | "Lwt_daemon" | "Lwt_log_core" | "Lwt_log_rules" | "Lwt_log_js"
      ->
        true
    | _ -> false
  in
  let packages = [ "lwt_log"; "lwt_log.core"; "js_of_ocaml-lwt.logger" ] in
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
