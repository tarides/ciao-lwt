  $ chmod a+w *.ml
  $ dune build @ocaml-index
  $ lwt-log-to-logs --migrate
  Formatted 1 files
  Warning: foo.ml: 8 occurrences have not been rewritten.
    Lwt_log_core.null (line 51 column 35)
    Lwt_log_core.null (line 56 column 41)
    Lwt_log_core.null (line 65 column 34)
    Lwt_log_core.default (line 81 column 11)
    Lwt_log_core.close (line 93 column 11)
    Lwt_log_core.close (line 94 column 11)
    Lwt_log_core.add_rule (line 95 column 11)
    Lwt_log_core.close (line 134 column 31)

  $ cat foo.ml
  let section : Logs.src = Logs.Src.create "test:section"
  
  let _lwt =
    Logs.msg Logs.App (fun fmt -> fmt "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.msg Logs.App (fun fmt -> fmt "log");
    Lwt.return_unit
  
  let () = Logs.msg Logs.App (fun fmt -> fmt "log")
  let () = Logs.msg Logs.App (fun fmt -> fmt "log")
  
  (* String log lwt *)
  let _lwt =
    Logs.debug ~src:section (fun fmt -> fmt "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.info ~src:section (fun fmt -> fmt "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.app ~src:section (fun fmt -> fmt "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.warn ~src:section (fun fmt -> fmt "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.err ~src:section (fun fmt -> fmt "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.err
      ~src:
        (* TODO: lwt-log-to-logs: This message was previously on the [fatal] level. *)
        section (fun fmt -> fmt "log");
    Lwt.return_unit
  
  (* Format log lwt *)
  let _lwt =
    Logs.debug ~src:section (fun fmt -> fmt "%s" "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.info ~src:section (fun fmt -> fmt "%s" "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.app ~src:section (fun fmt -> fmt "%s" "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.warn ~src:section (fun fmt -> fmt "%s" "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.err ~src:section (fun fmt -> fmt "%s" "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.err
      ~src:
        (* TODO: lwt-log-to-logs: This message was previously on the [fatal] level. *)
        section (fun fmt -> fmt "%s" "log");
    Lwt.return_unit
  
  (* String log async *)
  let () = Logs.debug ~src:section (fun fmt -> fmt "log")
  let () = Logs.info ~src:section (fun fmt -> fmt "log")
  let () = Logs.app ~src:section (fun fmt -> fmt "log")
  let () = Logs.warn ~src:section (fun fmt -> fmt "log")
  let () = Logs.err ~src:section (fun fmt -> fmt "log")
  
  let () =
    Logs.err
      ~src:
        (* TODO: lwt-log-to-logs: This message was previously on the [fatal] level. *)
        section (fun fmt -> fmt "log")
  
  (* Format log async *)
  let () = Logs.debug ~src:section (fun fmt -> fmt "%s" "log")
  let () = Logs.info ~src:section (fun fmt -> fmt "%s" "log")
  let () = Logs.app ~src:section (fun fmt -> fmt "%s" "log")
  let () = Logs.warn ~src:section (fun fmt -> fmt "%s" "log")
  let () = Logs.err ~src:section (fun fmt -> fmt "%s" "log")
  
  let () =
    Logs.err
      ~src:
        (* TODO: lwt-log-to-logs: This message was previously on the [fatal] level. *)
        section (fun fmt -> fmt "%s" "log")
  
  (* Format string *)
  let () = Logs.info (fun fmt -> fmt "%s" "@")
  let () = Logs.info (fun fmt -> fmt "%s" "%")
  
  let () =
    let s = "foo" in
    Logs.info (fun fmt -> fmt "%s" s)
  
  (* Other arguments *)
  let () =
    Logs.info (fun fmt -> fmt ("exn" ^^ "@\n%s") (Printexc.to_string Not_found))
  
  let () =
    Logs.info (fun fmt ->
        fmt
          (* TODO: lwt-log-to-logs: Labelled argument ~location was dropped. *)
          "location")
  
  let () =
    Logs.info (fun fmt ->
        fmt
          (* TODO: lwt-log-to-logs: Labelled argument ~logger was dropped. *)
          "logger")
  
  (* Other arguments as opt labels *)
  let () =
    Logs.info (fun fmt ->
        fmt ("exn" ^^ "@\n%s")
          (Printexc.to_string
             (* TODO: lwt-log-to-logs: Last argument is a [exception option] while [exception] is expected. *)
             (Some Not_found)))
  
  let () =
    Logs.info (fun fmt ->
        fmt
          (* TODO: lwt-log-to-logs: Labelled argument ?location was dropped. *)
          "location")
  
  let () =
    Logs.info (fun fmt ->
        fmt
          (* TODO: lwt-log-to-logs: Labelled argument ?logger was dropped. *)
          "logger")
  
  (* Partially applied *)
  [@@@warning "-5"]
  
  let _
      (* TODO: lwt-log-to-logs: Last argument is a [exception option] while [exception] is expected. *)
      (* TODO: lwt-log-to-logs: Labelled argument ?logger was dropped. *)
      (* TODO: lwt-log-to-logs: Labelled argument ?location was dropped. *)
      (* TODO: lwt-log-to-logs: Labelled argument ?inspect was dropped. *) =
   fun ?inspect:x1 ?section:x2 ?exn:x3 ?location:x4 ?logger:x5 x6 ->
    Logs.info ?src:x2 (fun fmt ->
        fmt ("%s" ^^ "@\n%s") x6 (Printexc.to_string x3))
  
  let _ =
   fun ?exn:x1 ?location:x2 ?logger:x3 x4 ->
    Logs.info
      ~src:
        (* TODO: lwt-log-to-logs: Last argument is a [exception option] while [exception] is expected. *)
        (* TODO: lwt-log-to-logs: Labelled argument ?logger was dropped. *)
        (* TODO: lwt-log-to-logs: Labelled argument ?location was dropped. *)
        section (fun fmt -> fmt ("%s" ^^ "@\n%s") x4 (Printexc.to_string x1))
  
  let _ =
   fun ?location:x1 ?logger:x2 x3 ->
    Logs.info (fun fmt ->
        fmt ("%s" ^^ "@\n%s") x3
          (Printexc.to_string
             (* TODO: lwt-log-to-logs: Labelled argument ?logger was dropped. *)
             (* TODO: lwt-log-to-logs: Labelled argument ?location was dropped. *)
             Not_found))
  
  let _
      (* TODO: lwt-log-to-logs: Labelled argument ~location was dropped. *)
      (* TODO: lwt-log-to-logs: Labelled argument ?logger was dropped. *) =
   fun ?logger:x1 x2 -> Logs.info (fun fmt -> fmt "%s" x2)
  
  let _ (* TODO: lwt-log-to-logs: Labelled argument ~logger was dropped. *) =
   fun x1 -> Logs.info (fun fmt -> fmt "%s" x1)
  
  [@@@warning "+5"]
  
  let () =
    Logs.Src.set_level section (Some Logs.Debug);
    Logs.Src.set_level section (Some Logs.Info);
    Logs.Src.set_level section (Some Logs.App);
    Logs.Src.set_level section (Some Logs.Warning);
    Logs.Src.set_level section (Some Logs.Error);
    Logs.Src.set_level section (Some Logs.Error);
    Logs.Src.set_level section None;
    Logs.set_reporter
      (let logs_formatter =
         Format.formatter_of_out_channel
           (* TODO: lwt-log-to-logs: Format.formatter_of_out_channel: Argument is a [Lwt_io.output_channel] but a [out_channel] is expected. *)
           Lwt_io.stderr
       in
       Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ())
  
  let () =
    let _ : Logs.reporter = Logs.reporter () in
    let _ =
      Lwt_log.default
      (* TODO: lwt-log-to-logs: Use [Logs.set_reporter : reporter -> unit]. *)
    in
    let _ = Logs.nop_reporter in
    let _ =
      let logs_formatter =
        Format.formatter_of_out_channel
          (* TODO: lwt-log-to-logs: Lwt_log.channel: The [~template] argument is unsupported. Use [~pp_header] instead. *)
          (* TODO: lwt-log-to-logs: Format.formatter_of_out_channel: Argument is a [Lwt_io.output_channel] but a [out_channel] is expected. *)
          Lwt_io.stdout
      in
      Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ()
    in
    let keep = `Keep in
    let _ =
      let logs_formatter =
        Format.formatter_of_out_channel
          (* TODO: lwt-log-to-logs: Format.formatter_of_out_channel: Argument is a [Lwt_io.output_channel] but a [out_channel] is expected. *)
          Lwt_io.stdout
      in
      Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ()
    in
    let _ =
      let logs_formatter =
        Format.formatter_of_out_channel
          (* TODO: lwt-log-to-logs: Lwt_log.channel: The [~close_mode] argument has been dropped. The behavior is always [`Keep]. *)
          (* TODO: lwt-log-to-logs: Format.formatter_of_out_channel: Argument is a [Lwt_io.output_channel] but a [out_channel] is expected. *)
          Lwt_io.stdout
      in
      Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ()
    in
    let _ =
      let logs_formatter =
        Format.formatter_of_out_channel
          (* TODO: lwt-log-to-logs: Lwt_log.channel: The [~close_mode] argument has been dropped. The behavior is always [`Keep]. *)
          (* TODO: lwt-log-to-logs: Format.formatter_of_out_channel: Argument is a [Lwt_io.output_channel] but a [out_channel] is expected. *)
          Lwt_io.stdout
      in
      Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ()
    in
    ()
  
  let () =
    let _ =
      Lwt_log.close
        (* TODO: lwt-log-to-logs: close is no longer supported. *)
        (* TODO: lwt-log-to-logs: close is no longer supported. *)
        (Logs.reporter ())
    in
    let _ =
      Lwt_log.close (* TODO: lwt-log-to-logs: close is no longer supported. *)
    in
    let _ =
      Lwt_log.add_rule
      (* TODO: lwt-log-to-logs: add_rule is no longer supported. *)
    in
    let _
        (* TODO: lwt-log-to-logs: [Logs_syslog_unix.unix_reporter] take a single path but a list is passed. *)
        (* TODO: lwt-log-to-logs: The [~facility] argument is not of the right type. *)
        (* TODO: lwt-log-to-logs: Labelled argument ?template was dropped. *)
        (* TODO: lwt-log-to-logs: Add dependency on library [logs-syslog.unix] and package [logs-syslog]. *)
        =
     fun ?template:x1 ?paths:x2 ~facility:x3 x4 ->
      Logs_syslog_unix.unix_reporter ?socket:x2 ~facility:x3 ()
    in
    let _
        (* TODO: lwt-log-to-logs: [file]: Channel is never closed. *)
        (* TODO: lwt-log-to-logs: Labelled argument ?template was dropped. *) =
     fun ?template:x1 ?mode:x2 ?perm:x3 ~file_name:x4 x5 ->
      let logs_formatter =
        Format.formatter_of_out_channel
          (open_out_gen
             (let append_mode =
                match match x2 with Some x -> x | None -> `Append with
                | `Append -> Open_append
                | `Truncate -> Open_trunc
              in
              [ append_mode; Open_wronly; Open_creat; Open_text ])
             (match x3 with Some x -> x | None -> 0o640)
             x4)
      in
      Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ()
    in
    ()
  
  let () =
    let _ =
      let logs_formatter =
        Format.formatter_of_out_channel
          (open_out_gen
             (let append_mode =
                match `Append with
                | `Append -> Open_append
                | `Truncate -> Open_trunc
              in
              [ append_mode; Open_wronly; Open_creat; Open_text ])
             0o640
             (* TODO: lwt-log-to-logs: [file]: Channel is never closed. *)
             (* TODO: lwt-log-to-logs: Labelled argument ~template was dropped. *)
             "")
      in
      Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ()
    in
    let _ =
      let logs_formatter =
        Format.formatter_of_out_channel
          (open_out_gen
             (let append_mode =
                match
                  (* TODO: lwt-log-to-logs: [file]: Channel is never closed. *)
                  `Truncate
                with
                | `Append -> Open_append
                | `Truncate -> Open_trunc
              in
              [ append_mode; Open_wronly; Open_creat; Open_text ])
             0o640 "")
      in
      Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ()
    in
    let _ =
      let mode = `Append in
      let logs_formatter =
        Format.formatter_of_out_channel
          (open_out_gen
             (let append_mode =
                match
                  (* TODO: lwt-log-to-logs: [file]: Channel is never closed. *)
                  mode
                with
                | `Append -> Open_append
                | `Truncate -> Open_trunc
              in
              [ append_mode; Open_wronly; Open_creat; Open_text ])
             0o640 "")
      in
      Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ()
    in
    let _ =
      let mode = Some `Append in
      let logs_formatter =
        Format.formatter_of_out_channel
          (open_out_gen
             (let append_mode =
                match
                  match
                    (* TODO: lwt-log-to-logs: [file]: Channel is never closed. *)
                    mode
                  with
                  | Some x -> x
                  | None -> `Append
                with
                | `Append -> Open_append
                | `Truncate -> Open_trunc
              in
              [ append_mode; Open_wronly; Open_creat; Open_text ])
             0o640 "")
      in
      Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ()
    in
    let _ =
      let logs_formatter =
        Format.formatter_of_out_channel
          (open_out_gen
             (let append_mode =
                match `Append with
                | `Append -> Open_append
                | `Truncate -> Open_trunc
              in
              [ append_mode; Open_wronly; Open_creat; Open_text ])
             (* TODO: lwt-log-to-logs: [file]: Channel is never closed. *) 1 "")
      in
      Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ()
    in
    let _ =
      let perm = 1 in
      let logs_formatter =
        Format.formatter_of_out_channel
          (open_out_gen
             (let append_mode =
                match `Append with
                | `Append -> Open_append
                | `Truncate -> Open_trunc
              in
              [ append_mode; Open_wronly; Open_creat; Open_text ])
             (* TODO: lwt-log-to-logs: [file]: Channel is never closed. *)
             perm "")
      in
      Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ()
    in
    let _ =
      let perm = Some 1 in
      let logs_formatter =
        Format.formatter_of_out_channel
          (open_out_gen
             (let append_mode =
                match `Append with
                | `Append -> Open_append
                | `Truncate -> Open_trunc
              in
              [ append_mode; Open_wronly; Open_creat; Open_text ])
             (match
                (* TODO: lwt-log-to-logs: [file]: Channel is never closed. *)
                perm
              with
             | Some x -> x
             | None -> 0o640)
             "")
      in
      Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ()
    in
    let _ =
      let logs_formatter =
        Format.formatter_of_out_channel
          (open_out_gen
             (let append_mode =
                match `Append with
                | `Append -> Open_append
                | `Truncate -> Open_trunc
              in
              [ append_mode; Open_wronly; Open_creat; Open_text ])
             0o640
             (* TODO: lwt-log-to-logs: [file]: Channel is never closed. *) "")
      in
      Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ()
    in
    ()
  
  let _open_files () =
    (* Extracted from ocsigenserver's [src/server/ocsigen_messages.ml]. *)
    let open Lwt.Infix in
    let access_file = "access.log" in
    let warning_file = "warnings.log" in
    let error_file = "errors.log" in
    let access_logger = ref Logs.nop_reporter in
    let stderr =
      let logs_formatter =
        Format.formatter_of_out_channel
          (* TODO: lwt-log-to-logs: Format.formatter_of_out_channel: Argument is a [Lwt_io.output_channel] but a [out_channel] is expected. *)
          Lwt_io.stderr
      in
      Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ()
    in
    let stdout =
      let logs_formatter =
        Format.formatter_of_out_channel
          (* TODO: lwt-log-to-logs: Format.formatter_of_out_channel: Argument is a [Lwt_io.output_channel] but a [out_channel] is expected. *)
          Lwt_io.stdout
      in
      Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ()
    in
    let loggers = ref [] in
    (* CHECK: we are closing asynchronously! That should be ok, though. *)
    List.iter
      (fun l ->
        ignore
          (Lwt_log.close
             (* TODO: lwt-log-to-logs: close is no longer supported. *)
             (* TODO: lwt-log-to-logs: close is no longer supported. *)
             l
            : unit Lwt.t))
      !loggers;
    match None with
    | Some facility ->
        (* log to syslog *)
        let syslog =
          Logs_syslog_unix.unix_reporter
          (* TODO: lwt-log-to-logs: The [~facility] argument is not of the right type. *)
          (* TODO: lwt-log-to-logs: Add dependency on library [logs-syslog.unix] and package [logs-syslog]. *)
            ~facility ()
        in
        loggers := [ syslog ];
        Logs.set_reporter
          (let broadcast_reporters = [ syslog; stderr ] in
           {
             Logs.report =
               (fun src level ~over k msgf ->
                 List.fold_left
                   (fun k r _unit -> r.Logs.report src level ~over k msgf)
                   k broadcast_reporters ());
           });
        Lwt.return ()
    | None ->
        (* log to files *)
        let open_log path =
          let logs_formatter =
            Format.formatter_of_out_channel
              (open_out_gen
                 (let append_mode =
                    match `Append with
                    | `Append -> Open_append
                    | `Truncate -> Open_trunc
                  in
                  [ append_mode; Open_wronly; Open_creat; Open_text ])
                 0o640
                 (* TODO: lwt-log-to-logs: [file]: Channel is never closed. *)
                 path)
          in
          Logs.format_reporter ~app:logs_formatter ~dst:logs_formatter ()
        in
        open_log access_file >>= fun acc ->
        access_logger := acc;
        open_log warning_file >>= fun war ->
        open_log error_file >>= fun err ->
        loggers := [ acc; war; err ];
        Logs.set_reporter
          (let broadcast_reporters =
             [
               (let dispatch_f =
                 fun _sect (lev : Logs.level) ->
                  match lev with
                  | Logs.Error | Logs.Error -> err
                  | Logs.Warning -> war
                  | _ -> Logs.nop_reporter
                in
                {
                  Logs.report =
                    (fun src level ~over k msgf ->
                      (dispatch_f src level).Logs.report src level ~over k msgf);
                });
               (let dispatch_f =
                 fun _sect lev ->
                  if false then Logs.nop_reporter
                  else
                    match lev with
                    | Logs.Warning | Logs.Error | Logs.Error -> stderr
                    | _ -> stdout
                in
                {
                  Logs.report =
                    (fun src level ~over k msgf ->
                      (dispatch_f src level).Logs.report src level ~over k msgf);
                });
             ]
           in
           {
             Logs.report =
               (fun src level ~over k msgf ->
                 List.fold_left
                   (fun k r _unit -> r.Logs.report src level ~over k msgf)
                   k broadcast_reporters ());
           });
        Lwt.return ()
  
  let _ : string =
    (* TODO: lwt-log-to-logs: Templates are no longer supported *) ""
  
  let _ : Logs.src = Logs.Src.create "test:section"
