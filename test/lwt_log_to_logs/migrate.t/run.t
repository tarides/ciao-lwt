  $ chmod a+w *.ml
  $ dune build @ocaml-index
  $ lwt-log-to-logs --migrate
  Warning: foo.ml: 15 occurrences have not been rewritten.
    Lwt_log_core.null (line 38 column 35)
    Lwt_log_core.null (line 43 column 41)
    Lwt_log_core.null (line 52 column 34)
    Lwt_log_core.default (line 68 column 11)
    Lwt_log_core.close (line 88 column 31)
    Lwt_log.syslog (line 92 column 20)
    Lwt_log.file (line 98 column 27)
    Lwt_log_core.dispatch (line 107 column 13)
    Lwt_log_core.Error (line 109 column 19)
    Lwt_log_core.Fatal (line 109 column 35)
    Lwt_log_core.Warning (line 110 column 19)
    Lwt_log_core.dispatch (line 112 column 13)
    Lwt_log_core.Warning (line 116 column 21)
    Lwt_log_core.Error (line 116 column 39)
    Lwt_log_core.Fatal (line 116 column 55)
  Formatted 1 files, 0 errors

  $ cat foo.ml
  let section : Logs.src = Logs.Src.create "test:section"
  
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
  
  (* Other arguments *)
  let () =
    Logs.info (fun fmt ->
        fmt
          (* TODO: lwt-log-to-logs: Labelled argument ~exn was dropped. *)
          "exn")
  
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
        fmt
          (* TODO: lwt-log-to-logs: Labelled argument ?exn was dropped. *)
          "exn")
  
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
      (* TODO: lwt-log-to-logs: Labelled argument ?logger was dropped. *)
      (* TODO: lwt-log-to-logs: Labelled argument ?location was dropped. *)
      (* TODO: lwt-log-to-logs: Labelled argument ?exn was dropped. *) =
   fun ?section:x1 ?exn:x2 ?location:x3 ?logger:x4 x5 ->
    Logs.info ?src:x1 (fun fmt -> fmt x5)
  
  let _ =
   fun ?exn:x1 ?location:x2 ?logger:x3 x4 ->
    Logs.info
      ~src:
        (* TODO: lwt-log-to-logs: Labelled argument ?logger was dropped. *)
        (* TODO: lwt-log-to-logs: Labelled argument ?location was dropped. *)
        (* TODO: lwt-log-to-logs: Labelled argument ?exn was dropped. *)
        section (fun fmt -> fmt x4)
  
  let _
      (* TODO: lwt-log-to-logs: Labelled argument ~exn was dropped. *)
      (* TODO: lwt-log-to-logs: Labelled argument ?logger was dropped. *)
      (* TODO: lwt-log-to-logs: Labelled argument ?location was dropped. *) =
   fun ?location:x1 ?logger:x2 x3 -> Logs.info (fun fmt -> fmt x3)
  
  let _
      (* TODO: lwt-log-to-logs: Labelled argument ~location was dropped. *)
      (* TODO: lwt-log-to-logs: Labelled argument ?logger was dropped. *) =
   fun ?logger:x1 x2 -> Logs.info (fun fmt -> fmt x2)
  
  let _ (* TODO: lwt-log-to-logs: Labelled argument ~logger was dropped. *) =
   fun x1 -> Logs.info (fun fmt -> fmt x1)
  
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
    let _ = Logs.reporter () in
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
    List.iter (fun l -> ignore (Lwt_log.close l : unit Lwt.t)) !loggers;
    match None with
    | Some facility ->
        (* log to syslog *)
        let syslog = Lwt_log.syslog ~facility () in
        loggers := [ syslog ];
        Logs.set_reporter
          (let broadcast_reporters = [ syslog; stderr ] in
           {
             Logs.report =
               (fun src level ~over k msgf ->
                 List.iter
                   (fun r -> r.Logs.report msgf k ~over level src)
                   broadcast_reporters);
           });
        Lwt.return ()
    | None ->
        (* log to files *)
        let open_log path = Lwt_log.file ~file_name:path () in
        open_log access_file >>= fun acc ->
        access_logger := acc;
        open_log warning_file >>= fun war ->
        open_log error_file >>= fun err ->
        loggers := [ acc; war; err ];
        Logs.set_reporter
          (let broadcast_reporters =
             [
               Lwt_log.dispatch (fun _sect lev ->
                   match lev with
                   | Lwt_log.Error | Lwt_log.Fatal -> err
                   | Lwt_log.Warning -> war
                   | _ -> Logs.nop_reporter);
               Lwt_log.dispatch (fun _sect lev ->
                   if false then Logs.nop_reporter
                   else
                     match lev with
                     | Lwt_log.Warning | Lwt_log.Error | Lwt_log.Fatal -> stderr
                     | _ -> stdout);
             ]
           in
           {
             Logs.report =
               (fun src level ~over k msgf ->
                 List.iter
                   (fun r -> r.Logs.report msgf k ~over level src)
                   broadcast_reporters);
           });
        Lwt.return ()
