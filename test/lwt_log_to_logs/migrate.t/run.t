  $ chmod a+w *.ml
  $ dune build @ocaml-index
  $ lwt-log-to-logs --migrate
  Warning: foo.ml: 19 occurrences have not been rewritten.
    Lwt_log_core.make (line 1 column 15)
    Lwt_log_core.debug (line 4 column 12)
    Lwt_log_core.info (line 5 column 12)
    Lwt_log_core.notice (line 6 column 12)
    Lwt_log_core.warning (line 7 column 12)
    Lwt_log_core.error (line 8 column 12)
    Lwt_log_core.fatal (line 9 column 12)
    Lwt_log_core.debug_f (line 12 column 12)
    Lwt_log_core.info_f (line 13 column 12)
    Lwt_log_core.notice_f (line 14 column 12)
    Lwt_log_core.warning_f (line 15 column 12)
    Lwt_log_core.error_f (line 16 column 12)
    Lwt_log_core.fatal_f (line 17 column 12)
    Lwt_log_core.null (line 38 column 35)
    Lwt_log_core.null (line 43 column 41)
    Lwt_log_core.ign_info (line 48 column 9)
    Lwt_log_core.null (line 52 column 34)
    Lwt_log_core.default (line 57 column 3)
    Lwt_log.channel (line 57 column 22)
  Formatted 1 files, 0 errors

  $ cat foo.ml
  let section = Lwt_log.Section.make "test:section"
  
  (* String log lwt *)
  let _lwt = Lwt_log.debug ~section "log"
  let _lwt = Lwt_log.info ~section "log"
  let _lwt = Lwt_log.notice ~section "log"
  let _lwt = Lwt_log.warning ~section "log"
  let _lwt = Lwt_log.error ~section "log"
  let _lwt = Lwt_log.fatal ~section "log"
  
  (* Format log lwt *)
  let _lwt = Lwt_log.debug_f ~section "%s" "log"
  let _lwt = Lwt_log.info_f ~section "%s" "log"
  let _lwt = Lwt_log.notice_f ~section "%s" "log"
  let _lwt = Lwt_log.warning_f ~section "%s" "log"
  let _lwt = Lwt_log.error_f ~section "%s" "log"
  let _lwt = Lwt_log.fatal_f ~section "%s" "log"
  
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
  
  let _ = Lwt_log.ign_info
  
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
    Lwt_log.default := Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stderr ()
