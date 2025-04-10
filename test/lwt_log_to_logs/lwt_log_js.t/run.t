  $ chmod a+w *.ml
  $ dune build @ocaml-index
  $ lwt-log-to-logs --migrate
  Formatted 1 files, 0 errors

  $ cat test.ml
  let () = Logs.set_reporter (Logs_browser.console_reporter ())
  
  (* All logging functions *)
  let _lwt =
    Logs.debug (fun fmt -> fmt "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.info (fun fmt -> fmt "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.app (fun fmt -> fmt "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.warn (fun fmt -> fmt "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.err (fun fmt -> fmt "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.err (fun fmt ->
        fmt
          (* TODO: lwt-log-to-logs: This message was previously on the [fatal] level. *)
          "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.debug (fun fmt -> fmt "%s" "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.info (fun fmt -> fmt "%s" "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.app (fun fmt -> fmt "%s" "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.warn (fun fmt -> fmt "%s" "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.err (fun fmt -> fmt "%s" "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.err (fun fmt ->
        fmt
          (* TODO: lwt-log-to-logs: This message was previously on the [fatal] level. *)
          "%s"
          "log");
    Lwt.return_unit
  
  let () = Logs.debug (fun fmt -> fmt "log")
  let () = Logs.info (fun fmt -> fmt "log")
  let () = Logs.app (fun fmt -> fmt "log")
  let () = Logs.warn (fun fmt -> fmt "log")
  let () = Logs.err (fun fmt -> fmt "log")
  
  let () =
    Logs.err (fun fmt ->
        fmt
          (* TODO: lwt-log-to-logs: This message was previously on the [fatal] level. *)
          "log")
  
  let () = Logs.debug (fun fmt -> fmt "%s" "log")
  let () = Logs.info (fun fmt -> fmt "%s" "log")
  let () = Logs.app (fun fmt -> fmt "%s" "log")
  let () = Logs.warn (fun fmt -> fmt "%s" "log")
  let () = Logs.err (fun fmt -> fmt "%s" "log")
  
  let () =
    Logs.err (fun fmt ->
        fmt
          (* TODO: lwt-log-to-logs: This message was previously on the [fatal] level. *)
          "%s"
          "log")
