  $ chmod a+w *.ml
  $ dune build @ocaml-index
  $ lwt-log-to-logs --migrate
  Formatted 1 files

  $ cat test.ml
  let () = Logs.set_reporter (Logs_browser.console_reporter ())
  
  let _lwt =
    Logs.msg Logs.App (fun fmt -> fmt "log");
    Lwt.return_unit
  
  let _lwt =
    Logs.msg Logs.App (fun fmt -> fmt "log");
    Lwt.return_unit
  
  let () = Logs.msg Logs.App (fun fmt -> fmt "log")
  let () = Logs.msg Logs.App (fun fmt -> fmt "log")
  
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
  
  let _ =
    Logs.app (fun fmt ->
        fmt
          (* TODO: lwt-log-to-logs: Labelled argument ~inspect was dropped. *)
          "log")
  
  let _f () : Logs.src * Logs.src * Logs.reporter * string
      (* TODO: lwt-log-to-logs: Templates are no longer supported *) =
    assert false
  
  let _f = function
    | Logs.Debug -> ()
    | Logs.Info -> ()
    | Logs.App -> ()
    | Logs.Warning -> ()
    | Logs.Error -> ()
    | Logs.Error -> ()
