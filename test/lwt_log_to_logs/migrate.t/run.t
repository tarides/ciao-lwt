  $ chmod a+w *.ml
  $ dune build @ocaml-index
  $ lwt-log-to-logs --migrate
  Warning: foo.ml: 6 occurrences have not been rewritten.
    Lwt_log_core.null (line 38 column 35)
    Lwt_log_core.null (line 43 column 41)
    Lwt_log_core.ign_info (line 48 column 9)
    Lwt_log_core.null (line 52 column 34)
    Lwt_log_core.default (line 64 column 3)
    Lwt_log.channel (line 64 column 22)
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
    Logs.Src.set_level section (Some Logs.Debug);
    Logs.Src.set_level section (Some Logs.Info);
    Logs.Src.set_level section (Some Logs.App);
    Logs.Src.set_level section (Some Logs.Warning);
    Logs.Src.set_level section (Some Logs.Error);
    Logs.Src.set_level section (Some Logs.Error);
    Logs.Src.set_level section None;
    Lwt_log.default := Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stderr ()
