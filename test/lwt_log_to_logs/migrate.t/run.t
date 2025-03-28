  $ chmod a+w *.ml
  $ dune build @ocaml-index
  $ lwt-log-to-logs --migrate
  Error: Unexpected label "exn" at (foo.ml[36,1241+26]..[36,1241+40])
  Error: Unexpected label "location" at (foo.ml[37,1288+26]..[37,1288+50])
  Error: Unexpected label "logger" at (foo.ml[38,1345+26]..[38,1345+46])
  Error: Unexpected label "exn" at (foo.ml[41,1435+26]..[41,1435+47])
  Error: Unexpected label "location" at (foo.ml[42,1489+26]..[42,1489+57])
  Error: Unexpected label "logger" at (foo.ml[43,1553+26]..[43,1553+53])
  Error: Unexpected label "exn" at (foo.ml[50,1716+25]..[50,1716+39])
  Error: Unexpected label "location" at (foo.ml[51,1756+25]..[51,1756+49])
  Error: Unexpected label "logger" at (foo.ml[52,1806+25]..[52,1806+45])
  Warning: foo.ml: 38 occurrences have not been rewritten.
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
    Lwt_log_core.ign_debug (line 20 column 10)
    Lwt_log_core.ign_notice (line 22 column 10)
    Lwt_log_core.ign_warning (line 23 column 10)
    Lwt_log_core.ign_error (line 24 column 10)
    Lwt_log_core.ign_fatal (line 25 column 10)
    Lwt_log_core.ign_debug_f (line 28 column 10)
    Lwt_log_core.ign_notice_f (line 30 column 10)
    Lwt_log_core.ign_warning_f (line 31 column 10)
    Lwt_log_core.ign_error_f (line 32 column 10)
    Lwt_log_core.ign_fatal_f (line 33 column 10)
    Lwt_log_core.ign_info (line 36 column 10)
    Lwt_log_core.ign_info (line 37 column 10)
    Lwt_log_core.ign_info (line 38 column 10)
    Lwt_log_core.null (line 38 column 35)
    Lwt_log_core.ign_info (line 41 column 10)
    Lwt_log_core.ign_info (line 42 column 10)
    Lwt_log_core.ign_info (line 43 column 10)
    Lwt_log_core.null (line 43 column 41)
    Lwt_log_core.ign_info (line 48 column 9)
    Lwt_log_core.ign_info (line 50 column 9)
    Lwt_log_core.ign_info (line 51 column 9)
    Lwt_log_core.ign_info (line 52 column 9)
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
  let () = Lwt_log.ign_debug ~section "log"
  let () = Logs.info ~src:section (fun fmt -> fmt "log")
  let () = Lwt_log.ign_notice ~section "log"
  let () = Lwt_log.ign_warning ~section "log"
  let () = Lwt_log.ign_error ~section "log"
  let () = Lwt_log.ign_fatal ~section "log"
  
  (* Format log async *)
  let () = Lwt_log.ign_debug_f ~section "%s" "log"
  let () = Logs.info ~src:section (fun fmt -> fmt "%s" "log")
  let () = Lwt_log.ign_notice_f ~section "%s" "log"
  let () = Lwt_log.ign_warning_f ~section "%s" "log"
  let () = Lwt_log.ign_error_f ~section "%s" "log"
  let () = Lwt_log.ign_fatal_f ~section "%s" "log"
  
  (* Other arguments *)
  let () = Lwt_log.ign_info ~exn:Not_found "log"
  let () = Lwt_log.ign_info ~location:("here", 1, 2) "log"
  let () = Lwt_log.ign_info ~logger:Lwt_log.null "log"
  
  (* Other arguments as opt labels *)
  let () = Lwt_log.ign_info ?exn:(Some Not_found) "log"
  let () = Lwt_log.ign_info ?location:(Some ("here", 1, 2)) "log"
  let () = Lwt_log.ign_info ?logger:(Some Lwt_log.null) "log"
  
  (* Partially applied *)
  [@@@warning "-5"]
  
  let _ = Lwt_log.ign_info
  let _ = fun x1 -> Logs.info ~src:section (fun fmt -> fmt x1)
  let _ = Lwt_log.ign_info ~exn:Not_found
  let _ = Lwt_log.ign_info ~location:("here", 1, 2)
  let _ = Lwt_log.ign_info ~logger:Lwt_log.null
  
  [@@@warning "+5"]
  
  let () =
    Lwt_log.default := Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stderr ()
