let section : Lwt_log.section = Lwt_log.Section.make "test:section"

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
let () = Lwt_log.ign_info ~section "log"
let () = Lwt_log.ign_notice ~section "log"
let () = Lwt_log.ign_warning ~section "log"
let () = Lwt_log.ign_error ~section "log"
let () = Lwt_log.ign_fatal ~section "log"

(* Format log async *)
let () = Lwt_log.ign_debug_f ~section "%s" "log"
let () = Lwt_log.ign_info_f ~section "%s" "log"
let () = Lwt_log.ign_notice_f ~section "%s" "log"
let () = Lwt_log.ign_warning_f ~section "%s" "log"
let () = Lwt_log.ign_error_f ~section "%s" "log"
let () = Lwt_log.ign_fatal_f ~section "%s" "log"

(* Other arguments *)
let () = Lwt_log.ign_info ~exn:Not_found "exn"
let () = Lwt_log.ign_info ~location:("here", 1, 2) "location"
let () = Lwt_log.ign_info ~logger:Lwt_log.null "logger"

(* Other arguments as opt labels *)
let () = Lwt_log.ign_info ?exn:(Some Not_found) "exn"
let () = Lwt_log.ign_info ?location:(Some ("here", 1, 2)) "location"
let () = Lwt_log.ign_info ?logger:(Some Lwt_log.null) "logger"

(* Partially applied *)
[@@@warning "-5"]

let _ = Lwt_log.ign_info
let _ = Lwt_log.ign_info ~section
let _ = Lwt_log.ign_info ~exn:Not_found
let _ = Lwt_log.ign_info ~location:("here", 1, 2)
let _ = Lwt_log.ign_info ~logger:Lwt_log.null

[@@@warning "+5"]

let () =
  Lwt_log.Section.set_level section Lwt_log.Debug;
  Lwt_log.Section.set_level section Lwt_log.Info;
  Lwt_log.Section.set_level section Lwt_log.Notice;
  Lwt_log.Section.set_level section Lwt_log.Warning;
  Lwt_log.Section.set_level section Lwt_log.Error;
  Lwt_log.Section.set_level section Lwt_log.Fatal;
  Lwt_log.Section.reset_level section;
  Lwt_log.default := Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stderr ()

let () =
  let _ = !Lwt_log.default in
  let _ = Lwt_log.null in
  let _ = Lwt_log.channel ~template:"foo" ~close_mode:`Keep ~channel:Lwt_io.stdout () in
  let keep = `Keep in
  let _ = Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout () in
  let _ = Lwt_log.channel ~close_mode:keep ~channel:Lwt_io.stdout () in
  let _ = Lwt_log.channel ~close_mode:`Close ~channel:Lwt_io.stdout () in
  ()
