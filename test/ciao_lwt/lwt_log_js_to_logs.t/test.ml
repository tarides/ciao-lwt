let () = Lwt_log.default := Lwt_log_js.console
let _lwt = Lwt_log_js.log ~level:Notice "log"
let _lwt = Lwt_log_js.log_f ~level:Notice "log"
let () = Lwt_log_js.ign_log ~level:Notice "log"
let () = Lwt_log_js.ign_log_f ~level:Notice "log"

(* All logging functions *)
let _lwt = Lwt_log_js.debug "log"
let _lwt = Lwt_log_js.info "log"
let _lwt = Lwt_log_js.notice "log"
let _lwt = Lwt_log_js.warning "log"
let _lwt = Lwt_log_js.error "log"
let _lwt = Lwt_log_js.fatal "log"
let _lwt = Lwt_log_js.debug_f "%s" "log"
let _lwt = Lwt_log_js.info_f "%s" "log"
let _lwt = Lwt_log_js.notice_f "%s" "log"
let _lwt = Lwt_log_js.warning_f "%s" "log"
let _lwt = Lwt_log_js.error_f "%s" "log"
let _lwt = Lwt_log_js.fatal_f "%s" "log"
let () = Lwt_log_js.ign_debug "log"
let () = Lwt_log_js.ign_info "log"
let () = Lwt_log_js.ign_notice "log"
let () = Lwt_log_js.ign_warning "log"
let () = Lwt_log_js.ign_error "log"
let () = Lwt_log_js.ign_fatal "log"
let () = Lwt_log_js.ign_debug_f "%s" "log"
let () = Lwt_log_js.ign_info_f "%s" "log"
let () = Lwt_log_js.ign_notice_f "%s" "log"
let () = Lwt_log_js.ign_warning_f "%s" "log"
let () = Lwt_log_js.ign_error_f "%s" "log"
let () = Lwt_log_js.ign_fatal_f "%s" "log"
let _ = Lwt_log_js.ign_notice ~inspect:42 "log"

let _f () :
    Lwt_log_js.section
    * Lwt_log_js.Section.t
    * Lwt_log_js.logger
    * Lwt_log_js.template =
  assert false

let _f = function
  | Lwt_log_js.Debug -> ()
  | Lwt_log_js.Info -> ()
  | Lwt_log_js.Notice -> ()
  | Lwt_log_js.Warning -> ()
  | Lwt_log_js.Error -> ()
  | Lwt_log_js.Fatal -> ()
