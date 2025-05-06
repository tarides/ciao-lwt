let section : Lwt_log.section = Lwt_log.Section.make "test:section"

let _lwt = Lwt_log.log ~level:Notice "log"
let _lwt = Lwt_log.log_f ~level:Notice "log"
let () = Lwt_log.ign_log ~level:Notice "log"
let () = Lwt_log.ign_log_f ~level:Notice "log"

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

(* Format string *)
let () = Lwt_log.ign_info "@"
let () = Lwt_log.ign_info "%"

let () =
  let s = "foo" in
  Lwt_log.ign_info s

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
  let _ : Lwt_log.logger = !Lwt_log.default in
  let _ = Lwt_log.default in
  let _ = Lwt_log.null in
  let _ =
    Lwt_log.channel ~template:"foo" ~close_mode:`Keep ~channel:Lwt_io.stdout ()
  in
  let keep = `Keep in
  let _ = Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout () in
  let _ = Lwt_log.channel ~close_mode:keep ~channel:Lwt_io.stdout () in
  let _ = Lwt_log.channel ~close_mode:`Close ~channel:Lwt_io.stdout () in
  ()

let () =
  let _ = Lwt_log.close !Lwt_log.default in
  let _ = Lwt_log.close in
  let _ = Lwt_log.add_rule in
  let _ = Lwt_log.syslog in
  let _ = Lwt_log.file in
  ()

let () =
  let _ = Lwt_log.file ~template:"" ~file_name:"" () in
  let _ = Lwt_log.file ~mode:`Truncate ~file_name:"" () in
  let _ =
    let mode = `Append in
    Lwt_log.file ~mode ~file_name:"" ()
  in
  let _ =
    let mode = Some `Append in
    Lwt_log.file ?mode ~file_name:"" ()
  in
  let _ = Lwt_log.file ~perm:1 ~file_name:"" () in
  let _ =
    let perm = 1 in
    Lwt_log.file ~perm ~file_name:"" ()
  in
  let _ =
    let perm = Some 1 in
    Lwt_log.file ?perm ~file_name:"" ()
  in
  let _ = Lwt_log.file ~file_name:"" () in
  ()

let _open_files () =
  (* Extracted from ocsigenserver's [src/server/ocsigen_messages.ml]. *)
  let open Lwt.Infix in
  let access_file = "access.log" in
  let warning_file = "warnings.log" in
  let error_file = "errors.log" in
  let access_logger = ref Lwt_log_core.null in
  let stderr = Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stderr () in
  let stdout = Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout () in
  let loggers = ref [] in
  (* CHECK: we are closing asynchronously! That should be ok, though. *)
  List.iter (fun l -> ignore (Lwt_log.close l : unit Lwt.t)) !loggers;
  match None with
  | Some facility ->
      (* log to syslog *)
      let syslog = Lwt_log.syslog ~facility () in
      loggers := [ syslog ];
      Lwt_log.default := Lwt_log.broadcast [ syslog; stderr ];
      Lwt.return ()
  | None ->
      (* log to files *)
      let open_log path = Lwt_log.file ~file_name:path () in
      open_log access_file >>= fun acc ->
      access_logger := acc;
      open_log warning_file >>= fun war ->
      open_log error_file >>= fun err ->
      loggers := [ acc; war; err ];
      Lwt_log.default :=
        Lwt_log.broadcast
          [
            Lwt_log.dispatch (fun _sect (lev : Lwt_log.level) ->
                match lev with
                | Lwt_log.Error | Lwt_log.Fatal -> err
                | Lwt_log.Warning -> war
                | _ -> Lwt_log.null);
            Lwt_log.dispatch (fun _sect lev ->
                if false then Lwt_log.null
                else
                  match lev with
                  | Lwt_log.Warning | Lwt_log.Error | Lwt_log.Fatal -> stderr
                  | _ -> stdout);
          ];
      Lwt.return ()

let _ : Lwt_log.template = ""
let _ : Lwt_log.Section.t = Lwt_log.Section.make "test:section"
