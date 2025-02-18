open Lwt.Syntax

let _main () =
  Lwt.try_bind
    (fun () ->
      let* () = Lwt_fmt.printf "Main.main" in
      let+ () = Test.test () in
      ())
    (fun () -> Lwt.return ())
    (function
      | Failure msg -> Lwt_fmt.printf "Failure: %s\n%!" msg
      | exc -> Lwt.reraise exc)

let main () =
  let main () =
    Lwt.try_bind (fun () -> Lwt.return_none) Lwt.return Lwt.reraise
  in
  Lwt.try_bind main
    (function Some _ -> Lwt.return () | None -> Lwt.return_unit)
    (fun _ -> Lwt.return ())

let () = Lwt_main.run (main ())

let _ = Lwt.return_none
let _ = Lwt.return_nil
let _ = Lwt.return_true
let _ = Lwt.return_false
let _ = Lwt.return_ok ()
let _ = Lwt.return_error ()
