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
let _ = Lwt_fmt.printf ""
let _ = Lwt_fmt.eprintf ""
let _ = Lwt_unix.sleep 1.0

let x =
  Lwt.try_bind
    (fun () -> Lwt_unix.with_timeout 1.0 (fun () -> Lwt.return 42))
    Lwt.return
    (function Lwt_unix.Timeout -> Lwt.return 0 | x -> Lwt.reraise x)

open Lwt

let _ = x >>= function 0 -> Lwt.return_true | _ -> Lwt.return_false

let _ =
  print_endline "Hello";
  Lwt.return ()

let _ =
  print_endline "Hello";
  Lwt.return_unit

let _ =
  print_endline "Hello";
  Lwt.return 1

let _ =
  print_endline "Hello";
  Lwt.return (() [@foo])
