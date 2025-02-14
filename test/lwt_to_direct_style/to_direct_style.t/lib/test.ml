open Lwt.Infix
open Lwt.Syntax

let lwt_calls () =
  Lwt.try_bind
    (fun () ->
      Lwt.bind (Lwt_fmt.printf "1") (fun () ->
          Lwt.map (fun () -> `Ok) (Lwt_fmt.printf "2")))
    (fun `Ok -> Lwt.bind (Lwt_fmt.printf "3") (fun () -> Lwt.return ()))
    (fun _ -> Lwt.return ())

let lwt_calls_point_free () =
  Lwt.bind (Lwt_fmt.printf "1") @@ fun () ->
  Lwt_fmt.printf "2" |> Lwt.map @@ fun () -> ()

let letops () =
  let* `Ok =
    let* () = Lwt_fmt.printf "1" in
    let+ () = Lwt_fmt.printf "2" in
    `Ok
  and* () = Lwt_fmt.printf "3" in
  Lwt.return ()

let infix () =
  Lwt_fmt.printf "1"
  >>= (fun () -> Lwt_fmt.printf "2" >|= fun () -> ())
  <&> (Lwt_fmt.printf "3" >>= fun () -> Lwt.return ())

let test () =
  Lwt_fmt.printf "Test.test" >>= fun () ->
  Lwt.both (lwt_calls ()) (lwt_calls_point_free ()) >>= fun _ ->
  Lwt.join [ (letops ()); (infix ()) ] >>= Lwt.return
