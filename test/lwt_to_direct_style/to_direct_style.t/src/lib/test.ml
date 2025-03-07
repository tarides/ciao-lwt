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
  let* `Ok =
    let* () = Lwt_fmt.printf "4" in
    let+ () = Lwt_fmt.printf "5" in
    `Ok
  and* () = Lwt_fmt.printf "6"
  and* () = Lwt_fmt.printf "7" in
  Lwt.return ()

let infix () =
  Lwt_fmt.printf "1"
  >>= (fun () -> Lwt_fmt.printf "2" >|= fun () -> ())
  <&> (Lwt_fmt.printf "3" >>= fun () -> Lwt.return ())

let lwt_calls_open () =
  let open Lwt in
  let open Lwt_fmt in
  try_bind
    (fun () -> bind (printf "1") (fun () -> map (fun () -> `Ok) (printf "2")))
    (fun `Ok -> bind (printf "3") (fun () -> return ()))
    (fun _ -> return ())

let lwt_calls_rebind () =
  let tr = Lwt.try_bind in
  let b = Lwt.bind in
  let ( >> ) = Lwt.Infix.( >|= ) in
  let p fmt = Lwt_fmt.printf fmt in
  let ( ~@ ) = Lwt.return in
  tr
    (fun () -> b (p "1") (fun () -> p "2" >> fun () -> `Ok))
    (fun `Ok -> b (p "3") (fun () -> ~@()))
    (fun _ -> ~@())

let lwt_calls_alias () =
  let module L = Lwt in
  let module F = Lwt_fmt in
  let open L.Infix in
  F.printf "1"
  >>= (fun () -> F.printf "2" >|= fun () -> ())
  <&> (F.printf "3" >>= fun () -> L.return ())

let lwt_calls_include () =
  let module L = struct
    include Lwt
    include Lwt_fmt
  end in
  let open L in
  try_bind
    (fun () -> bind (printf "1") (fun () -> map (fun () -> `Ok) (printf "2")))
    (fun `Ok -> bind (printf "3") (fun () -> return ()))
    (fun _ -> return ())

let test () =
  Lwt_fmt.printf "Test.test" >>= fun () ->
  Lwt.both (lwt_calls ()) (lwt_calls_point_free ()) >>= fun _ ->
  (let a = lwt_calls () and b = lwt_calls_point_free () in
   Lwt.both a b)
  >>= fun _ ->
  Lwt.join
    [
      letops ();
      infix ();
      lwt_calls_open ();
      lwt_calls_rebind ();
      lwt_calls_alias ();
      lwt_calls_include ();
    ]
  >>= Lwt.return
