let%client f () =
  let%lwt x = y in
  ()

let%server f () =
  let%lwt x = y in
  ()
