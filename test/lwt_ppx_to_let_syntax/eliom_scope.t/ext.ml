let%server f () =
  [%client
    let%lwt x = y in
    ()]
