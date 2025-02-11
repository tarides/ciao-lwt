let _ =
  let%lwt binding_name = Lwt.return binding_value in
  binding_body

let _ = match%lwt input with case -> ()
let _ = match%lwt input with case -> () | case2 -> ()
