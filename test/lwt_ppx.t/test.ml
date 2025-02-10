let _ =
  let%lwt binding_name = Lwt.return binding_value in
  binding_body
