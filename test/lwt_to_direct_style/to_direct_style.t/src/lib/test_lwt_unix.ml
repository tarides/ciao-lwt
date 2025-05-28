let _ =
  Unix.(openfile "foo" [O_RDWR; O_NONBLOCK; O_APPEND]) 0o660
  |> Lwt_unix.of_unix_file_descr
  |> Lwt_io.(of_fd ~mode:input)
