open Lwt.Syntax

let _f fname =
  let inp =
    Unix.(openfile fname [ O_RDWR; O_NONBLOCK; O_APPEND ]) 0o660
    |> Lwt_unix.of_unix_file_descr
    |> Lwt_io.(of_fd ~mode:input)
  in
  let buf = Bytes.create 1024 in
  let* _n : int = Lwt_io.read_into inp buf 0 1024 in
  Lwt.return ()

let _ = Lwt_unix.Timeout
let _ : Lwt_unix.sockaddr = Lwt_unix.ADDR_UNIX ""

let (Lwt_unix.ADDR_UNIX _ | ADDR_INET _) =
  Lwt_unix.ADDR_INET (Unix.inet_addr_any, 0)
