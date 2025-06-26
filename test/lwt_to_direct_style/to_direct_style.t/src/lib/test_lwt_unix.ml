open Lwt.Syntax

let _f fname =
  let inp =
    Unix.(openfile fname [ O_RDWR; O_NONBLOCK; O_APPEND ]) 0o660
    |> Lwt_unix.of_unix_file_descr
    |> Lwt_io.(of_fd ~mode:input)
  in
  let buf = Bytes.create 1024 in
  let* (_n : int) = Lwt_io.read_into inp buf 0 1024 in
  let* () = Lwt_io.read_into_exactly inp buf 0 1024 in
  Lwt.return ()

let _ = Lwt_unix.Timeout
let _ : Lwt_unix.sockaddr = Lwt_unix.ADDR_UNIX ""

let (Lwt_unix.ADDR_UNIX _ | ADDR_INET _) =
  Lwt_unix.ADDR_INET (Unix.inet_addr_any, 0)

let _ = Lwt_unix.getaddrinfo
let _f fd = Lwt_io.of_fd ~mode:Lwt_io.output fd
let _f fd = Lwt_io.of_fd ~mode:Lwt_io.Input fd
let _f fd = Lwt_io.of_fd ~mode:Lwt_io.Output fd
let _f out_chan = Lwt_io.write out_chan "str"
let _ : Lwt_io.input_channel = Lwt_io.stdin
let _ : Lwt_io.output_channel = Lwt_io.stdout
let _f chan = Lwt_io.read_line chan

let _f fname =
  let* fd = Lwt_io.open_file ~mode:Lwt_io.input fname in
  Lwt_io.close fd

let _f fname =
  let* fd = Lwt_io.open_file ~mode:Lwt_io.output fname in
  Lwt_io.length fd

let _f fname = Lwt_unix.stat fname
let _f fname = Lwt_unix.lstat fname
let _f chan = Lwt_io.read chan
let _f chan = Lwt_io.read ~count:42 chan
let _f chan = Lwt_io.read ?count:(Some 42) chan
let _f chan = Lwt_io.flush chan
