open Lwt.Syntax

let async_process _ = Lwt.return ()

let _f _ = Lwt_unix.with_timeout 1.0 (fun () -> Lwt.return 42)

let _f fname =
  let* fd = Lwt_io.open_file ~mode:Lwt_io.input fname in
  Lwt_io.close fd

let main () =
  Lwt.async (fun () -> async_process 1);
  let fd = Lwt_unix.of_unix_file_descr Unix.stdin in
  let in_chan = Lwt_io.of_fd ~mode:Lwt_io.input fd in
  let* s = Lwt_io.read in_chan in
  Lwt_io.printf "%s" s

let () = Lwt_main.run (main ())
