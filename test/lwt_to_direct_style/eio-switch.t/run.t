Make a writable directory tree:
  $ cp -r --no-preserve=mode -L src out
  $ cd out

  $ dune build @ocaml-index
  $ lwt-to-direct-style --migrate --eio-sw-as-fiber-var Fiber_var.sw
  Formatted 1 files
  Warning: main.ml: 2 occurrences have not been rewritten.
    Lwt_io.read (line 9 column 12)
    Lwt_io.printf (line 10 column 3)

  $ cat main.ml
  open Eio.Std
  
  let async_process _ = ()
  
  let main () =
    Fiber.fork
      ~sw:(Option.get (Fiber.get Fiber_var.sw))
      (fun () -> async_process 1);
    let fd =
     fun ?blocking:x1 ?set_flags:x2 ->
      Eio_unix.Fd.of_unix
        ~sw:(Option.get (Fiber.get Fiber_var.sw))
        ?blocking:x1 ~close_unix:true
        (* TODO: lwt-to-direct-style: Labelled argument ?set_flags was dropped. *)
        Unix.stdin
    in
    let in_chan =
      (Eio_unix.Net.import_socket_stream fd : [ `R | `Flow | `Close ] Std.r)
    in
    let s = Lwt_io.read in_chan in
    Lwt_io.printf "%s" s
  
  let () =
    Eio_main.run (fun env ->
        (* TODO: lwt-to-direct-style: [Eio_main.run] argument used to be a [Lwt] promise and is now a [fun]. Make sure no asynchronous or IO calls are done outside of this [fun]. *)
        (* TODO: lwt-to-direct-style: Make sure to create a [Switch.t] and store it in fiber variable ["Fiber_var.sw"]. *)
        main ())
