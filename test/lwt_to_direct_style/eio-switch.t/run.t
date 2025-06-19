Make a writable directory tree:
  $ cp -r --no-preserve=mode -L src out
  $ cd out

  $ dune build @ocaml-index
  $ lwt-to-direct-style --migrate --eio-sw-as-fiber-var Fiber_var.sw --eio-env-as-fiber-var Fiber_var.env
  Formatted 1 files
  Warning: main.ml: 1 occurrences have not been rewritten.
    Lwt_io.printf (line 16 column 3)

  $ cat main.ml
  open Eio.Std
  
  let async_process _ = ()
  
  let _f _ =
    Eio.Time.with_timeout_exn
      (Stdlib.Option.get (Fiber.get Fiber_var.env))#mono_clock 1.0 (fun () -> 42)
  
  let _f fname =
    let fd =
      Eio.Buf_read.of_flow ~max_size:1_000_000
        (Eio.Path.open_in
           ~sw:(Stdlib.Option.get (Fiber.get Fiber_var.sw))
           (Eio.Path.( / ) (Stdlib.Option.get (Fiber.get Fiber_var.env))#cwd fname))
    in
    Eio.Resource.close fd
  
  let main () =
    Fiber.fork
      ~sw:(Stdlib.Option.get (Fiber.get Fiber_var.sw))
      (fun () -> async_process 1);
    let fd = Unix.stdin in
    let in_chan =
      Eio.Buf_read.of_flow ~max_size:1_000_000
        (Eio_unix.Net.import_socket_stream
           ~sw:(Stdlib.Option.get (Fiber.get Fiber_var.sw))
           ~close_unix:true fd
          : [ `R | `Flow | `Close ] r)
    in
    let s = Eio.Buf_read.take_all in_chan in
    Lwt_io.printf "%s" s
  
  let () =
    Eio_main.run (fun env ->
        Fiber.with_binding Fiber_var.env env (fun () ->
            Switch.run ~name:"main" (fun sw ->
                Fiber.with_binding Fiber_var.sw sw (fun () ->
                    (* TODO: lwt-to-direct-style: [Eio_main.run] argument used to be a [Lwt] promise and is now a [fun]. Make sure no asynchronous or IO calls are done outside of this [fun]. *)
                    main ()))))
