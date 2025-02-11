  $ chmod a+w test.ml

  $ lwt-to-eio
  Formatted 1 files, 0 errors

  $ cat test.ml
  let _ = Lwt.bind (fun binding_name -> binding_body) (Lwt.return binding_value)
  let _ = Lwt.bind (function case -> ()) input
  let _ = Lwt.bind (function case -> () | case2 -> ()) input
  let _ = Lwt.catch (fun () -> input) (function case -> ())
  let _ = Lwt.catch (fun () -> input) (function case -> () | case2 -> ())
