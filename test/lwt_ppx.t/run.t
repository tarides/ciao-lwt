  $ chmod a+w test.ml

  $ lwt-to-eio
  Formatted 1 files, 0 errors

  $ cat test.ml
  let _ = Lwt.bind (Lwt.return binding_value) (fun binding_name -> binding_body)
  let _ = Lwt.bind input (function case -> ())
  let _ = Lwt.bind input (function case -> () | case2 -> ())
  let _ = Lwt.catch (fun () -> input) (function case -> ())
  let _ = Lwt.catch (fun () -> input) (function case -> () | case2 -> ())
