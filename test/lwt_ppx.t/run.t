  $ chmod a+w test.ml

  $ lwt-to-eio
  Formatted 1 files, 0 errors

  $ cat test.ml
  let _ = Lwt.bind (fun binding_name -> binding_body) (Lwt.return binding_value)
