  $ chmod a+w *.ml
  $ lwt-ppx-to-let-syntax .
  Formatted 6 files, 0 errors

  $ cat client.ml
  open%client Lwt.Syntax
  
  let%client f () =
    let* x = y in
    ()

  $ cat server.ml
  open%server Lwt.Syntax
  
  let%server f () =
    let* x = y in
    ()

  $ cat shared.ml
  open%shared Lwt.Syntax
  
  let%shared f () =
    let* x = y in
    ()

  $ cat default.ml
  open Lwt.Syntax
  
  let f () =
    let* x = y in
    ()

  $ cat ext.ml
  open%client Lwt.Syntax
  
  let%server f () =
    [%client
      let* x = y in
      ()]

  $ cat both.ml
  open%shared Lwt.Syntax
  
  let%client f () =
    let* x = y in
    ()
  
  let%server f () =
    let* x = y in
    ()
