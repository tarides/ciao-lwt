  $ cp -r --no-preserve=mode -L src out
  $ cd out
  $ dune build @ocaml-index

  $ ciao-lwt to-eio --migrate
  Found no occurrences.
  [1]

  $ cat main.ml
  let _ = Js_of_ocaml_lwt.Lwt_js.sleep 1.
  
  open Js_of_ocaml_lwt
  
  let _ = Lwt_js.sleep 1.
  let _ = Lwt_js.yield ()
  
  open Lwt_js_events
  
  let _f elm = click elm
