  $ cp -r --no-preserve=mode -L src out
  $ cd out
  $ dune build @ocaml-index

  $ ciao-lwt to-eio
  main.ml: (6 occurrences)
    Js_of_ocaml_lwt (line 3 column 6)
    Js_of_ocaml_lwt.Lwt_js_events (line 8 column 6)
    Js_of_ocaml_lwt__Lwt_js.sleep (line 1 column 9)
    Js_of_ocaml_lwt__Lwt_js.sleep (line 5 column 9)
    Js_of_ocaml_lwt__Lwt_js.yield (line 6 column 9)
    Js_of_ocaml_lwt__Lwt_js_events.click (line 10 column 14)

  $ ciao-lwt to-eio --migrate
  Formatted 1 files

  $ cat main.ml
  let _ = Js_of_ocaml_eio.Eio_js.sleep 1.
  let _ = Js_of_ocaml_eio.Eio_js.sleep 1.
  let _ = Js_of_ocaml_eio.Eio_js.yield ()
  let _f elm = Js_of_ocaml_eio.Eio_js_events.click elm
