Make a writable directory tree:
  $ cp -r --no-preserve=mode -L src out
  $ cd out

  $ dune build @ocaml-index
  $ find _build -name '*.ocaml-index'
  _build/default/lib/.test.objs/cctx.ocaml-index
  _build/default/bin/.main.eobjs/cctx.ocaml-index

  $ lwt-to-direct-style
  bin/main.ml: (14 occurrences)
    "Lwt.return" (bin/main.ml[9,150+15]..[9,150+25])
    "Lwt.return" (bin/main.ml[16,317+45]..[16,317+55])
    "Lwt.return" (bin/main.ml[19,410+24]..[19,410+34])
    "Lwt.return" (bin/main.ml[20,475+14]..[20,475+24])
    "Lwt.return_unit" (bin/main.ml[19,410+48]..[19,410+63])
    "Lwt.return_none" (bin/main.ml[16,317+28]..[16,317+43])
    "Lwt.reraise" (bin/main.ml[12,254+15]..[12,254+26])
    "Lwt.reraise" (bin/main.ml[16,317+56]..[16,317+67])
    "Lwt.try_bind" (bin/main.ml[4,32+2]..[4,32+14])
    "Lwt.try_bind" (bin/main.ml[16,317+4]..[16,317+16])
    "Lwt.try_bind" (bin/main.ml[18,390+2]..[18,390+14])
    "let*" (bin/main.ml[6,62+6]..[6,62+10])
    "let+" (bin/main.ml[7,108+6]..[7,108+10])
    "Lwt.Syntax" (bin/main.ml[1,0+5]..[1,0+15])
  lib/test.ml: (26 occurrences)
    "Lwt.return" (lib/test.ml[9,185+57]..[9,185+67])
    "Lwt.return" (lib/test.ml[10,258+14]..[10,258+24])
    "Lwt.return" (lib/test.ml[22,555+2]..[22,555+12])
    "Lwt.return" (lib/test.ml[27,662+40]..[27,662+50])
    "Lwt.return" (lib/test.ml[32,840+39]..[32,840+49])
    "Lwt.bind" (lib/test.ml[7,81+6]..[7,81+14])
    "Lwt.bind" (lib/test.ml[9,185+16]..[9,185+24])
    "Lwt.bind" (lib/test.ml[13,318+2]..[13,318+10])
    "Lwt.map" (lib/test.ml[8,128+10]..[8,128+17])
    "Lwt.map" (lib/test.ml[14,363+24]..[14,363+31])
    "Lwt.try_bind" (lib/test.ml[5,51+2]..[5,51+14])
    "Lwt.join" (lib/test.ml[32,840+2]..[32,840+10])
    "Lwt.both" (lib/test.ml[31,775+2]..[31,775+10])
    ">>=" (lib/test.ml[26,608+2]..[26,608+5])
    ">>=" (lib/test.ml[27,662+26]..[27,662+29])
    ">>=" (lib/test.ml[30,732+29]..[30,732+32])
    ">>=" (lib/test.ml[31,775+52]..[31,775+55])
    ">>=" (lib/test.ml[32,840+35]..[32,840+38])
    ">|=" (lib/test.ml[26,608+36]..[26,608+39])
    "<&>" (lib/test.ml[27,662+2]..[27,662+5])
    "Lwt.Infix" (lib/test.ml[1,0+5]..[1,0+14])
    "let*" (lib/test.ml[17,428+2]..[17,428+6])
    "let*" (lib/test.ml[18,441+4]..[18,441+8])
    "and*" (lib/test.ml[21,521+2]..[21,521+6])
    "let+" (lib/test.ml[19,477+4]..[19,477+8])
    "Lwt.Syntax" (lib/test.ml[2,15+5]..[2,15+15])

  $ lwt-to-direct-style --migrate
  Warning: 10 occurrences have not been rewritten.
    "Lwt.try_bind" (bin/main.ml[4,32+2]..[4,32+14])
    "let*" (bin/main.ml[6,62+6]..[6,62+10])
    "let+" (bin/main.ml[7,108+6]..[7,108+10])
    "Lwt.reraise" (bin/main.ml[12,254+15]..[12,254+26])
    "Lwt.try_bind" (bin/main.ml[16,317+4]..[16,317+16])
    "Lwt.return_none" (bin/main.ml[16,317+28]..[16,317+43])
    "Lwt.return" (bin/main.ml[16,317+45]..[16,317+55])
    "Lwt.reraise" (bin/main.ml[16,317+56]..[16,317+67])
    "Lwt.try_bind" (bin/main.ml[18,390+2]..[18,390+14])
    "Lwt.return_unit" (bin/main.ml[19,410+48]..[19,410+63])
  Warning: 9 occurrences have not been rewritten.
    "Lwt.try_bind" (lib/test.ml[5,51+2]..[5,51+14])
    "let*" (lib/test.ml[17,428+2]..[17,428+6])
    "let*" (lib/test.ml[18,441+4]..[18,441+8])
    "let+" (lib/test.ml[19,477+4]..[19,477+8])
    "and*" (lib/test.ml[21,521+2]..[21,521+6])
    "<&>" (lib/test.ml[27,662+2]..[27,662+5])
    "Lwt.both" (lib/test.ml[31,775+2]..[31,775+10])
    "Lwt.join" (lib/test.ml[32,840+2]..[32,840+10])
    "Lwt.return" (lib/test.ml[32,840+39]..[32,840+49])
  Formatted 2 files, 0 errors

  $ cat bin/main.ml
  let _main () =
    Lwt.try_bind
      (fun () ->
        let* () = Lwt_fmt.printf "Main.main" in
        let+ () = Test.test () in
        ())
      (fun () -> ())
      (function
        | Failure msg -> Lwt_fmt.printf "Failure: %s\n%!" msg
        | exc -> Lwt.reraise exc)
  
  let main () =
    let main () =
      Lwt.try_bind (fun () -> Lwt.return_none) Lwt.return Lwt.reraise
    in
    Lwt.try_bind main
      (function Some _ -> () | None -> Lwt.return_unit)
      (fun _ -> ())
  
  let () = Lwt_main.run (main ())

  $ cat lib/test.ml
  let lwt_calls () =
    Lwt.try_bind
      (fun () ->
        let () = Lwt_fmt.printf "1" in
        let () = Lwt_fmt.printf "2" in
  
        `Ok)
      (fun `Ok -> let () = Lwt_fmt.printf "3" in
  
                  ())
      (fun _ -> ())
  
  let lwt_calls_point_free () =
    let () = Lwt_fmt.printf "1" in
    let () = Lwt_fmt.printf "2" in
  
    ()
  
  let letops () =
    let* `Ok =
      let* () = Lwt_fmt.printf "1" in
      let+ () = Lwt_fmt.printf "2" in
      `Ok
    and* () = Lwt_fmt.printf "3" in
    ()
  
  let infix () =
    (let () = Lwt_fmt.printf "1" in
     let () = Lwt_fmt.printf "2" in
  
     ())
    <&> let () = Lwt_fmt.printf "3" in
  
        ()
  
  let test () =
    let () = Lwt_fmt.printf "Test.test" in
    let _ = Lwt.both (lwt_calls ()) (lwt_calls_point_free ()) in
    Lwt.return (Lwt.join [ letops (); infix () ])
