Make a writable directory tree:
  $ cp -r --no-preserve=mode -L src out
  $ cd out

  $ dune build @ocaml-index
  $ find _build -name '*.ocaml-index'
  _build/default/lib/.test.objs/cctx.ocaml-index
  _build/default/bin/.main.eobjs/cctx.ocaml-index

  $ lwt-to-direct-style
  bin/main.ml: (19 occurrences)
    "return" (bin/main.ml[9,150+15]..[9,150+25])
    "return" (bin/main.ml[16,317+45]..[16,317+55])
    "return" (bin/main.ml[19,410+24]..[19,410+34])
    "return" (bin/main.ml[20,475+14]..[20,475+24])
    "return_unit" (bin/main.ml[19,410+48]..[19,410+63])
    "return_none" (bin/main.ml[16,317+28]..[16,317+43])
    "return_none" (bin/main.ml[24,538+8]..[24,538+23])
    "return_nil" (bin/main.ml[25,562+8]..[25,562+22])
    "return_true" (bin/main.ml[26,585+8]..[26,585+23])
    "return_false" (bin/main.ml[27,609+8]..[27,609+24])
    "return_ok" (bin/main.ml[28,634+8]..[28,634+21])
    "return_error" (bin/main.ml[29,659+8]..[29,659+24])
    "reraise" (bin/main.ml[12,254+15]..[12,254+26])
    "reraise" (bin/main.ml[16,317+56]..[16,317+67])
    "try_bind" (bin/main.ml[4,32+2]..[4,32+14])
    "try_bind" (bin/main.ml[16,317+4]..[16,317+16])
    "try_bind" (bin/main.ml[18,390+2]..[18,390+14])
    "let*" (bin/main.ml[6,62+6]..[6,62+10])
    "let+" (bin/main.ml[7,108+6]..[7,108+10])
  lib/test.ml: (45 occurrences)
    "return" (lib/test.ml[9,185+57]..[9,185+67])
    "return" (lib/test.ml[10,258+14]..[10,258+24])
    "return" (lib/test.ml[22,555+2]..[22,555+12])
    "return" (lib/test.ml[27,662+40]..[27,662+50])
    "return" (lib/test.ml[34,872+45]..[34,872+51])
    "return" (lib/test.ml[35,929+14]..[35,929+20])
    "return" (lib/test.ml[42,1102+15]..[42,1102+25])
    "return" (lib/test.ml[54,1424+34]..[54,1424+42])
    "return" (lib/test.ml[64,1674+45]..[64,1674+51])
    "return" (lib/test.ml[65,1731+14]..[65,1731+20])
    "return" (lib/test.ml[79,2041+6]..[79,2041+16])
    "bind" (lib/test.ml[7,81+6]..[7,81+14])
    "bind" (lib/test.ml[9,185+16]..[9,185+24])
    "bind" (lib/test.ml[13,318+2]..[13,318+10])
    "bind" (lib/test.ml[33,793+15]..[33,793+19])
    "bind" (lib/test.ml[34,872+16]..[34,872+20])
    "bind" (lib/test.ml[39,1008+10]..[39,1008+18])
    "bind" (lib/test.ml[63,1595+15]..[63,1595+19])
    "bind" (lib/test.ml[64,1674+16]..[64,1674+20])
    "map" (lib/test.ml[8,128+10]..[8,128+17])
    "map" (lib/test.ml[14,363+24]..[14,363+31])
    "map" (lib/test.ml[33,793+44]..[33,793+47])
    "map" (lib/test.ml[63,1595+44]..[63,1595+47])
    "try_bind" (lib/test.ml[5,51+2]..[5,51+14])
    "try_bind" (lib/test.ml[32,782+2]..[32,782+10])
    "try_bind" (lib/test.ml[38,981+11]..[38,981+23])
    "try_bind" (lib/test.ml[62,1584+2]..[62,1584+10])
    "join" (lib/test.ml[70,1879+2]..[70,1879+10])
    "both" (lib/test.ml[69,1814+2]..[69,1814+10])
    ">>=" (lib/test.ml[26,608+2]..[26,608+5])
    ">>=" (lib/test.ml[27,662+26]..[27,662+29])
    ">>=" (lib/test.ml[53,1376+2]..[53,1376+5])
    ">>=" (lib/test.ml[54,1424+20]..[54,1424+23])
    ">>=" (lib/test.ml[68,1771+29]..[68,1771+32])
    ">>=" (lib/test.ml[69,1814+52]..[69,1814+55])
    ">>=" (lib/test.ml[79,2041+2]..[79,2041+5])
    ">|=" (lib/test.ml[26,608+36]..[26,608+39])
    ">|=" (lib/test.ml[40,1030+15]..[40,1030+32])
    ">|=" (lib/test.ml[53,1376+30]..[53,1376+33])
    "<&>" (lib/test.ml[27,662+2]..[27,662+5])
    "<&>" (lib/test.ml[54,1424+2]..[54,1424+5])
    "let*" (lib/test.ml[17,428+2]..[17,428+6])
    "let*" (lib/test.ml[18,441+4]..[18,441+8])
    "and*" (lib/test.ml[21,521+2]..[21,521+6])
    "let+" (lib/test.ml[19,477+4]..[19,477+8])

  $ lwt-to-direct-style --migrate
  Warning: 4 occurrences have not been rewritten.
    let* (bin/main.ml[6,62+6]..[6,62+10])
    let+ (bin/main.ml[7,108+6]..[7,108+10])
    reraise (bin/main.ml[12,254+15]..[12,254+26])
    reraise (bin/main.ml[16,317+56]..[16,317+67])
  Warning: 13 occurrences have not been rewritten.
    let* (lib/test.ml[17,428+2]..[17,428+6])
    let* (lib/test.ml[18,441+4]..[18,441+8])
    let+ (lib/test.ml[19,477+4]..[19,477+8])
    and* (lib/test.ml[21,521+2]..[21,521+6])
    <&> (lib/test.ml[27,662+2]..[27,662+5])
    try_bind (lib/test.ml[38,981+11]..[38,981+23])
    bind (lib/test.ml[39,1008+10]..[39,1008+18])
    >|= (lib/test.ml[40,1030+15]..[40,1030+32])
    return (lib/test.ml[42,1102+15]..[42,1102+25])
    <&> (lib/test.ml[54,1424+2]..[54,1424+5])
    both (lib/test.ml[69,1814+2]..[69,1814+10])
    join (lib/test.ml[70,1879+2]..[70,1879+10])
    return (lib/test.ml[79,2041+6]..[79,2041+16])
  Formatted 2 files, 0 errors

  $ cat bin/main.ml
  open Lwt.Syntax
  
  let _main () =
    match
      let* () = Lwt_fmt.printf "Main.main" in
      let+ () = Test.test () in
      ()
    with
    | () -> ()
    | exception Failure msg -> Lwt_fmt.printf "Failure: %s\n%!" msg
  
  let main () =
    let main () = match None with v -> v in
    match main () with Some _ -> () | None -> () | exception _ -> ()
  
  let () = Lwt_main.run (main ())
  let _ = None
  let _ = []
  let _ = true
  let _ = false
  let _ = Ok ()
  let _ = Error ()

  $ cat lib/test.ml
  open Lwt.Infix
  open Lwt.Syntax
  
  let lwt_calls () =
    match
      let () = Lwt_fmt.printf "1" in
      let () = Lwt_fmt.printf "2" in
      `Ok
    with
    | `Ok ->
        let () = Lwt_fmt.printf "3" in
        ()
    | exception _ -> ()
  
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
    <&>
    let () = Lwt_fmt.printf "3" in
    ()
  
  let lwt_calls_open () =
    let open Lwt in
    let open Lwt_fmt in
    match
      let () = printf "1" in
      let () = printf "2" in
      `Ok
    with
    | `Ok ->
        let () = printf "3" in
        ()
    | exception _ -> ()
  
  let lwt_calls_rebind () =
    let tr = Lwt.try_bind in
    let b = Lwt.bind in
    let ( >> ) = Lwt.Infix.( >|= ) in
    let p fmt = Lwt_fmt.printf fmt in
    let ( ~@ ) = Lwt.return in
    tr
      (fun () -> b (p "1") (fun () -> p "2" >> fun () -> `Ok))
      (fun `Ok -> b (p "3") (fun () -> ~@()))
      (fun _ -> ~@())
  
  let lwt_calls_alias () =
    let module L = Lwt in
    let module F = Lwt_fmt in
    let open L.Infix in
    (let () = F.printf "1" in
     let () = F.printf "2" in
     ())
    <&>
    let () = F.printf "3" in
    ()
  
  let lwt_calls_include () =
    let module L = struct
      include Lwt
      include Lwt_fmt
    end in
    let open L in
    match
      let () = printf "1" in
      let () = printf "2" in
      `Ok
    with
    | `Ok ->
        let () = printf "3" in
        ()
    | exception _ -> ()
  
  let test () =
    let () = Lwt_fmt.printf "Test.test" in
    let _ = Lwt.both (lwt_calls ()) (lwt_calls_point_free ()) in
    Lwt.return
      (Lwt.join
         [
           letops ();
           infix ();
           lwt_calls_open ();
           lwt_calls_rebind ();
           lwt_calls_alias ();
           lwt_calls_include ();
         ])
