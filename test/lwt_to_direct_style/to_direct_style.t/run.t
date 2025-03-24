Make a writable directory tree:
  $ cp -r --no-preserve=mode -L src out
  $ cd out

  $ dune build @ocaml-index
  $ find _build -name '*.ocaml-index'
  _build/default/lib/.test.objs/cctx.ocaml-index
  _build/default/bin/.main.eobjs/cctx.ocaml-index

  $ lwt-to-direct-style
  bin/main.ml: (32 occurrences)
    Lwt.return (bin/main.ml[9,150+15]..[9,150+25])
    Lwt.return (bin/main.ml[16,317+45]..[16,317+55])
    Lwt.return (bin/main.ml[19,410+24]..[19,410+34])
    Lwt.return (bin/main.ml[20,475+14]..[20,475+24])
    Lwt.return (bin/main.ml[37,792+52]..[37,792+62])
    Lwt.return (bin/main.ml[38,860+4]..[38,860+14])
    Lwt.return (bin/main.ml[39,875+34]..[39,875+44])
    Lwt.return_unit (bin/main.ml[19,410+48]..[19,410+63])
    Lwt.return_none (bin/main.ml[16,317+28]..[16,317+43])
    Lwt.return_none (bin/main.ml[24,538+8]..[24,538+23])
    Lwt.return_nil (bin/main.ml[25,562+8]..[25,562+22])
    Lwt.return_true (bin/main.ml[26,585+8]..[26,585+23])
    Lwt.return_false (bin/main.ml[27,609+8]..[27,609+24])
    Lwt.return_ok (bin/main.ml[28,634+8]..[28,634+21])
    Lwt.return_error (bin/main.ml[29,659+8]..[29,659+24])
    Lwt.reraise (bin/main.ml[12,254+15]..[12,254+26])
    Lwt.reraise (bin/main.ml[16,317+56]..[16,317+67])
    Lwt.reraise (bin/main.ml[39,875+54]..[39,875+65])
    Lwt.try_bind (bin/main.ml[4,32+2]..[4,32+14])
    Lwt.try_bind (bin/main.ml[16,317+4]..[16,317+16])
    Lwt.try_bind (bin/main.ml[18,390+2]..[18,390+14])
    Lwt.try_bind (bin/main.ml[36,777+2]..[36,777+14])
    Lwt.let* (bin/main.ml[6,62+6]..[6,62+10])
    Lwt.let+ (bin/main.ml[7,108+6]..[7,108+10])
    Lwt_fmt.printf (bin/main.ml[6,62+16]..[6,62+30])
    Lwt_fmt.printf (bin/main.ml[11,194+23]..[11,194+37])
    Lwt_fmt.printf (bin/main.ml[31,688+8]..[31,688+22])
    Lwt_fmt.eprintf (bin/main.ml[32,714+8]..[32,714+23])
    Lwt_main.run (bin/main.ml[22,505+9]..[22,505+21])
    Lwt_unix.sleep (bin/main.ml[33,741+8]..[33,741+22])
    Lwt_unix.Timeout (bin/main.ml[39,875+14]..[39,875+30])
    Lwt_unix.with_timeout (bin/main.ml[37,792+15]..[37,792+36])
  lib/test.ml: (150 occurrences)
    Lwt.t (lib/test.ml[103,2618+35]..[103,2618+40])
    Lwt.t (lib/test.ml[155,4038+13]..[155,4038+18])
    Lwt.t (lib/test.ml[156,4061+21]..[156,4061+26])
    Lwt.t (lib/test.ml[157,4102+13]..[157,4102+18])
    Lwt.t (lib/test.ml[158,4164+22]..[158,4164+27])
    Lwt.t (lib/test.ml[158,4164+37]..[158,4164+42])
    Lwt.t (lib/test.ml[159,4239+14]..[159,4239+19])
    Lwt.new_key (lib/test.ml[150,3895+10]..[150,3895+21])
    Lwt.get (lib/test.ml[151,3920+8]..[151,3920+15])
    Lwt.with_value (lib/test.ml[152,3940+8]..[152,3940+22])
    Lwt.with_value (lib/test.ml[153,3991+8]..[153,3991+22])
    Lwt.wakeup (lib/test.ml[124,3241+2]..[124,3241+12])
    Lwt.wakeup_later (lib/test.ml[125,3260+2]..[125,3260+18])
    Lwt.return (lib/test.ml[9,185+57]..[9,185+67])
    Lwt.return (lib/test.ml[10,258+14]..[10,258+24])
    Lwt.return (lib/test.ml[28,713+2]..[28,713+12])
    Lwt.return (lib/test.ml[33,820+40]..[33,820+50])
    Lwt.return (lib/test.ml[40,1030+45]..[40,1030+51])
    Lwt.return (lib/test.ml[41,1087+14]..[41,1087+20])
    Lwt.return (lib/test.ml[48,1260+15]..[48,1260+25])
    Lwt.return (lib/test.ml[60,1582+34]..[60,1582+42])
    Lwt.return (lib/test.ml[70,1832+45]..[70,1832+51])
    Lwt.return (lib/test.ml[71,1889+14]..[71,1889+20])
    Lwt.return (lib/test.ml[88,2290+6]..[88,2290+16])
    Lwt.return (lib/test.ml[90,2308+8]..[90,2308+18])
    Lwt.return (lib/test.ml[94,2359+22]..[94,2359+32])
    Lwt.return (lib/test.ml[94,2359+37]..[94,2359+47])
    Lwt.return (lib/test.ml[95,2415+25]..[95,2415+35])
    Lwt.return (lib/test.ml[111,2902+8]..[111,2902+18])
    Lwt.return (lib/test.ml[117,3039+19]..[117,3039+29])
    Lwt.return (lib/test.ml[140,3706+27]..[140,3706+37])
    Lwt.fail (lib/test.ml[109,2841+8]..[109,2841+16])
    Lwt.fail_with (lib/test.ml[110,2868+8]..[110,2868+21])
    Lwt.fail_invalid_arg (lib/test.ml[118,3074+32]..[118,3074+52])
    Lwt.wait (lib/test.ml[122,3186+13]..[122,3186+21])
    Lwt.task (lib/test.ml[142,3776+8]..[142,3776+16])
    Lwt.bind (lib/test.ml[7,81+6]..[7,81+14])
    Lwt.bind (lib/test.ml[9,185+16]..[9,185+24])
    Lwt.bind (lib/test.ml[13,318+2]..[13,318+10])
    Lwt.bind (lib/test.ml[39,951+15]..[39,951+19])
    Lwt.bind (lib/test.ml[40,1030+16]..[40,1030+20])
    Lwt.bind (lib/test.ml[45,1166+10]..[45,1166+18])
    Lwt.bind (lib/test.ml[69,1753+15]..[69,1753+19])
    Lwt.bind (lib/test.ml[70,1832+16]..[70,1832+20])
    Lwt.map (lib/test.ml[8,128+10]..[8,128+17])
    Lwt.map (lib/test.ml[14,363+24]..[14,363+31])
    Lwt.map (lib/test.ml[39,951+44]..[39,951+47])
    Lwt.map (lib/test.ml[69,1753+44]..[69,1753+47])
    Lwt.catch (lib/test.ml[101,2523+11]..[101,2523+20])
    Lwt.catch (lib/test.ml[102,2574+11]..[102,2574+20])
    Lwt.catch (lib/test.ml[103,2618+11]..[103,2618+20])
    Lwt.catch (lib/test.ml[104,2675+11]..[104,2675+20])
    Lwt.catch (lib/test.ml[105,2725+11]..[105,2725+20])
    Lwt.catch (lib/test.ml[106,2798+11]..[106,2798+20])
    Lwt.try_bind (lib/test.ml[5,51+2]..[5,51+14])
    Lwt.try_bind (lib/test.ml[38,940+2]..[38,940+10])
    Lwt.try_bind (lib/test.ml[44,1139+11]..[44,1139+23])
    Lwt.try_bind (lib/test.ml[68,1742+2]..[68,1742+10])
    Lwt.finalize (lib/test.ml[118,3074+8]..[118,3074+20])
    Lwt.async (lib/test.ml[119,3145+8]..[119,3145+17])
    Lwt.async (lib/test.ml[123,3214+2]..[123,3214+11])
    Lwt.async (lib/test.ml[157,4102+38]..[157,4102+47])
    Lwt.ignore_result (lib/test.ml[140,3706+8]..[140,3706+25])
    Lwt.ignore_result (lib/test.ml[141,3748+8]..[141,3748+25])
    Lwt.join (lib/test.ml[79,2128+2]..[79,2128+10])
    Lwt.join (lib/test.ml[116,3013+8]..[116,3013+16])
    Lwt.join (lib/test.ml[117,3039+8]..[117,3039+16])
    Lwt.both (lib/test.ml[75,1972+2]..[75,1972+10])
    Lwt.both (lib/test.ml[77,2096+3]..[77,2096+11])
    Lwt.choose (lib/test.ml[115,2985+8]..[115,2985+18])
    Lwt.pick (lib/test.ml[94,2359+11]..[94,2359+19])
    Lwt.pick (lib/test.ml[95,2415+11]..[95,2415+19])
    Lwt.pick (lib/test.ml[96,2462+11]..[96,2462+19])
    Lwt.Return (lib/test.ml[146,3830+4]..[146,3830+10])
    Lwt.Fail (lib/test.ml[147,3848+4]..[147,3848+8])
    Lwt.Sleep (lib/test.ml[148,3878+4]..[148,3878+9])
    Lwt.state (lib/test.ml[145,3805+8]..[145,3805+17])
    Lwt.pause (lib/test.ml[114,2964+8]..[114,2964+17])
    Lwt.>>= (lib/test.ml[32,766+2]..[32,766+5])
    Lwt.>>= (lib/test.ml[33,820+26]..[33,820+29])
    Lwt.>>= (lib/test.ml[59,1534+2]..[59,1534+5])
    Lwt.>>= (lib/test.ml[60,1582+20]..[60,1582+23])
    Lwt.>>= (lib/test.ml[74,1929+29]..[74,1929+32])
    Lwt.>>= (lib/test.ml[75,1972+52]..[75,1972+55])
    Lwt.>>= (lib/test.ml[78,2113+2]..[78,2113+5])
    Lwt.>>= (lib/test.ml[88,2290+2]..[88,2290+5])
    Lwt.>>= (lib/test.ml[158,4164+59]..[158,4164+62])
    Lwt.=<< (lib/test.ml[111,2902+19]..[111,2902+22])
    Lwt.>|= (lib/test.ml[32,766+36]..[32,766+39])
    Lwt.>|= (lib/test.ml[46,1188+15]..[46,1188+32])
    Lwt.>|= (lib/test.ml[59,1534+30]..[59,1534+33])
    Lwt.=|< (lib/test.ml[112,2927+15]..[112,2927+18])
    Lwt.<&> (lib/test.ml[33,820+2]..[33,820+5])
    Lwt.<&> (lib/test.ml[60,1582+2]..[60,1582+5])
    Lwt.<?> (lib/test.ml[113,2948+10]..[113,2948+13])
    Lwt.let* (lib/test.ml[17,428+2]..[17,428+6])
    Lwt.let* (lib/test.ml[18,441+4]..[18,441+8])
    Lwt.let* (lib/test.ml[22,555+2]..[22,555+6])
    Lwt.let* (lib/test.ml[23,568+4]..[23,568+8])
    Lwt.let* (lib/test.ml[94,2359+2]..[94,2359+6])
    Lwt.let* (lib/test.ml[95,2415+2]..[95,2415+6])
    Lwt.let* (lib/test.ml[96,2462+2]..[96,2462+6])
    Lwt.let* (lib/test.ml[101,2523+2]..[101,2523+6])
    Lwt.let* (lib/test.ml[102,2574+2]..[102,2574+6])
    Lwt.let* (lib/test.ml[103,2618+2]..[103,2618+6])
    Lwt.let* (lib/test.ml[104,2675+2]..[104,2675+6])
    Lwt.let* (lib/test.ml[105,2725+2]..[105,2725+6])
    Lwt.let* (lib/test.ml[106,2798+2]..[106,2798+6])
    Lwt.and* (lib/test.ml[21,521+2]..[21,521+6])
    Lwt.and* (lib/test.ml[26,648+2]..[26,648+6])
    Lwt.and* (lib/test.ml[27,679+2]..[27,679+6])
    Lwt.let+ (lib/test.ml[19,477+4]..[19,477+8])
    Lwt.let+ (lib/test.ml[24,604+4]..[24,604+8])
    Lwt_condition.create (lib/test.ml[131,3409+8]..[131,3409+28])
    Lwt_condition.wait (lib/test.ml[132,3441+14]..[132,3441+32])
    Lwt_condition.wait (lib/test.ml[133,3479+20]..[133,3479+38])
    Lwt_condition.wait (lib/test.ml[134,3530+20]..[134,3530+38])
    Lwt_fmt.printf (lib/test.ml[7,81+16]..[7,81+30])
    Lwt_fmt.printf (lib/test.ml[8,128+35]..[8,128+49])
    Lwt_fmt.printf (lib/test.ml[9,185+26]..[9,185+40])
    Lwt_fmt.printf (lib/test.ml[13,318+12]..[13,318+26])
    Lwt_fmt.printf (lib/test.ml[14,363+2]..[14,363+16])
    Lwt_fmt.printf (lib/test.ml[18,441+14]..[18,441+28])
    Lwt_fmt.printf (lib/test.ml[19,477+14]..[19,477+28])
    Lwt_fmt.printf (lib/test.ml[21,521+12]..[21,521+26])
    Lwt_fmt.printf (lib/test.ml[23,568+14]..[23,568+28])
    Lwt_fmt.printf (lib/test.ml[24,604+14]..[24,604+28])
    Lwt_fmt.printf (lib/test.ml[26,648+12]..[26,648+26])
    Lwt_fmt.printf (lib/test.ml[27,679+12]..[27,679+26])
    Lwt_fmt.printf (lib/test.ml[31,745+2]..[31,745+16])
    Lwt_fmt.printf (lib/test.ml[32,766+17]..[32,766+31])
    Lwt_fmt.printf (lib/test.ml[33,820+7]..[33,820+21])
    Lwt_fmt.printf (lib/test.ml[39,951+21]..[39,951+27])
    Lwt_fmt.printf (lib/test.ml[39,951+65]..[39,951+71])
    Lwt_fmt.printf (lib/test.ml[40,1030+22]..[40,1030+28])
    Lwt_fmt.printf (lib/test.ml[47,1224+14]..[47,1224+28])
    Lwt_fmt.printf (lib/test.ml[58,1519+2]..[58,1519+10])
    Lwt_fmt.printf (lib/test.ml[59,1534+17]..[59,1534+25])
    Lwt_fmt.printf (lib/test.ml[60,1582+7]..[60,1582+15])
    Lwt_fmt.printf (lib/test.ml[69,1753+21]..[69,1753+27])
    Lwt_fmt.printf (lib/test.ml[69,1753+65]..[69,1753+71])
    Lwt_fmt.printf (lib/test.ml[70,1832+22]..[70,1832+28])
    Lwt_fmt.printf (lib/test.ml[74,1929+2]..[74,1929+16])
    Lwt_list.iter_s (lib/test.ml[127,3285+8]..[127,3285+23])
    Lwt_list.iter_p (lib/test.ml[128,3325+8]..[128,3325+23])
    Lwt_list.iteri_p (lib/test.ml[129,3365+8]..[129,3365+24])
    Lwt_mutex.create (lib/test.ml[135,3581+8]..[135,3581+24])
    Lwt_mutex.lock (lib/test.ml[136,3609+8]..[136,3609+22])
    Lwt_mutex.unlock (lib/test.ml[137,3634+8]..[137,3634+24])
    Lwt_mutex.with_lock (lib/test.ml[138,3661+8]..[138,3661+27])
  lib/test.mli: (15 occurrences)
    Lwt.t (lib/test.mli[1,0+34]..[1,0+39])
    Lwt.t (lib/test.mli[2,40+53]..[2,40+58])
    Lwt.t (lib/test.mli[3,99+60]..[3,99+65])
    Lwt.t (lib/test.mli[4,165+13]..[4,165+18])
    Lwt.t (lib/test.mli[5,184+21]..[5,184+26])
    Lwt.t (lib/test.mli[6,211+13]..[6,211+18])
    Lwt.t (lib/test.mli[7,238+22]..[7,238+27])
    Lwt.t (lib/test.mli[7,238+37]..[7,238+42])
    Lwt.t (lib/test.mli[8,281+14]..[8,281+19])
    Lwt.t (lib/test.mli[9,318+24]..[9,318+29])
    Lwt_condition.t (lib/test.mli[1,0+12]..[1,0+27])
    Lwt_condition.t (lib/test.mli[2,40+29]..[2,40+44])
    Lwt_condition.t (lib/test.mli[3,99+36]..[3,99+51])
    Lwt_mutex.t (lib/test.mli[2,40+9]..[2,40+20])
    Lwt_mutex.t (lib/test.mli[3,99+9]..[3,99+20])

  $ lwt-to-direct-style --migrate
  Warning: bin/main.ml: 1 occurrences have not been rewritten.
    Lwt_main.run (line 22 column 10)
  Warning: lib/test.ml: 4 occurrences have not been rewritten.
    Lwt.<?> (line 113 column 11)
    Lwt.choose (line 115 column 9)
    Lwt_list.iteri_p (line 129 column 9)
    Lwt.Fail (line 147 column 5)
  Warning: lib/test.mli: 2 occurrences have not been rewritten.
    Lwt_mutex.t (line 2 column 10)
    Lwt_mutex.t (line 3 column 10)
  Formatted 3 files, 0 errors

  $ cat bin/main.ml
  open Lwt.Syntax
  
  let _main () =
    match
      let () = Format.printf "Main.main" in
      let () = Test.test () in
      ()
    with
    | () -> ()
    | exception Failure msg -> Format.printf "Failure: %s\n%!" msg
  
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
  let _ = Format.printf ""
  let _ = Format.eprintf ""
  let _ = Eio_unix.sleep 1.0
  
  let _ =
    match
      Eio.Time.with_timeout_exn env#mono_clock
        (* TODO: lwt-to-direct-style: [env] must be propagated from the main loop *)
        1.0 (fun () -> 42)
    with
    | v -> v
    | exception Eio.Time.Timeout -> 0

  $ cat lib/test.ml
  open Eio.Std
  open Lwt.Infix
  open Lwt.Syntax
  
  let lwt_calls () =
    match
      let () = Format.printf "1" in
      let () = Format.printf "2" in
      `Ok
    with
    | `Ok ->
        let () = Format.printf "3" in
        ()
    | exception _ -> ()
  
  let lwt_calls_point_free () =
    let () = Format.printf "1" in
    let () = Format.printf "2" in
    ()
  
  let letops () =
    let (), `Ok =
      Fiber.pair (Format.printf "3")
        (let () = Format.printf "1" in
         let () = Format.printf "2" in
         `Ok)
    in
    let (), ((), `Ok) =
      Fiber.pair (Format.printf "6")
        (Fiber.pair (Format.printf "7")
           (let () = Format.printf "4" in
            let () = Format.printf "5" in
            `Ok))
    in
    ()
  
  let infix () =
    Fiber.pair
      (fun () ->
        let () =
          Format.printf
            (* TODO: lwt-to-direct-style: This computation might not be suspended correctly. *)
            "1"
        in
        let () = Format.printf "2" in
        ())
      (fun () ->
        let () =
          Format.printf
            (* TODO: lwt-to-direct-style: This computation might not be suspended correctly. *)
            "3"
        in
        ())
  
  let lwt_calls_open () =
    let open Lwt in
    let open Lwt_fmt in
    match
      let () = Format.printf "1" in
      let () = Format.printf "2" in
      `Ok
    with
    | `Ok ->
        let () = Format.printf "3" in
        ()
    | exception _ -> ()
  
  let lwt_calls_rebind () =
    let tr = fun x1 x2 x3 -> match x1 () with v -> x2 v | exception v -> x3 v in
    let b = fun x1 x2 -> x2 x1 in
    let ( >> ) = fun x1 x2 -> x2 x1 in
    let p fmt = Format.printf fmt in
    let ( ~@ ) = fun x1 -> x1 in
    tr
      (fun () -> b (p "1") (fun () -> p "2" >> fun () -> `Ok))
      (fun `Ok -> b (p "3") (fun () -> ~@()))
      (fun _ -> ~@())
  
  let lwt_calls_alias () =
    let module L = Lwt in
    let module F = Lwt_fmt in
    let open L.Infix in
    Fiber.pair
      (fun () ->
        let () =
          Format.printf
            (* TODO: lwt-to-direct-style: This computation might not be suspended correctly. *)
            "1"
        in
        let () = Format.printf "2" in
        ())
      (fun () ->
        let () =
          Format.printf
            (* TODO: lwt-to-direct-style: This computation might not be suspended correctly. *)
            "3"
        in
        ())
  
  let lwt_calls_include () =
    let module L = struct
      include Lwt
      include Lwt_fmt
    end in
    let open L in
    match
      let () = Format.printf "1" in
      let () = Format.printf "2" in
      `Ok
    with
    | `Ok ->
        let () = Format.printf "3" in
        ()
    | exception _ -> ()
  
  let test () =
    let () = Format.printf "Test.test" in
    let _ = Fiber.pair lwt_calls lwt_calls_point_free in
    let _ =
      let a = lwt_calls () and b = lwt_calls_point_free () in
      Fiber.pair
        (fun () ->
          a
          (* TODO: lwt-to-direct-style: This computation might not be suspended correctly. *))
        (fun () ->
          b
          (* TODO: lwt-to-direct-style: This computation might not be suspended correctly. *))
    in
    Fiber.all
      [
        letops;
        infix;
        lwt_calls_open;
        lwt_calls_rebind;
        lwt_calls_alias;
        lwt_calls_include;
      ]
  
  let x = ()
  
  let _ =
    let xs = [ x ] in
    let _ = Fiber.any [ (fun x1 -> x1); (fun x1 -> x1) ] in
    let _ =
      Fiber.any
        [
          (fun () ->
            x
            (* TODO: lwt-to-direct-style: This computation might not be suspended correctly. *));
          (fun x1 -> x1);
          (fun () ->
            x
            (* TODO: lwt-to-direct-style: This computation might not be suspended correctly. *));
        ]
    in
    let _ =
      Fiber.any xs
      (* TODO: lwt-to-direct-style: This expression is a ['a Lwt.t list] but a [(unit -> 'a) list] is expected. *)
    in
    x
  
  let _ =
    let handle _ = x in
    let _ = try x with _ -> x in
    let _ = try x with v -> handle v in
    let _ = try (fun _ : unit Promise.t -> x) () with v -> handle v in
    let _ = try (function () -> x) () with v -> handle v in
    let _ = try x with Not_found -> x | _ -> x in
    let _ = try handle () with v -> handle v in
    x
  
  let _ = raise Not_found
  let _ = failwith "not found"
  let _ = x
  let _ = Fun.id x
  
  let _ =
    x
    (* TODO: lwt-to-direct-style: [<?>] can't be automatically translated.Use Eio.Promise instead.  *)
    <?> x
  
  let _ = Fiber.yield ()
  
  let _ =
    Lwt.choose
      (* TODO: lwt-to-direct-style: [Lwt.choose] can't be automatically translated.Use Eio.Promise instead.  *)
      (* TODO: lwt-to-direct-style: [Lwt.choose] can't be automatically translated.Use Eio.Promise instead.  *)
      [ x; x ]
  
  let _ =
    Fiber.all
      [
        (fun () ->
          x
          (* TODO: lwt-to-direct-style: This computation might not be suspended correctly. *));
        (fun () ->
          x
          (* TODO: lwt-to-direct-style: This computation might not be suspended correctly. *));
      ]
  
  let _ = Fiber.all [ (fun x1 -> x1) ]
  let _ = Fun.protect ~finally:(fun () -> x) (fun () -> invalid_arg "")
  
  let _ =
    Fiber.fork ~sw
      (* TODO: lwt-to-direct-style: [sw] must be propagated here. *)
      (fun () -> x)
  
  let _ =
    let t, u
        (* TODO: lwt-to-direct-style: Translation is incomplete, [Promise.await] must be called on the promise when it's part of control-flow. *)
        =
      Promise.create ()
    in
    Fiber.fork ~sw
      (* TODO: lwt-to-direct-style: [sw] must be propagated here. *) (fun () -> t);
    Promise.resolve u ();
    Promise.resolve u ()
  
  let _ = List.iter (fun _ -> x) []
  let _ = Fiber.List.iter (fun _ -> x) []
  
  let _ =
    Lwt_list.iteri_p
      (* TODO: lwt-to-direct-style: [iteri] can't be translated automatically. See https://ocaml.org/p/eio/latest/doc/Eio/Fiber/List/index.html *)
      (* TODO: lwt-to-direct-style: [iteri] can't be translated automatically. See https://ocaml.org/p/eio/latest/doc/Eio/Fiber/List/index.html *)
      (fun _ _ -> x)
      []
  
  let _ = Eio.Condition.create ()
  
  let f1 cond =
    Eio.Condition.await
      (* TODO: lwt-to-direct-style: A mutex must be passed *)
      cond __mutex__
  
  let f2 mutex cond = Eio.Condition.await cond mutex
  
  let f3 mutex cond =
    Eio.Condition.await cond
      (Option.get
         (* TODO: lwt-to-direct-style: [mutex] shouldn't be an option. *)
         mutex)
  
  let m = Eio.Mutex.create ()
  let _ = Eio.Mutex.lock m
  let _ = Eio.Mutex.unlock m
  let _ = Eio.Mutex.use_rw ~protect:false m (fun () -> x)
  
  let _ (* TODO: lwt-to-direct-style: [sw] must be propagated here. *) =
    Fiber.fork ~sw (fun x1 -> x1)
  
  let _ =
    Fiber.fork ~sw (fun () ->
        (* TODO: lwt-to-direct-style: [sw] must be propagated here. *)
        x
        (* TODO: lwt-to-direct-style: This computation might not be suspended correctly. *))
  
  let _
      (* TODO: lwt-to-direct-style: Use [Switch] or [Cancel] for defining a cancellable context. *)
      (* TODO: lwt-to-direct-style: Translation is incomplete, [Promise.await] must be called on the promise when it's part of control-flow. *)
      =
    Promise.create ()
  
  let _ =
    match Promise.peek x with
    | Some x -> x
    | Fail (* TODO: lwt-to-direct-style: [Lwt.Fail] shouldn't be used *) _ ->
        failwith "fail"
    | None -> ()
  
  let key = Fiber.create_key ()
  let _ = Fiber.get key
  
  let _ =
    (Option.fold ~none:Fiber.without_binding
       ~some:(Fun.flip Fiber.with_binding)
       (Some 12)) key (fun () -> x)
  
  let _ =
    (Option.fold ~none:Fiber.without_binding
       ~some:(Fun.flip Fiber.with_binding)
       None) key (fun () -> x)
  
  let x : unit Promise.t = x
  let f : unit -> unit = fun () -> x
  
  let g : unit Promise.t -> unit =
   fun y ->
    Fiber.fork ~sw
      (* TODO: lwt-to-direct-style: [sw] must be propagated here. *)
      (fun () -> y)
  
  let h : (unit -> unit) -> unit =
   fun f ->
    let () = f () in
    x
  
  let i : (unit Promise.t -> unit) -> unit = fun f -> f x

  $ cat lib/test.mli
  val f1 :
    Eio.Condition.t ->
    (* TODO: lwt-to-direct-style: Eio conditions don't carry a value. Use a mutable variable and a dedicated mutex. *)
    'a
  
  val f2 : Lwt_mutex.t -> Eio.Condition.t -> unit
  val f3 : Lwt_mutex.t option -> Eio.Condition.t -> unit
  val x : unit Promise.t
  val f : unit -> unit
  val g : unit Promise.t -> unit
  val h : (unit -> unit) -> unit
  val i : (unit Promise.t -> unit) -> unit
  val test : unit -> unit
