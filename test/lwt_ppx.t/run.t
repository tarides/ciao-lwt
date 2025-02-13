  $ chmod a+w test.ml

  $ lwt-ppx-to-let-syntax .
  Formatted 1 files, 0 errors

  $ cat test.ml
  open Lwt.Syntax
  
  let _ =
    let* binding_name = Lwt.return binding_value in
    binding_body
  
  let _ =
    let* n1 : t = v1 in
    let* n3 = n3 in
  
    ()
  
  let _ =
    let* n1 = v1 and* n2 = v2 in
    let* n1 = v1 and* n2 = v2 and* v3 = v3 and* v4 : t = v4 in
  
    (* Not translated due to a bug in ocamlformat *)
    let%lwt v5 : t :> t' = v5 and v6 : t :> t' = v6 in
    let* n1 = n1 and* n2 = n2 in
  
    ()
  
  let _ = Lwt.bind input (function case -> ())
  let _ = Lwt.bind input (function case -> () | case2 -> ())
  
  let _ =
    Lwt.try_bind
      (fun () -> input)
      (function case -> ())
      (function E -> () | exc -> Lwt.reraise exc)
  
  let _ =
    Lwt.try_bind
      (fun () -> input)
      (function case -> ())
      (function E -> () | catchall -> ())
  
  let _ =
    Lwt.catch (fun () -> input) (function E -> () | exc -> Lwt.reraise exc)
  
  let _ = Lwt.catch (fun () -> input) (fun (E | catchall) -> ())
  let _ = Lwt.catch (fun () -> input) (fun catchall -> ())
  let _ = Lwt.catch (fun () -> input) (function E -> () | catchall -> ())
  
  let _ =
    Lwt.catch
      (fun () -> input)
      (function E -> () | E' -> () | exc -> Lwt.reraise exc)
  
  let _ =
    (let __ppx_lwt_bound = 10 in
     let rec __ppx_lwt_loop pat =
       if pat > __ppx_lwt_bound then Lwt.return_unit
       else
         let* () = loop_body in
         __ppx_lwt_loop (pat + 1)
     in
     __ppx_lwt_loop 0);
  
    (let __ppx_lwt_bound = 0 in
     let rec __ppx_lwt_loop pat =
       if pat < __ppx_lwt_bound then Lwt.return_unit
       else
         let* () = loop_body in
         __ppx_lwt_loop (pat - 1)
     in
     __ppx_lwt_loop 10);
  
    (let rec __ppx_lwt_loop () =
       if while_condition then Lwt.bind loop_body __ppx_lwt_loop
       else Lwt.return_unit
     in
     __ppx_lwt_loop ());
  
    ()
  
  let _ =
    let* () = stmt_1 in
    let* () = stmt_2 in
    let* () = stmt_3 in
  
    stmt_4
  
  let _ =
    Lwt.catch (fun () -> assert false) Lwt.fail;
    Lwt.catch (fun () -> assert (e = 1)) Lwt.fail
  
  let _ =
    Lwt.bind cond (function true -> a | false -> b);
    Lwt.bind cond (function true -> a | false -> Lwt.return_unit);
    Lwt.bind cond1 (function true -> a | false -> if cond2 then b else c);
    Lwt.bind cond1 (function
      | true -> a
      | false -> Lwt.bind cond2 (function true -> b | false -> c))
  
  let _ = let* a = b in
  
          c
  
  let _ =
    Lwt.finalize (fun () -> expr) (fun () -> this);
    Lwt.finalize (fun () -> expr) (fun () -> this);
    Lwt.finalize (fun () -> some expr) (fun () -> this);
    Lwt.finalize (fun () -> some expr) (fun () -> this)
