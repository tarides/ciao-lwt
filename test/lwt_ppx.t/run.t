  $ chmod a+w test.ml

  $ lwt-to-eio
  Formatted 1 files, 0 errors

  $ cat test.ml
  let _ = Lwt.bind (Lwt.return binding_value) (fun binding_name -> binding_body)
  let _ = Lwt.bind input (function case -> ())
  let _ = Lwt.bind input (function case -> () | case2 -> ())
  let _ = Lwt.catch (fun () -> input) (function case -> ())
  let _ = Lwt.catch (fun () -> input) (function case -> () | case2 -> ())
  
  let _ =
    (let __ppx_lwt_bound = 10 in
     let rec __ppx_lwt_loop pat =
       if pat > __ppx_lwt_bound then Lwt.return_unit
       else Lwt.bind loop_body (fun () -> __ppx_lwt_loop (pat + 1))
     in
     __ppx_lwt_loop 0);
  
    (let __ppx_lwt_bound = 0 in
     let rec __ppx_lwt_loop pat =
       if pat < __ppx_lwt_bound then Lwt.return_unit
       else Lwt.bind loop_body (fun () -> __ppx_lwt_loop (pat - 1))
     in
     __ppx_lwt_loop 10);
  
    (let rec __ppx_lwt_loop () =
       if while_condition then Lwt.bind loop_body __ppx_lwt_loop
       else Lwt.return_unit
     in
     __ppx_lwt_loop ());
  
    ()
  
  let _ =
    Lwt.bind stmt_1 (fun () ->
        Lwt.bind stmt_2 (fun () -> Lwt.bind stmt_3 (fun () -> stmt_4)))
  
  let _ =
    Lwt.catch (fun () -> assert false) Lwt.fail;
    Lwt.catch (fun () -> assert (e = 1)) Lwt.fail
