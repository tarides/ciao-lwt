  $ chmod a+w test.ml

  $ lwt-ppx-to-let-syntax
  Formatted 1 files, 0 errors

  $ cat test.ml
  let _ = Lwt.bind (Lwt.return binding_value) (fun binding_name -> binding_body)
  
  let _ =
    Lwt.bind v1 (fun (n1 : t) ->
        Lwt.bind (v2 : t :> t') (fun n2 -> Lwt.bind n3 (fun n3 -> ())))
  
  let _ =
    let __ppx_lwt_0 = v1 and __ppx_lwt_1 = v2 in
    Lwt.bind __ppx_lwt_1 (fun n2 ->
        Lwt.bind __ppx_lwt_0 (fun n1 ->
            let __ppx_lwt_0 = v1
            and __ppx_lwt_1 = v2
            and __ppx_lwt_2 = v3
            and __ppx_lwt_3 = v4
            and __ppx_lwt_4 = (v5 : t :> t') in
            Lwt.bind __ppx_lwt_4 (fun v5 ->
                Lwt.bind __ppx_lwt_3 (fun (v4 : t) ->
                    Lwt.bind __ppx_lwt_2 (fun v3 ->
                        Lwt.bind __ppx_lwt_1 (fun n2 ->
                            Lwt.bind __ppx_lwt_0 (fun n1 -> ())))))))
  
  let _ = Lwt.bind input (function case -> ())
  let _ = Lwt.bind input (function case -> () | case2 -> ())
  
  let _ =
    Lwt.try_bind (fun () -> input) (function case -> ()) (function E -> ())
  
  let _ = Lwt.catch (fun () -> input) (function E -> ())
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
  
  let _ =
    Lwt.bind cond (function true -> a | false -> b);
    Lwt.bind cond (function true -> a | false -> Lwt.return_unit);
    Lwt.bind cond1 (function true -> a | false -> if cond2 then b else c);
    Lwt.bind cond1 (function
      | true -> a
      | false -> Lwt.bind cond2 (function true -> b | false -> c))
  
  let _ = Lwt.bind b (fun a -> c)
  
  let _ =
    Lwt.finalize (fun () -> expr) (fun () -> this);
    Lwt.finalize (fun () -> expr) (fun () -> this);
    Lwt.finalize (fun () -> some expr) (fun () -> this);
    Lwt.finalize (fun () -> some expr) (fun () -> this)
