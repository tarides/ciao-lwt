  $ mkdir use_letop use_lwt_bind
  $ cp *.ml use_letop
  $ cp *.ml use_lwt_bind

  $ lwt-ppx-to-let-syntax use_letop
  Formatted 3 files, 0 errors
  $ lwt-ppx-to-let-syntax --use-lwt-bind use_lwt_bind
  Formatted 3 files, 0 errors

  $ cat use_letop/test.ml
  open Lwt.Syntax
  
  let _ =
    let* binding_name = Lwt.return binding_value in
    binding_body
  
  let _ =
    let* a = Lwt.return v1 in
    let* b = Lwt.return v2 in
    let* c = Lwt.return v3 in
    let* d = Lwt.return v4 in
  
    binding_body
  
  let _ =
    let* n1 = v1 and* n2 = v2 in
    let* n1 = v1 and* n2 = v2 and* v3 = v3 and* v4 : t = v4 in
  
    let%lwt v5 : t :> t' = v5 and v6 : t :> t' = v6 in
    ()
  
  (* Let bindings in different contextes. *)
  let _ =
    let* a = let* b = c in
  
             d in
    let* _ =
      Lwt.try_bind
        (fun () -> x)
        (function c -> let* a = b in
  
                       c)
        (function E -> () | exc -> Lwt.reraise exc)
    in
    let* () = Lwt.catch (fun () -> let* a = b in
  
                                   c) (fun _ -> d) in
    let* x = x in
  
    ()

  $ cat use_letop/dont_use_bind.ml
  (* [let*] is not used in this file and [open Lwt.Syntax] is not needed. *)
  let x = 42

  $ cat use_letop/already_had_open.ml
  open Lwt.Syntax
  
  (* This file already has [open Lwt.Syntax], no need to add one more. *)
  
  let _ = let* a = b in
  
          c

  $ cat use_lwt_bind/test.ml
  let _ = Lwt.bind (Lwt.return binding_value) (fun binding_name -> binding_body)
  
  let _ =
    Lwt.bind (Lwt.return v1) (fun a ->
        Lwt.bind (Lwt.return v2) (fun b ->
            Lwt.bind (Lwt.return v3) (fun c ->
                Lwt.bind (Lwt.return v4) (fun d -> binding_body))))
  
  let _ =
    let __ppx_lwt_0 = v1 and __ppx_lwt_1 = v2 in
    Lwt.bind __ppx_lwt_1 (fun n2 ->
        Lwt.bind __ppx_lwt_0 (fun n1 ->
            let __ppx_lwt_0 = v1
            and __ppx_lwt_1 = v2
            and __ppx_lwt_2 = v3
            and __ppx_lwt_3 = v4 in
            Lwt.bind __ppx_lwt_3 (fun (v4 : t) ->
                Lwt.bind __ppx_lwt_2 (fun v3 ->
                    Lwt.bind __ppx_lwt_1 (fun n2 ->
                        Lwt.bind __ppx_lwt_0 (fun n1 ->
                            let __ppx_lwt_0 = (v5 : t :> t')
                            and __ppx_lwt_1 = (v6 : t :> t') in
                            Lwt.bind __ppx_lwt_1 (fun v6 ->
                                Lwt.bind __ppx_lwt_0 (fun v5 -> ()))))))))
  
  (* Let bindings in different contextes. *)
  let _ =
    Lwt.bind
      (Lwt.bind c (fun b -> d))
      (fun a ->
        Lwt.bind
          (Lwt.try_bind
             (fun () -> x)
             (function c -> Lwt.bind b (fun a -> c))
             (function E -> () | exc -> Lwt.reraise exc))
          (fun _ ->
            Lwt.bind
              (Lwt.catch (fun () -> Lwt.bind b (fun a -> c)) (fun _ -> d))
              (fun () -> Lwt.bind x (fun x -> ()))))

  $ cat use_lwt_bind/dont_use_bind.ml
  (* [let*] is not used in this file and [open Lwt.Syntax] is not needed. *)
  let x = 42

  $ cat use_lwt_bind/already_had_open.ml
  open Lwt.Syntax
  
  (* This file already has [open Lwt.Syntax], no need to add one more. *)
  
  let _ = Lwt.bind b (fun a -> c)
