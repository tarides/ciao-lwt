open Ocamlformat_utils.Parsing
open Asttypes
open Parsetree
open Ast_helper
open Ocamlformat_utils.Ast_utils

let eio ~eio_sw_as_fiber_var ~eio_env_as_fiber_var add_comment =
  let used_eio_std = ref false in
  let eio_std_ident mod_ i =
    used_eio_std := true;
    [ mod_; i ]
  in
  let fiber_ident = eio_std_ident "Fiber"
  and promise_ident = eio_std_ident "Promise"
  and switch_ident = eio_std_ident "Switch"
  and std_ident i =
    used_eio_std := true;
    [ i ]
  in
  let add_comment fmt = Format.kasprintf add_comment fmt in
  let add_comment_dropped_exp ~label ?(cmt = "") exp =
    add_comment "Dropped expression (%s): [%s].%s" label
      (Ocamlformat_utils.format_expression exp)
      cmt
  in
  (* If [--eio-sw-as-fiber-var] is passed on the command line, this will query
     the current switch. Otherwise, this will generate a comment.
     Return an expression that can be passed to a [~sw] argument. *)
  let get_current_switch () =
    match eio_sw_as_fiber_var with
    | Some ident ->
        mk_apply_simple
          [ "Stdlib"; "Option"; "get" ]
          [ mk_apply_simple (fiber_ident "get") [ Exp.ident (mk_loc ident) ] ]
    | None ->
        add_comment "[sw] (of type Switch.t) must be propagated here.";
        mk_exp_ident [ "sw" ]
  in
  let get_current_switch_arg () =
    (Labelled (mk_loc "sw"), get_current_switch ())
  in
  let env field =
    let env_exp =
      match eio_env_as_fiber_var with
      | Some ident ->
          mk_apply_simple
            [ "Stdlib"; "Option"; "get" ]
            [ mk_apply_simple (fiber_ident "get") [ Exp.ident (mk_loc ident) ] ]
      | None ->
          add_comment "[env] must be propagated from the main loop";
          mk_exp_ident [ "env" ]
    in
    Exp.send env_exp (mk_loc field)
  in
  let buf_read_of_flow flow =
    mk_apply_ident
      [ "Eio"; "Buf_read"; "of_flow" ]
      [
        (Labelled (mk_loc "max_size"), mk_const_int "1_000_000"); (Nolabel, flow);
      ]
  in
  let buf_write_of_flow flow =
    add_comment
      "Write operations to buffered IO should be moved inside [with_flow].";
    mk_apply_simple
      [ "Eio"; "Buf_write"; "with_flow" ]
      [
        flow;
        mk_fun ~arg_name:"outbuf" (fun _outbuf ->
            mk_variant_exp "Move_writing_code_here");
      ]
  in
  let import_socket_stream ~r_or_w fd =
    (* Used by [input_io] and [output_io]. *)
    Exp.constraint_
      (mk_apply_ident
         [ "Eio_unix"; "Net"; "import_socket_stream" ]
         [
           get_current_switch_arg ();
           (Labelled (mk_loc "close_unix"), mk_constr_exp [ "true" ]);
           (Nolabel, fd);
         ])
      (mk_typ_constr
         ~params:
           [ mk_poly_variant [ (r_or_w, []); ("Flow", []); ("Close", []) ] ]
         (std_ident "r"))
  in
  object
    method both ~left ~right =
      mk_apply_simple (fiber_ident "pair") [ left; right ]

    method pick lst = mk_apply_simple (fiber_ident "any") [ lst ]

    method choose lst =
      let fiber_f =
        let open Mk_function in
        mk_function
          (return (fun p () -> mk_apply_simple (promise_ident "await") [ p ])
          $ arg "p" $ arg_unit)
      in
      add_comment
        "The list [%s] is expected to be a list of promises. Use \
         [Fiber.fork_promise] to make a promise."
        (Ocamlformat_utils.format_expression lst);
      mk_apply_simple (fiber_ident "any")
        [ mk_apply_simple [ "List"; "map" ] [ fiber_f; lst ] ]

    method async process_f =
      Exp.apply
        (mk_exp_ident (fiber_ident "fork"))
        [ get_current_switch_arg (); (Nolabel, process_f) ]

    method wait unit =
      add_comment
        "Translation is incomplete, [Promise.await] must be called on the \
         promise when it's part of control-flow.";
      mk_apply_simple (promise_ident "create") [ unit ]

    method wakeup u arg = mk_apply_simple (promise_ident "resolve") [ u; arg ]
    method join lst = mk_apply_simple (fiber_ident "all") [ lst ]
    method pause unit = mk_apply_simple (fiber_ident "yield") [ unit ]

    method extra_opens =
      if !used_eio_std then [ mk_longident' [ "Eio"; "Std" ] ] else []

    method list_parallel =
      function
      | ("filter" | "filter_map" | "map" | "iter") as ident ->
          Some (fiber_ident "List" @ [ ident ])
      | ident ->
          add_comment
            "[%s] can't be translated automatically. See \
             https://ocaml.org/p/eio/latest/doc/Eio/Fiber/List/index.html"
            ident;
          None

    method sleep d = mk_apply_simple [ "Eio_unix"; "sleep" ] [ d ]

    method with_timeout d f =
      mk_apply_simple
        [ "Eio"; "Time"; "with_timeout_exn" ]
        [ env "mono_clock"; d; f ]

    method timeout_exn = mk_longident [ "Eio"; "Time"; "Timeout" ]

    method condition_create unit =
      mk_apply_simple [ "Eio"; "Condition"; "create" ] [ unit ]

    method condition_wait mutex cond =
      mk_apply_simple [ "Eio"; "Condition"; "await" ] [ cond; mutex ]

    method condition_type param =
      (match param.ptyp_desc with
      | Ptyp_constr ({ txt = Lident "unit"; _ }, []) -> ()
      | _ ->
          add_comment
            "Eio conditions don't carry a value. Use a mutable variable and a \
             dedicated mutex.");
      mk_typ_constr [ "Eio"; "Condition"; "t" ]

    method cancel_message =
      "Use [Switch] or [Cancel] for defining a cancellable context."

    method state p = mk_apply_simple (promise_ident "peek") [ p ]

    method mutex_create unit =
      mk_apply_simple [ "Eio"; "Mutex"; "create" ] [ unit ]

    method mutex_lock m = mk_apply_simple [ "Eio"; "Mutex"; "lock" ] [ m ]
    method mutex_unlock m = mk_apply_simple [ "Eio"; "Mutex"; "unlock" ] [ m ]

    method mutex_with_lock t f =
      mk_apply_ident
        [ "Eio"; "Mutex"; "use_rw" ]
        [
          (mk_lbl "protect", mk_constr_exp [ "false" ]);
          (Nolabel, t);
          (Nolabel, f);
        ]

    method key_new unit = mk_apply_simple (fiber_ident "create_key") [ unit ]
    method key_get key = mk_apply_simple (fiber_ident "get") [ key ]

    method key_with_value key val_opt f =
      match val_opt with
      | `Some val_ ->
          mk_apply_simple (fiber_ident "with_binding") [ key; val_; f ]
      | `None -> mk_apply_simple (fiber_ident "without_binding") [ key; f ]
      | `Exp val_opt ->
          Exp.apply
            (mk_apply_ident [ "Option"; "fold" ]
               [
                 (mk_lbl "none", mk_exp_ident (fiber_ident "without_binding"));
                 ( mk_lbl "some",
                   mk_apply_simple [ "Fun"; "flip" ]
                     [ mk_exp_ident (fiber_ident "with_binding") ] );
                 (Nolabel, val_opt);
               ])
            [ (Nolabel, key); (Nolabel, f) ]

    method promise_type param =
      mk_typ_constr ~params:[ param ] (promise_ident "t")

    method direct_style_type param = param

    method of_unix_file_descr ?blocking:_ fd =
      (* TODO: We don't use [Eio_unix.Fd.t] because there is no conversion to [Flow.sink]. *)
      (* let blocking_arg = *)
      (*   let lbl = mk_loc "blocking" in *)
      (*   match blocking with *)
      (*   | Some (expr, `Lbl) -> [ (Labelled lbl, expr) ] *)
      (*   | Some (expr, `Opt) -> [ (Optional lbl, expr) ] *)
      (*   | None -> [] *)
      (* in *)
      (* mk_apply_ident *)
      (*   [ "Eio_unix"; "Fd"; "of_unix" ] *)
      (*   ([ get_current_switch_arg () ] *)
      (*   @ blocking_arg *)
      (*   @ [ *)
      (*       (Labelled (mk_loc "close_unix"), mk_constr_exp [ "true" ]); *)
      (*       (Nolabel, fd); *)
      (*     ]) *)
      fd

    method io_read ~exactly input buffer buf_offset buf_len =
      add_comment "[%s] should be a [Cstruct.t]."
        (Ocamlformat_utils.format_expression buffer);
      add_comment
        "[Eio.Flow.single_read] operates on a [Flow.source] but [%s] is likely \
         of type [Eio.Buf_read.t]. Rewrite this code to use [Buf_read] (which \
         contains an internal buffer) or change the call to \
         [Eio.Buf_read.of_flow] used to create the buffer."
        (Ocamlformat_utils.format_expression input);
      add_comment_dropped_exp ~label:"buffer offset"
        ~cmt:" This will behave as if it was [0]." buf_offset;
      add_comment_dropped_exp ~label:"buffer length"
        ~cmt:" This will behave as if it was [Cstruct.length buffer]." buf_len;
      let fun_ = if exactly then "read_exact" else "single_read" in
      mk_apply_simple [ "Eio"; "Flow"; fun_ ] [ input; buffer ]

    method io_read_all input =
      mk_apply_simple [ "Eio"; "Buf_read"; "take_all" ] [ input ]

    method io_read_string_count _input _count_arg =
      add_comment
        "Eio doesn't have a direct equivalent of [Lwt_io.read ~count]. Rewrite \
         the code using [Eio.Buf_read]'s lower level API or switch to \
         unbuffered IO.";
      None

    method io_flush output =
      mk_apply_simple [ "Eio"; "Buf_write"; "flush" ] [ output ]

    method fd_close fd =
      (* TODO: See [of_unix_file_descr]. mk_apply_simple [ "Eio_unix"; "Fd" ] [ fd ] *)
      mk_apply_simple [ "Unix"; "close" ] [ fd ]

    method main_run promise =
      let with_binding var_ident x body =
        let var = Exp.ident (mk_loc var_ident) in
        mk_apply_simple (fiber_ident "with_binding") [ var; x; mk_thunk body ]
      in
      add_comment
        "[Eio_main.run] argument used to be a [Lwt] promise and is now a \
         [fun]. Make sure no asynchronous or IO calls are done outside of this \
         [fun].";
      let wrap_sw_fiber_var k =
        match eio_sw_as_fiber_var with
        | Some var_ident ->
            let fun_sw =
              mk_fun ~arg_name:"sw" (fun sw -> with_binding var_ident sw k)
            in
            mk_apply_ident (switch_ident "run")
              [
                (* TODO: Add the [~name] argument. Currently commented-out because added in a too recent version of eio.
                   (Labelled (mk_loc "name"), mk_const_string "main"); *)
                (Nolabel, fun_sw);
              ]
        | None -> k
      in
      let wrap_env_fiber_var env k =
        match eio_env_as_fiber_var with
        | Some var_ident -> with_binding var_ident env k
        | None -> k
      in
      mk_apply_simple [ "Eio_main"; "run" ]
        [
          mk_fun ~arg_name:"env" (fun env ->
              wrap_env_fiber_var env (wrap_sw_fiber_var promise));
        ]

    method input_io =
      function
      | `Of_fd fd -> buf_read_of_flow (import_socket_stream ~r_or_w:"R" fd)
      | `Fname fname ->
          buf_read_of_flow
          @@ mk_apply_ident
               [ "Eio"; "Path"; "open_in" ]
               [
                 get_current_switch_arg ();
                 ( Nolabel,
                   mk_apply_simple [ "Eio"; "Path"; "/" ] [ env "cwd"; fname ]
                 );
               ]

    method output_io =
      function
      | `Of_fd fd -> buf_write_of_flow (import_socket_stream ~r_or_w:"W" fd)
      | `Fname fname ->
          add_comment
            "[flags] and [perm] arguments were dropped. The [~create] was \
             added by default and might not match the previous flags. Use \
             [~append:true] for [O_APPEND].";
          buf_write_of_flow
          @@ mk_apply_ident
               [ "Eio"; "Path"; "open_out" ]
               [
                 get_current_switch_arg ();
                 ( Labelled (mk_loc "create"),
                   mk_variant_exp ~arg:(mk_const_int "0o666") "If_missing" );
                 ( Nolabel,
                   mk_apply_simple [ "Eio"; "Path"; "/" ] [ env "cwd"; fname ]
                 );
               ]

    method io_read_line chan =
      mk_apply_simple [ "Eio"; "Buf_read"; "line" ] [ chan ]

    (* This is of type [Optint.Int63.t] instead of [int] with Lwt. *)
    method io_length fd = mk_apply_simple [ "Eio"; "File"; "size" ] [ fd ]

    method io_write_str chan str =
      mk_apply_simple [ "Eio"; "Buf_write"; "string" ] [ chan; str ]

    method io_close fd = mk_apply_simple [ "Eio"; "Resource"; "close" ] [ fd ]
    method type_in_channel = mk_typ_constr [ "Eio"; "Buf_read"; "t" ]
    method type_out_channel = mk_typ_constr [ "Eio"; "Buf_write"; "t" ]

    method path_stat ~follow path =
      mk_apply_ident [ "Eio"; "Path"; "stat" ]
        [
          (Labelled (mk_loc "follow"), mk_constr_of_bool follow);
          (Nolabel, mk_apply_simple [ "Eio"; "Path"; "/" ] [ env "cwd"; path ]);
        ]

    method domain_detach thunk =
      mk_apply_ident
        (fiber_ident "fork_promise")
        [
          get_current_switch_arg ();
          ( Nolabel,
            mk_thunk
              (mk_apply_simple
                 [ "Eio"; "Domain_manager"; "run" ]
                 [ env "domain_mgr"; thunk ]) );
        ]

    method net_with_connection sockaddr f =
      add_comment
        "[%s] is of type [Unix.sockaddr] but it should be a \
         [Eio.Net.Sockaddr.stream]."
        (Ocamlformat_utils.format_expression sockaddr);
      mk_apply_simple (switch_ident "run")
        [
          mk_fun ~arg_name:"sw" (fun sw ->
              Exp.apply f
                [
                  ( Nolabel,
                    mk_apply_ident
                      [ "Eio"; "Net"; "connect" ]
                      [
                        (Labelled (mk_loc "sw"), sw);
                        (Nolabel, env "net");
                        (Nolabel, sockaddr);
                      ] );
                ]);
        ]

    method jsoo_lwt_js_equivalent = Some [ "Js_of_ocaml_eio"; "Eio_js" ]

    method jsoo_lwt_js_events_equivalent =
      Some [ "Js_of_ocaml_eio"; "Eio_js_events" ]
  end
