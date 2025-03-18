open Ocamlformat_utils.Parsing
open Asttypes

(* open Parsetree *)
open Ast_helper
open Ocamlformat_utils.Ast_utils

let eio () =
  let used_eio_std = ref false in
  let fiber_ident i =
    used_eio_std := true;
    [ "Fiber"; i ]
  in
  let promise_ident i =
    used_eio_std := true;
    [ "Promise"; i ]
  in
  object
    method both ~left ~right =
      mk_apply_simple (fiber_ident "pair") [ left; right ]

    method pick lst = mk_apply_simple (fiber_ident "any") [ lst ]

    method async process_f =
      Comments.add_default_loc "[sw] must be propagated here.";
      Exp.apply
        (mk_exp_ident (fiber_ident "fork"))
        [
          (Labelled (mk_loc "sw"), mk_exp_ident [ "sw" ]); (Nolabel, process_f);
        ]

    method wait () =
      Comments.add_default_loc
        "Translation is incomplete, [Promise.await] must be called on the \
         promise when it's part of control-flow.";
      mk_apply_simple (promise_ident "create") [ mk_unit_val ]

    method wakeup u arg = mk_apply_simple (promise_ident "resolve") [ u; arg ]
    method join lst = mk_apply_simple (fiber_ident "all") [ lst ]
    method pause () = mk_apply_simple (fiber_ident "yield") [ mk_unit_val ]

    method extra_opens =
      if !used_eio_std then [ mk_longident' [ "Eio"; "Std" ] ] else []

    method choose_comment_hint = "Use Eio.Promise instead. "

    method list_parallel =
      function
      | ("filter" | "filter_map" | "map" | "iter") as ident ->
          Some (fiber_ident "List" @ [ ident ])
      | ident ->
          Comments.add_default_loc
            ("[" ^ ident
           ^ "] can't be translated automatically. See \
              https://ocaml.org/p/eio/latest/doc/Eio/Fiber/List/index.html");
          None

    method sleep d = mk_apply_simple [ "Eio_unix"; "sleep" ] [ d ]

    method with_timeout d f =
      Comments.add_default_loc "[env] must be propagated from the main loop";
      let clock = Exp.send (mk_exp_ident [ "env" ]) (mk_loc "mono_clock") in
      mk_apply_simple [ "Eio"; "Time"; "with_timeout_exn" ] [ clock; d; f ]

    method timeout_exn =
      Pat.construct (mk_longident [ "Eio"; "Time"; "Timeout" ]) None

    method condition_create () =
      mk_apply_simple [ "Eio"; "Condition"; "create" ] [ mk_unit_val ]

    method condition_wait mutex cond =
      mk_apply_simple [ "Eio"; "Condition"; "await" ] [ cond; mutex ]

    method cancel_message =
      "Use [Switch] or [Cancel] for defining a cancellable context."

    method state p = mk_apply_simple (promise_ident "peek") [ p ]

    method mutex_create () =
      mk_apply_simple [ "Eio"; "Mutex"; "create" ] [ mk_unit_val ]

    method mutex_lock m = mk_apply_simple [ "Eio"; "Mutex"; "lock" ] [ m ]
    method mutex_unlock m = mk_apply_simple [ "Eio"; "Mutex"; "unlock" ] [ m ]

    method mutex_with_lock t f =
      mk_apply_ident
        [ "Eio"; "Mutex"; "use_rw" ]
        [
          (mk_lbl "protect", mk_constr_exp "false"); (Nolabel, t); (Nolabel, f);
        ]

    method key_new () =
      mk_apply_simple (fiber_ident "create_key") [ mk_unit_val ]

    method key_get key = mk_apply_simple (fiber_ident "get") [ key ]

    method key_with_value key val_opt f =
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
  end
