open Ocamlformat_utils.Parsing
open Asttypes
open Parsetree
open Ast_helper
open Ocamlformat_utils.Ast_utils

type t = {
  both : left:expression -> right:expression -> expression;
      (** Transform [Lwt.both]. *)
  pick : expression -> expression;
  join : expression -> expression;
  async : expression -> expression;
  wait : unit -> expression;
  wakeup : expression -> expression -> expression;
  pause : unit -> expression;
  extra_opens : Longident.t list;  (** Opens to add at the top of the module. *)
  choose_comment_hint : string;
  list_parallel : string -> string list option;
      (** Given a function name from [Lwt_list] with the [_p] suffix removed,
          return an identifier. *)
}

module Eio = struct
  let both ~left ~right = mk_apply_simple [ "Fiber"; "pair" ] [ left; right ]
  let pick lst = mk_apply_simple [ "Fiber"; "any" ] [ lst ]

  let async process_f =
    Comments.add_default_loc "[sw] must be propagated here.";
    Exp.apply
      (mk_exp_ident [ "Fiber"; "fork" ])
      [ (Labelled (mk_loc "sw"), mk_exp_ident [ "sw" ]); (Nolabel, process_f) ]

  let wait () =
    Comments.add_default_loc
      "Translation is incomplete, [Promise.await] must be called on the \
       promise when it's part of control-flow.";
    mk_apply_simple [ "Promise"; "create" ] [ mk_unit_val ]

  let wakeup u arg = mk_apply_simple [ "Promise"; "resolve" ] [ u; arg ]
  let join lst = mk_apply_simple [ "Fiber"; "all" ] [ lst ]
  let pause () = mk_apply_simple [ "Fiber"; "yield" ] [ mk_unit_val ]
  let extra_opens = [ mk_longident' [ "Eio" ] ]
  let choose_comment_hint = "Use Eio.Promise instead. "

  let list_parallel = function
    | ("filter" | "filter_map" | "map" | "iter") as ident ->
        Some [ "Fiber"; "List"; ident ]
    | ident ->
        Comments.add_default_loc
          ("[" ^ ident
         ^ "] can't be translated automatically. See \
            https://ocaml.org/p/eio/latest/doc/Eio/Fiber/List/index.html");
        None
end

let eio =
  Eio.
    {
      both;
      pick;
      join;
      async;
      pause;
      wait;
      wakeup;
      extra_opens;
      choose_comment_hint;
      list_parallel;
    }
