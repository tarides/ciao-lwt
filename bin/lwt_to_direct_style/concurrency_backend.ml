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
  pause : unit -> expression;
  extra_opens : Longident.t list;  (** Opens to add at the top of the module. *)
  choose_comment_hint : string;
}

module Eio = struct
  let both ~left ~right = mk_apply_simple [ "Fiber"; "pair" ] [ left; right ]
  let pick lst = mk_apply_simple [ "Fiber"; "any" ] [ lst ]

  let async process_f =
    Comments.add !default_loc "[sw] must be propagated here.";
    Exp.apply
      (mk_exp_ident [ "Fiber"; "fork" ])
      [ (Labelled (mk_loc "sw"), mk_exp_ident [ "sw" ]); (Nolabel, process_f) ]

  let join lst = mk_apply_simple [ "Fiber"; "all" ] [ lst ]
  let pause () = mk_apply_simple [ "Fiber"; "yield" ] [ mk_unit_val ]
  let extra_opens = [ mk_longident' [ "Eio" ] ]
  let choose_comment_hint = "Use Eio.Promise instead. "
end

let eio =
  Eio.{ both; pick; join; async; pause; extra_opens; choose_comment_hint }
