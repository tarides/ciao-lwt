open Ocamlformat_utils.Parsing

(* open Asttypes *)
open Parsetree

(* open Ast_helper *)
open Ocamlformat_utils.Ast_utils

type t = {
  both : left:expression -> right:expression -> expression;
      (** Transform [Lwt.both]. *)
  choose : expression -> expression;
  extra_opens : Longident.t list;  (** Opens to add at the top of the module. *)
}

module Eio = struct
  let both ~left ~right = mk_apply_simple [ "Fiber"; "pair" ] [ left; right ]
  let choose lst = mk_apply_simple [ "Fiber"; "any" ] [ lst ]
  let extra_opens = [ mk_longident' [ "Eio" ] ]
end

let eio = Eio.{ both; choose; extra_opens }
