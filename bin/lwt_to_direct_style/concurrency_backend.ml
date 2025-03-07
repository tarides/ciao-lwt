open Ocamlformat_utils.Parsing

(* open Asttypes *)
open Parsetree

(* open Ast_helper *)
open Ocamlformat_utils.Ast_utils

type t = {
  both : left:expression -> right:expression -> expression;
      (** Transform [Lwt.both]. *)
  extra_opens : Longident.t list;  (** Opens to add at the top of the module. *)
}

module Eio = struct
  let both ~left ~right = mk_apply_simple [ "Fiber"; "pair" ] [ left; right ]
  let extra_opens = [ mk_longident' [ "Eio" ] ]
end

let eio = Eio.{ both; extra_opens }
