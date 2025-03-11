(** Add comments in the output. Comments are attached to a location that must
    appear in the output AST and cannot be a ghost location. *)

let _acc = ref []
let default_loc = ref Ocamlformat_utils.Parsing.Location.none

let add loc cmt =
  let cmt = " TODO: lwt-to-direct-style: " ^ cmt ^ " " in
  _acc := Ocamlformat_utils.Cmt.create_comment cmt loc :: !_acc

let add_default_loc cmt = add !default_loc cmt

let get () =
  let r = !_acc in
  _acc := [];
  r
