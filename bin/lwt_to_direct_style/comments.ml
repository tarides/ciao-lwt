(** Add comments in the output. Comments are attached to a location that must
    appear in the output AST and cannot be a ghost location. *)

let _acc = ref []

let add loc cmt =
  let cmt = " TODO: lwt-to-direct-style: " ^ cmt ^ " " in
  _acc := Ocamlformat_utils.Cmt.create_comment cmt loc :: !_acc

let get () =
  let r = !_acc in
  _acc := [];
  r
