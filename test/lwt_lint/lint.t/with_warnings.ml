(* Ignored values *)

let _ = ()
let _foo = ()
let _fst, x = ()
let (_fst : unit), _snd = ((), ())

let () =
  let _ = () in
  let _foo = () in
  let _fst, x = () in
  ignore x;
  ()
