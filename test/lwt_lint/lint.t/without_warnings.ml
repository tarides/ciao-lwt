(* Ignored values *)

let _ : unit = ()
let _foo : unit = ()
let (_fst, x) : unit * unit = ((), ())
let (_fst : unit), x = ((), ())
let _f x y = x + y

let () =
  let _ : unit = () in
  let _foo : unit = () in
  let (_fst, x) : unit * unit = ((), ()) in
  let (_fst : unit), x = ((), ()) in
  let _f x y = x + y in
  ignore (x : unit);
  ignore (x : unit :> unit);
  ()
