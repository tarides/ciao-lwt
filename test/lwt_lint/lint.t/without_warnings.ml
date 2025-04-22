(* Ignored values *)

let _ : unit = ()
let _foo : unit = ()
let (_fst, x) : unit * unit = (), ()
let ((_fst : unit), x) = (), ()

let () =
  let _ : unit = () in
  let _foo : unit = () in
  let (_fst, x) : unit * unit = (), () in
  let ((_fst : unit), x) = (), () in
  ()
