let _ =
  let%lwt binding_name = Lwt.return binding_value in
  binding_body

let _ =
  let%lwt a = Lwt.return v1 in
  let%lwt b = Lwt.return v2 in
  let%lwt c = Lwt.return v3 in
  let%lwt d = Lwt.return v4 in
  binding_body

let _ =
  let%lwt n1 = v1 and n2 = v2 in
  let%lwt n1 = v1 and n2 = v2 and v3 = v3 and v4 : t = v4 in
  let%lwt v5 : t :> t' = v5 and v6 : t :> t' = v6 in
  ()

(* Let bindings in different contextes. *)
let _ =
  let%lwt a =
    let%lwt b = c in
    d
  in
  let%lwt _ =
    match%lwt x with
    | c ->
        let%lwt a = b in
        c
    | exception E -> ()
  in
  let%lwt () =
    try%lwt
      let%lwt a = b in
      c
    with _ -> d
  in
  let%lwt x = x in
  ()
