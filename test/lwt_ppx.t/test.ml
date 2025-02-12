let _ =
  let%lwt binding_name = Lwt.return binding_value in
  binding_body

let _ = match%lwt input with case -> ()
let _ = match%lwt input with case -> () | case2 -> ()
let _ = try%lwt input with case -> ()
let _ = try%lwt input with case -> () | case2 -> ()

let _ =
  for%lwt pat = 0 to 10 do
    loop_body
  done;
  for%lwt pat = 10 downto 0 do
    loop_body
  done;
  while%lwt while_condition do
    loop_body
  done;
  ()

let _ =
  stmt_1 ;%lwt
  stmt_2 ;%lwt
  stmt_3 ;%lwt
  stmt_4

let _ =
  assert%lwt false;
  assert%lwt (e = 1)
