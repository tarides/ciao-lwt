let _ =
  let%lwt binding_name = Lwt.return binding_value in
  binding_body

let _ =
  let%lwt n1 : t = v1 in
  let%lwt n2 : t :> t' = v2 in
  let%lwt n3 = n3 in
  ()

let _ =
  let%lwt n1 = v1 and n2 = v2 in
  let%lwt n1 = v1
  and n2 = v2
  and v3 = v3
  and v4 : t = v4
  and v5 : t :> t' = v5 in
  ()

let _ = match%lwt input with case -> ()
let _ = match%lwt input with case -> () | case2 -> ()
let _ = match%lwt input with case -> () | exception E -> ()
let _ = match%lwt input with exception E -> ()
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
  stmt_1;%lwt
  stmt_2;%lwt
  stmt_3;%lwt
  stmt_4

let _ =
  assert%lwt false;
  assert%lwt (e = 1)

let _ =
  if%lwt cond then a else b;
  if%lwt cond then a;
  if%lwt cond1 then a else if cond2 then b else c;
  if%lwt cond1 then a else if%lwt cond2 then b else c

let _ =
  [%lwt
    let a = b in
    c]
