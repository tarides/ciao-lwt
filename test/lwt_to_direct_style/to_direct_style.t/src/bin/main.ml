open Lwt.Syntax

let main () =
  let* () = Lwt_fmt.printf "Main.main" in
  let+ () = Test.test () in
  ()

let () = Lwt_main.run (main ())
