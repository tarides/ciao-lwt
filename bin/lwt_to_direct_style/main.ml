let main migrate eio_sw_as_fiber_var =
  let backend = Concurrency_backend.eio ~eio_sw_as_fiber_var in
  let modify_ast ~fname = Ast_rewrite.rewrite_lwt_uses ~fname ~backend in
  let units = function
    | "Lwt" -> true
    | unit -> String.starts_with ~prefix:"Lwt_" unit
  in
  let packages = [ "lwt"; "lwt.unix" ] in
  if migrate then Migrate_utils.migrate ~packages ~units ~modify_ast
  else Migrate_utils.print_occurrences ~packages ~units

open Cmdliner

let ident_conv =
  let open Ocamlformat_utils.Parsing in
  let parse s =
    try Ok (Parse.longident ~ocaml_version:None (Lexing.from_string s))
    with _ -> Error (`Msg "Invalid identifier")
  in
  Arg.conv ~docv:"IDENTIFIER" (parse, Printast.fmt_longident)

let opt_eio_sw_as_fiber_var =
  let doc =
    "Eio only: Pass the active switch as a Fiber variable. It will be queried \
     everytime a '~sw' argument must be passed. Argument must be a \
     fully-qualified OCaml identifier pointing to a value of type 'Switch.t \
     Fiber.key'."
  in
  Arg.(
    value
    & opt (some ident_conv) None
    & info ~doc ~docv:"Fiber.key" [ "eio-sw-as-fiber-var" ])

let opt_migrate =
  let doc = "Modify the source code instead of printing occurrences of Lwt." in
  Arg.(value & flag & info ~doc [ "migrate" ])

let cmd =
  let doc =
    "Migrate your codebase from Lwt to direct-style concurrency libraries."
  in
  let info = Cmd.info "lwt-to-direct-style" ~version:"%%VERSION%%" ~doc in
  Cmd.v info
    Term.(term_result (const main $ opt_migrate $ opt_eio_sw_as_fiber_var))

let () = exit (Cmd.eval cmd)
