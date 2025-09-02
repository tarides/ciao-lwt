open Cmdliner

(** Command-line options *)

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

let opt_eio_env_as_fiber_var =
  let doc =
    "Eio only: Pass the environment as a Fiber variable. It will be queried \
     everytime the environment is needed. Argument must be a fully-qualified \
     OCaml identifier pointing to a value of type 'Switch.t Fiber.key'."
  in
  Arg.(
    value
    & opt (some ident_conv) None
    & info ~doc ~docv:"Fiber.key" [ "eio-env-as-fiber-var" ])

let opt_migrate =
  let doc = "Modify the source code instead of printing occurrences." in
  Arg.(value & flag & info ~doc [ "migrate" ])

(** Commands *)

module To_eio = struct
  let run migrate eio_sw_as_fiber_var eio_env_as_fiber_var =
    let backend =
      To_direct_style.Concurrency_backend.eio ~eio_sw_as_fiber_var
        ~eio_env_as_fiber_var
    in
    let modify_ast ~fname =
      To_direct_style.Ast_rewrite.rewrite_lwt_uses ~fname ~backend
    in
    let units = function
      | "Lwt" | "Js_of_ocaml_lwt" -> true
      | unit ->
          String.starts_with ~prefix:"Lwt_" unit
          || String.starts_with ~prefix:"Js_of_ocaml_lwt_" unit
    in
    let packages = [ "lwt"; "lwt.unix"; "js_of_ocaml-lwt" ] in
    if migrate then Migrate_utils.migrate ~packages ~units ~modify_ast
    else Migrate_utils.print_occurrences ~packages ~units

  let term =
    Term.(
      term_result
        (const run $ opt_migrate $ opt_eio_sw_as_fiber_var
       $ opt_eio_env_as_fiber_var))

  let info =
    let doc =
      "Generate a hashtable mapping identifiers to number of occurrences, as \
       computed from the implementations of .odocl files found in the given \
       directories."
    in
    Cmd.info "to-eio" ~doc

  let cmd = Cmd.v info term
end

module To_logs = struct
  let run migrate =
    let modify_ast = To_logs.modify_ast in
    let units = function
      | "Lwt_log" | "Lwt_daemon" | "Lwt_log_core" | "Lwt_log_rules"
      | "Lwt_log_js" ->
          true
      | _ -> false
    in
    let packages = [ "lwt_log"; "lwt_log.core"; "js_of_ocaml-lwt.logger" ] in
    if migrate then Migrate_utils.migrate ~packages ~units ~modify_ast
    else Migrate_utils.print_occurrences ~packages ~units

  let term = Term.(term_result (const run $ opt_migrate))

  let info =
    let doc = "Migrate your codebase from Lwt_log to Logs." in
    Cmd.info "to-logs" ~doc

  let cmd = Cmd.v info term
end

let cmd =
  let doc =
    "Migrate your codebase from Lwt to direct-style concurrency libraries."
  in
  let info = Cmd.info "ciao-lwt" ~version:"%%VERSION%%" ~doc in
  Cmd.group info [ To_eio.cmd; To_logs.cmd ]

let () = exit (Cmd.eval cmd)
