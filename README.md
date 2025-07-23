# A collection of tools for migrating away from Lwt

This is being developped as part of the project to migrate Ocsigen to
direct-style concurrency. See the relevant Discuss post:
https://discuss.ocaml.org/t/ann-ocsigen-migrating-to-effect-based-concurrency/16327

The tools in the collection are:

- [lwt-ppx-to-let-syntax](#remove-usages-of-lwt_ppx): Remove usages of `lwt_ppx`.
  These are replaced by Lwt library function calls.

- [lwt_lint](#find-implicit-forks): Find implicit forks

- [ciao-lwt to-logs](#migrate-from-Lwt_log-to-Logs): Migrate from `Lwt_log` to `Logs`.

- [ciao-lwt to-eio](#migrate-from-Lwt-to-Eio): Migrate from `Lwt` to `Eio`.

## Installation

Using Opam:

```
opam install .
```

Make sure to install the tools in the Opam switch used to build your project.

## Documentation

### Remove usages of `lwt_ppx`

Usage:
```
$ dune fmt # Make sure the project is formatted to avoid unrelated diffs
$ lwt-ppx-to-let-syntax .
$ dune fmt # Remove formatting changes created by the tool
```

This will recursively scan the current directory and modify all `.ml` files.
Expressions using `let%`, `match%lwt`, `if%lwt`, `[%lwt.finally ..]`, etc..
will be rewritten using the equivalent Lwt library functions.

For example, this expression:
```ocaml
let _ =
  match%lwt x with
  | A -> y
  | B -> z
```

is rewritten:
```ocaml
let _ =
  Lwt.bind x (function
    | A -> y
    | B -> z)
```

To make the new code more idiomatic and closer to the original code, `let%lwt`
is rewritten as `let*`. This example:
```ocaml
let _ =
  let%lwt x = y in
  ..
```

is rewritten to:
```ocaml
open Lwt.Syntax

let _ =
  let* x = y in
  ..
```

To disable this behaviour, eg. if `let*` is unwanted or already being used
for something else, use the `--use-lwt-bind` flag.
For example, the previous example rewritten using `lwt-ppx-to-let-syntax
--use-lwt-bind file.ml` is:
```ocaml
let _ =
  Lwt.bind x (fun y ->
    ..)
```

#### Known caveats

- The tool uses OCamlformat to print the changed code, which may reformat the
  entire codebase.
- Let bindings with coercion are not translated due to a bug in OCamlformat,
  for example `let%lwt x : t :> t' = y in`.
- Backtraces are less accurate. In addition to adding a shorter syntax,
  `lwt_ppx` also helped generate better backtraces in case of an exception
  within asynchronous code. This is removed to avoid poluting the codebase.

### Find implicit forks

This tool warns about values bound to `let _` or passed to `ignore` that do not have a type annotation.
The type annotations help find ignored Lwt threads, which are otherwise a challenge to translate into direct-style concurrency.

Usage:
```
$ lwt-lint .
```

To fix the warnings, add type annotations on `let _` and `ignore` expressions and wrap implicit forks with `Lwt.async (fun () -> ...)`.

### Migrate from `Lwt_log` to `Logs`

Usage:
```
$ dune fmt # Make sure the project is formatted to avoid unrelated diffs
$ dune build @ocaml-index # Build the index (required)
$ ciao-lwt to-logs --migrate .
$ dune fmt # Remove formatting changes created by the tool
```

This will rewrite files containing occurrences of `Lwt_log` and `Lwt_log_js`.
It must be run from the directory containing Dune's `_build`.

An example of use can be found here:
https://github.com/ocsigen/ocsigenserver/pull/256

#### Other changes

- The `Fatal` log level doesn't exist in Logs. `Error` is used instead.

- Log messages are formatted using `Format`. Logging code that uses `%a` and
  `%t` may need to be tweaked manually.

- Syslog support provided by the "logs-syslog" library.

- The `~exn` argument in logging functions is rewritten as a call to
  `Printexc.to_string`. The output may be different.

- The `broadcast` and `dispatch` functions are not immediately available in
  `Logs`. They are implemented by generating more code.

#### Known caveats

- The tool uses OCamlformat to print the modified code, which may reformat the
  entire codebase.

- There is no equivalent to the `~logger` argument in logging functions.
  Logging to a specific reporter is not possible with Logs.

- There is no equivalent to the `~location` argument in logging functions.

- There is no equivalent to the `~template` argument in loggers. This
  functionality must be rewritten by hand.

- There is no equivalent to the `~inspect` argument in `Lwt_log_js` functions.

- There is no equivalent to `Lwt_log.add_rule` in `Logs`. Basic use cases can
  be covered by `Logs.Src.set_level`. Advanced use cases must be implemented
  using a custom `reporter`.

- There is no equivalent to `Lwt_log.close`. Closing must be handled in the
  application code, if necessary.

### Migrate from `Lwt` to `Eio`

Usage:
```
$ dune fmt # Make sure the project is formatted to avoid unrelated diffs
$ dune build @ocaml-index # Build the index (required)
$ ciao-lwt to-eio --migrate .
$ dune fmt # Remove formatting changes created by the tool
```

This will rewrite any files containing occurrences of `Lwt` or other lwt
modules. It must be run from the directory containing Dune's `_build`.

Usages of `Lwt` are rewritten to use `Eio` instead. The tool can be adapted to
support other concurrency libraries, see
[`Concurrency_backend`](bin/lwt_to_direct_style/concurrency_backend.ml).

This works on both the syntax and type levels:
- OCamlformat is used to parse and print the code. Transformations are done on the AST.
  See [`Ocamlformat_utils`](lib/ocamlformat_utils/ocamlformat_utils.mli)
- Merlin is used to detect occurrences of the indentifiers that we want to rewrite using indexes.
  See [`Migrate_utils`](lib/migrate_utils/migrate_utils.mli)

#### Transformation to Direct-style

Translating code from Lwt to direct-style means transforming binds
(`Lwt.bind`, `let*`, etc.) into simple `let` and removing uses of
`Lwt.return`. Concurrency is assured by libraries like Eio, which no longer
require the bind and return operations.

This code:
```ocaml
let _ =
  let* x = f 1 in
  let+ y = f 2 in
  Lwt.bind (f 3) (fun z ->
    Lwt.return (x + y + z))
```
is changed to:
```ocaml
let _ =
  let x = f 1 in
  let y = f 2 in
  let z = f 3 in
  x + y + z
```

Other expressions are also simplified, like `Lwt.catch` and `Lwt.fail`,
`Lwt_list.iter_s`, binding operators, and more.

#### Concurrency

Concurrency must now be created by defining explicit fork points (using
`Eio.Fiber`) but code written for Lwt doesn't define them.
With Lwt, forks can happen everywhere and every `_ Lwt.t` value is a potential
promise.

This is the part of the process that requires the most manual intervention to
make the transition successful.

##### Forks

For example, this is a fork:
```ocaml
let _ =
  let a = operation_1 () in
  let* b = operation_2 () in
  let* a = a in
  Lwt.return (a + b)
```
`operation_1 ()` and `operation_2 ()` run concurrently but if we naively remove
binds and returns we generate code where the two operations run sequentially:
```ocaml
let _ =
  let a = operation_1 () in
  let b = operation_2 () in
  let a = a in
  a + b
```

The correct transformation is:
```ocaml
let _ =
  let a, b = Eio.Fiber.pair operation_1 operation_2 in
  a + b
```
Unfortunately, the tool is not able to generate the correct code in this case.

Explicit forks are handled correctly, like `Lwt.pick`, `Lwt.both` and `Lwt.async`.

##### Promises

Every `_ Lwt.t` value is a promise but transforming all of them to a
`Eio.Promise.t` would be extremely impractical and against the goal of doing
direct-style concurrency.
Instead, only `_ Lwt.t` values that are not directly `bind` to are considered
promises. This includes `_ Lwt.t` values that are part of a bigger value (eg.
in a tuple, record or hashtbl).

For example, this is a promise:
```ocaml
type t = { p : int Lwt.t }
let x = { p = operation_1 () }
```
The tool is not able to generate the right code:
```ocaml
open Eio.Std
type t = { p : int Promise.t }
let x = { p = operation_1 () }
```
You'll have to rely on the types to catch the missing fork. The correct code is:
```ocaml
open Eio.Std
type t = { p : (int, exn) result Promise.t }
let x = { p = Fiber.fork_promise ~sw (fun () -> operation_1 ()) }
```

This can be harder to debug when combined with implicit forks. For example, the
tool will completely change the meaning of the function `f` without modifying
its code:
```ocaml
(* before: start a concurrent thread and return a [int Lwt.t option]. *)
let f () = Some (operation_1 ())
(* after: wait for the operation to complete and return a [int option]. *)
let f () = Some (operation_1 ())
```

##### Other caveats

- Arguments to `Lwt.pick` and `Lwt.both` must now be suspended in a
  `(fun () -> ...)` expression, which was not needed before.
  Code like this:
  ```ocaml
  let _ =
    let thread_1 = ... in
    let thread_2 = Lwt.bind thread_1 (fun _ -> ...) in
    let thread_3 = ... in
    Lwt.both
  ```
  is transformed to:
  ```ocaml
  let _ =
    let thread_1 = Format.printf "1" in
    let thread_2 =
      let _ = thread_1 in
      Format.printf "2"
    in
    let thread_3 = Format.printf "3" in
    Fiber.pair
      (fun () ->
        thread_2
        (* TODO: ciao-lwt: This computation might not be suspended correctly. *))
      (fun () ->
        thread_3
        (* TODO: ciao-lwt: This computation might not be suspended correctly. *))
  ```

## Contribution

Contributions are most welcome!

- [File issues](https://github.com/tarides/ciao-lwt/issues) to report bugs or feature requests.
- [Contribute code or documentation](./CONTRIBUTING.md)

---

This project is created and maintained by\
<a href="https://tarides.com/"><img src="./Tarides.svg" width="200" alt="Tarides" /></a>

