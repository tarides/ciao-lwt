# A collection of tools for migrating away from Lwt

This is being developped as part of the project to migrate Ocsigen to
direct-style concurrency. See the relevant Discuss post:
https://discuss.ocaml.org/t/ann-ocsigen-migrating-to-effect-based-concurrency/16327

The tools in the collection are:

- [lwt-ppx-to-let-syntax](#remove-usages-of-lwt_ppx): Remove usages of `lwt_ppx`.
  These are replaced by Lwt library function calls.

## Remove usages of `lwt_ppx`

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

### Known caveats

- The tool uses OCamlformat to print the changed code, which may reformat the
  entire codebase.
- Let bindings with coercion are not translated due to a bug in OCamlformat,
  for example `let%lwt x : t :> t' = y in`.
- Backtraces are less accurate. In addition to adding a shorter syntax,
  `lwt_ppx` also helped generate better backtraces in case of an exception
  within asynchronous code. This is removed to avoid poluting the codebase.
