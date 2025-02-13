# WIP: An experimental tool to remove usages of `lwt_ppx`

Usage:
```
$ lwt-ppx-to-let-syntax .
```

This will recursively scan the current directory and modify every `.ml` files.

## Known caveats

- The tool uses OCamlformat for pretty printing after having changed the code, which might reformat the entire codebase.
- Let bindings with coercion are not translated due to a bug in OCamlformat, for example `let%lwt x : t :> t' = y in`.
