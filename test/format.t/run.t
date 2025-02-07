  $ mkdir -p _build .git lib/foo

  $ echo '(* syntax error' > _build/error.ml
  $ echo '(* syntax error' > .git/error.ml

  $ echo 'let x =  42' > lib/foo/foo.ml

  $ lwt-to-eio
  Formatted 1 files, 0 errors

  $ cat lib/foo/foo.ml
  let x = 42
