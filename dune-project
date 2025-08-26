(lang dune 3.17)

(name ciao_lwt)

(generate_opam_files true)

(source
 (github tarides/ciao-lwt))

(authors
 "Jules Aguillon <jules@j3s.fr>"
 "Nicolas Osborne <nicolas.osborne@tarides.com>")

(maintainers
 "Jules Aguillon <jules@j3s.fr>"
 "Nicolas Osborne <nicolas.osborne@tarides.com>")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name ciao_lwt)
 (synopsis
  "A tool for migrating from Lwt to direct-style concurrency libraries")
 (depends
  ocaml
  cmdliner
  merlin-lib
  (lwt :with-test)
  (js_of_ocaml-lwt :with-test)
  (ocaml-index :with-test)
  ; OCamlformat depends
  (ocaml
   (>= 4.08))
  (base
   (>= v0.12.0))
  (cmdliner
   (>= 1.1.0))
  dune-build-info
  either
  fix
  (fpath
   (>= 0.7.3))
  (menhir
   (>= 20201216))
  (menhirLib
   (>= 20201216))
  (menhirSdk
   (>= 20201216))
  (ocaml-version
   (>= 3.5.0))
  (ocp-indent
   (>= 1.8.0))
  stdio
  (uuseg
   (>= 10.0.0))
  (uutf
   (>= 1.0.1))
  (csexp
   (>= 1.4.0))
  astring
  camlp-streams
  re))

(package
 (name lwt_ppx_to_let_syntax)
 (synopsis "A tool for migrating away from lwt_ppx")
 (depends
  ocaml
  cmdliner
  ; OCamlformat depends
  (ocaml
   (>= 4.08))
  (base
   (>= v0.12.0))
  (cmdliner
   (>= 1.1.0))
  dune-build-info
  either
  fix
  (fpath
   (>= 0.7.3))
  (menhir
   (>= 20201216))
  (menhirLib
   (>= 20201216))
  (menhirSdk
   (>= 20201216))
  (ocaml-version
   (>= 3.5.0))
  (ocp-indent
   (>= 1.8.0))
  stdio
  (uuseg
   (>= 10.0.0))
  (uutf
   (>= 1.0.1))
  (csexp
   (>= 1.4.0))
  astring
  camlp-streams
  re))

(package
 (name lwt_lint)
 (synopsis "A linter for code that might cause implicit forking in Lwt.")
 (depends
  ; OCamlformat depends
  (ocaml
   (>= 4.08))
  (base
   (>= v0.12.0))
  (cmdliner
   (>= 1.1.0))
  dune-build-info
  either
  fix
  (fpath
   (>= 0.7.3))
  (menhir
   (>= 20201216))
  (menhirLib
   (>= 20201216))
  (menhirSdk
   (>= 20201216))
  (ocaml-version
   (>= 3.5.0))
  (ocp-indent
   (>= 1.8.0))
  stdio
  (uuseg
   (>= 10.0.0))
  (uutf
   (>= 1.0.1))
  (csexp
   (>= 1.4.0))
  astring
  camlp-streams
  re))
