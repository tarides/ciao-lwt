open Ocaml_typing

(* [tpat_alias_ident tpat] returns [Some ident] if [tpat] is [Tpat_alias _],
   [None] otherwise. *)
val tpat_alias_ident : Typedtree.value Typedtree.pattern_desc -> Ident.t option
