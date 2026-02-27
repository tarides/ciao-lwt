let tpat_alias_ident = function
  | Ocaml_typing.Typedtree.Tpat_alias (_, ident, _, _, _) -> Some ident
  | _ -> None
