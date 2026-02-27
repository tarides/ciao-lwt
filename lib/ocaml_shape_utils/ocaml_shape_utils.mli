open Ocaml_typing

module Decl : sig
  type t = Shape.Uid.t * Ident.t option * Typedtree.item_declaration

  val decl_kind_to_string : Typedtree.item_declaration -> string
  val pp : Format.formatter -> t -> unit
end

module Shap : sig
  (** Not named [Shape] to avoid confusion with the module from merlin's lib,
      which is also used elsewhere. *)

  type t

  val reduce : t -> Shape.Uid.t option
  val value : t -> Ident.t -> t
  val type_ : t -> Ident.t -> t
  val extension_constructor : t -> Ident.t -> t
  val class_ : t -> Ident.t -> t
  val class_type : t -> Ident.t -> t
  val module_ : t -> Ident.t -> t
  val module_type : t -> Ident.t -> t
  val pp : Format.formatter -> t -> unit
end

module Def_to_decl : sig
  (** Map definition UIDs (from the interface) to declaration UIDs (from the
      implementation), that can be looked up in indexes. *)

  type t

  val find : Shape.Uid.t -> t -> Shape.Uid.t list
end

type cmt = {
  unit_name : string;
  path : string;
  decls : Decl.t list;
  intf : Cmi_format.cmi_infos option;
      (** [None] if a [.mli] is present, unless [~read_cmti:true] is passed. *)
  shape : Shap.t;
  def_to_decl : Def_to_decl.t;
}

val cmts_of_packages :
  packages:string list -> units:(string -> bool) -> cmt list
(** Path to the [.cmt] in the given packages. Cmts for which [units "Unit_name"]
    return [false] are not collected. Uses [ocamlfind]. *)

val cmt_of_path : ?read_cmti:bool -> Fpath.t -> cmt option
(** If [~read_cmti] is [true] (defaults to [false]), the corresponding [cmti]
    file in the same directory is also read. *)

val pp : Format.formatter -> cmt -> unit
