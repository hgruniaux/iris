(* Copyright (C) 2025 Hubert Gruniaux
 * This file is part of Iris. *)

open Tast

type t

exception Duplicate_value of tvalue_decl
exception Duplicate_typedef of ttypedef_declaration
exception Duplicate_struct_union of tstruct_declaration

val create : unit -> t
(** Creates a new, empty symbol table. *)

val push_scope : t -> unit
val pop_scope : t -> unit

val in_global_scope : t -> bool
(** Returns [true] if the symbol table is currently in the global scope. *)

val add_value : t -> string -> tvalue_decl -> unit
(** Adds a value declaration to the symbol table's current scope. If a value
    with the same name already exists in the current scope, raises
    [Duplicate_value]. *)

val replace_value : t -> string -> tvalue_decl -> unit
(** Replaces a value declaration in the symbol table's current scope. If a value
    with the given name does not exist in the current scope, then it is added.
*)

val lookup_value : t -> string -> tvalue_decl option
(** Looks up a value declaration by name in the symbol table. *)

val lookup_local_value : t -> string -> tvalue_decl option
(** Looks up a value declaration by name in the current scope of the symbol
    table. Do not look up in parent scopes. *)

val add_typedef : t -> string -> ttypedef_declaration -> unit
(** Adds a typedef declaration to the symbol table's current scope. If a typedef
    with the same name already exists in the current scope, raises
    [Duplicate_typedef]. *)

val lookup_typedef : t -> string -> ttypedef_declaration option
(** Looks up a typedef declaration by name in the symbol table. *)

val lookup_local_typedef : t -> string -> ttypedef_declaration option
(** Looks up a typedef declaration by name in the current scope of the symbol
    table. Do not look up in parent scopes. *)

val add_struct_union :
  is_union:bool -> t -> string -> tstruct_declaration -> unit
(** Adds a struct or union declaration to the symbol table's current scope. If a
    struct or union with the same name already exists in the current scope,
    raises [Duplicate_struct_union]. *)

val lookup_struct_union :
  is_union:bool -> t -> string -> tstruct_declaration option
(** Looks up a struct or union declaration by name in the symbol table. *)

val lookup_local_struct_union :
  is_union:bool -> t -> string -> tstruct_declaration option
(** Looks up a struct or union declaration by name in the current scope of the
    symbol table. Do not look up in parent scopes. *)
