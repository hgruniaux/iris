(* Copyright (C) 2025 Hubert Gruniaux
 * This file is part of Iris. *)

open Tast

type scope = {
  scope_parent : scope option;
  scope_values : (string, tvalue_decl) Hashtbl.t;
  scope_typedefs : (string, ttypedef_declaration) Hashtbl.t;
  scope_structs : (string, tstruct_declaration) Hashtbl.t;
  scope_unions : (string, tstruct_declaration) Hashtbl.t;
}

type t = scope ref

exception Duplicate_value of tvalue_decl
exception Duplicate_typedef of ttypedef_declaration
exception Duplicate_struct_union of tstruct_declaration

let create_scope parent =
  {
    scope_parent = parent;
    scope_values = Hashtbl.create 7;
    scope_typedefs = Hashtbl.create 7;
    scope_structs = Hashtbl.create 7;
    scope_unions = Hashtbl.create 7;
  }

let create () =
  let global_scope = create_scope None in
  ref global_scope

let push_scope t =
  let new_scope = create_scope (Some !t) in
  t := new_scope

let pop_scope t =
  match !t.scope_parent with
  | None -> failwith "No scope to pop"
  | Some parent -> t := parent

let in_global_scope t = Option.is_none !t.scope_parent

let add_value t name decl =
  match Hashtbl.find_opt !t.scope_values name with
  | None -> Hashtbl.replace !t.scope_values name decl
  | Some previous_decl -> raise (Duplicate_value previous_decl)

let replace_value t name decl = Hashtbl.replace !t.scope_values name decl

let lookup t get_table name =
  let rec aux scope =
    match Hashtbl.find_opt (get_table scope) name with
    | Some decl -> Some decl
    | None -> (
        match scope.scope_parent with None -> None | Some parent -> aux parent)
  in
  aux !t

let lookup_local t get_table name = Hashtbl.find_opt (!t |> get_table) name
let get_value_table scope = scope.scope_values
let lookup_value t name = lookup t get_value_table name
let lookup_local_value t name = lookup_local t get_value_table name

let add_typedef t name decl =
  match Hashtbl.find_opt !t.scope_typedefs name with
  | None -> Hashtbl.replace !t.scope_typedefs name decl
  | Some previous_decl -> raise (Duplicate_typedef previous_decl)

let get_typedef_table scope = scope.scope_typedefs
let lookup_typedef t name = lookup t get_typedef_table name
let lookup_local_typedef t name = lookup_local t get_typedef_table name

let get_struct_union_table ~is_union scope =
  if is_union then scope.scope_unions else scope.scope_structs

let add_struct_union ~is_union t name decl =
  match Hashtbl.find_opt (get_struct_union_table ~is_union !t) name with
  | None -> Hashtbl.replace (get_struct_union_table ~is_union !t) name decl
  | Some previous_decl -> raise (Duplicate_struct_union previous_decl)

let lookup_struct_union ~is_union t name =
  lookup t (get_struct_union_table ~is_union) name

let lookup_local_struct_union ~is_union t name =
  lookup_local t (get_struct_union_table ~is_union) name
