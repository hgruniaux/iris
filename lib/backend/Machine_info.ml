(* Copyright (C) 2025 Hubert Gruniaux
 * This file is part of Iris. *)

open Ir_base

type endianess = Little_endian | Big_endian

let system_endianess = Little_endian

(** Cache for the sizes of IR types.

    We store the sizes of IR types in this cache to avoid recomputing them. Some
    types can be expensive to compute the size for (e.g., structs). *)
let size_of_cache = Hashtbl.create 17

(** Same as [size_of_cache] but for [align_of]. *)
let align_of_cache = Hashtbl.create 17

let pointer_integer_type = Ityp_i64

let rec size_of_impl ir_type =
  match ir_type with
  | Ityp_unit -> 0
  | Ityp_i1 -> 1
  | Ityp_i8 -> 1
  | Ityp_i16 -> 2
  | Ityp_i32 -> 4
  | Ityp_i64 -> 8
  | Ityp_f32 -> 4
  | Ityp_f64 -> 8
  | Ityp_ptr -> size_of pointer_integer_type
  | Ityp_array (elem_type, size) -> size * size_of elem_type
  | Ityp_struct member_types -> (
      let layout = compute_struct_layout ~is_packed:false member_types in
      match List.rev layout with
      | [] -> 0
      | (last_offset, last_size) :: _ -> last_offset + last_size)
  | Ityp_func _ -> size_of pointer_integer_type

and size_of ir_type =
  match Hashtbl.find_opt size_of_cache ir_type with
  | Some size -> size
  | None ->
      let size = size_of_impl ir_type in
      Hashtbl.add size_of_cache ir_type size;
      size

and align_of_impl ir_type =
  match ir_type with
  | Ityp_unit -> 1
  | Ityp_i1 -> 1
  | Ityp_i8 -> 1
  | Ityp_i16 -> 2
  | Ityp_i32 -> 4
  | Ityp_i64 -> 8
  | Ityp_f32 -> 4
  | Ityp_f64 -> 8
  | Ityp_ptr -> align_of pointer_integer_type
  | Ityp_array (elem_type, _) -> align_of elem_type
  | Ityp_struct member_types ->
      (* Compute the alignment of a struct by finding the maximum alignment
         of its members. *)
      List.fold_left
        (fun acc member_type ->
          let member_align = align_of member_type in
          if member_align > acc then member_align else acc)
        1 member_types
  | Ityp_func _ -> align_of pointer_integer_type

and align_of ir_type =
  match Hashtbl.find_opt align_of_cache ir_type with
  | Some align -> align
  | None ->
      let align = align_of_impl ir_type in
      Hashtbl.add align_of_cache ir_type align;
      align

(** Compute the layout of a packed struct. No padding is added between members,
    leading to a tight layout, but not necessarily optimal alignment. This
    corresponds to the GNU C attribute `__attribute__((packed))`. *)
and compute_packed_struct_layout member_types =
  List.fold_left
    (fun (offset, layout) member_type ->
      let size = size_of member_type in
      let new_layout = (offset, size) :: layout in
      (offset + size, new_layout))
    (0, []) member_types
  |> snd |> List.rev

(** Compute the layout of an aligned struct. Padding may be added between
    members to ensure proper alignment. *)
and compute_aligned_struct_layout member_types =
  let rec aux offset layout = function
    | [] -> List.rev layout
    | member_type :: rest ->
        let alignment = align_of member_type in
        let padding =
          if offset mod alignment = 0 then 0
          else alignment - (offset mod alignment)
        in
        let aligned_offset = offset + padding in
        let size = size_of member_type in
        let new_layout = (aligned_offset, size) :: layout in
        aux (aligned_offset + size) new_layout rest
  in
  aux 0 [] member_types

and compute_struct_layout ~is_packed member_types =
  if is_packed then compute_packed_struct_layout member_types
  else compute_aligned_struct_layout member_types
