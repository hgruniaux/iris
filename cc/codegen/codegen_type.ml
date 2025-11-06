(* Copyright (C) 2025 Hubert Gruniaux
 * This file is part of Iris. *)

open Tast
open LibIris

(** IR type cache. *)
let ir_type_cache = Hashtbl.create 17

let rec codegen_type_impl typ =
  match Type.head typ with
  | Ttyp_void -> Ir.Ityp_unit
  | Ttyp_bool -> Ir.Ityp_i1
  | Ttyp_char | Ttyp_signed_char | Ttyp_unsigned_char -> Ir.Ityp_i8
  | Ttyp_signed_short | Ttyp_unsigned_short -> Ir.Ityp_i16
  | Ttyp_signed_int | Ttyp_unsigned_int -> Ir.Ityp_i32
  | Ttyp_signed_long | Ttyp_unsigned_long -> Ir.Ityp_i64
  | Ttyp_signed_long_long | Ttyp_unsigned_long_long -> Ir.Ityp_i64
  | Ttyp_float -> Ir.Ityp_f32
  | Ttyp_double -> Ir.Ityp_f64
  | Ttyp_long_double -> Ir.Ityp_f64
  | Ttyp_ptr _ -> Ir.Ityp_ptr
  | Ttyp_array (elem_typ, Some size) ->
      let ir_elem_typ = codegen_type elem_typ in
      Ir.Ityp_array (ir_elem_typ, size)
  | Ttyp_array (_, None) -> failwith "codegen_type: incomplete array type"
  | Ttyp_function (return_type, param_types, is_variadic) ->
      let ir_return_type = codegen_type return_type in
      let ir_param_types = List.map codegen_type param_types in
      Ir.Ityp_func (ir_param_types, ir_return_type, is_variadic)
  | Ttyp_enum enum_decl -> codegen_type enum_decl.tenum_underlying_type
  | Ttyp_typedef _ -> assert false (* removed by Type.head *)
  | Ttyp_struct struct_decl ->
      (* [struct_decl.tstruct_fields] should not be None, because all incomplete types
         should have rejected by the frontend (except when indirectly referenced, but
         then we don't need them, see pointer case). *)
      let members = Option.get struct_decl.tstruct_fields in
      if struct_decl.tstruct_is_union then codegen_union_type members
      else codegen_struct_type members

and codegen_union_type union_fields =
  let ir_member_types =
    List.map (fun member -> codegen_type member.tstruct_field_type) union_fields
  in

  (* Find the biggest type. *)
  List.fold_left
    (fun acc typ ->
      if Machine_info.size_of typ > Machine_info.size_of acc then typ else acc)
    (List.hd ir_member_types) ir_member_types

and codegen_struct_type struct_fields =
  let ir_member_types =
    List.map
      (fun member -> codegen_type member.tstruct_field_type)
      struct_fields
  in
  Ir.Ityp_struct ir_member_types

and codegen_type typ =
  match Hashtbl.find_opt ir_type_cache typ with
  | Some ir_typ -> ir_typ
  | None ->
      let ir_typ = codegen_type_impl typ in
      Hashtbl.add ir_type_cache typ ir_typ;
      ir_typ
