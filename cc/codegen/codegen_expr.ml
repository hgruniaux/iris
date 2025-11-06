(* Copyright (C) 2025 Hubert Gruniaux
 * This file is part of Iris. *)

open Codegen_common
open Codegen_type
open Codegen_cast
open Tast
open LibIris

let rec codegen_expr ctx expr =
  match expr.texpr_kind with
  (* C23 6.5.2 Primary expressions *)
  | Texpr_bool v -> codegen_bool_expr ctx v
  | Texpr_int v -> codegen_int_expr ctx expr.texpr_type v
  | Texpr_float v -> codegen_float_expr ctx expr.texpr_type v
  | Texpr_string v -> codegen_string_expr ctx v
  | Texpr_decl decl -> codegen_decl_ref_expr ctx decl
  | Texpr_paren inner_expr -> codegen_expr ctx inner_expr
  (* C23 6.5.3 Postfix expressions *)
  | Texpr_array (base, index) -> codegen_array_expr ctx base index
  | Texpr_call (callee, args) -> codegen_call_expr ctx callee args
  | Texpr_member (base, member) -> codegen_member_expr ctx base member
  | Texpr_member_deref (base, member) ->
      codegen_member_deref_expr ctx base member
  (* C23 6.5.4 Unary operators *)
  | Texpr_unary (op, sub_expr) -> codegen_unary_expr ctx op expr sub_expr
  | Texpr_sizeof t -> codegen_sizeof_expr ctx expr.texpr_type t
  | Texpr_alignof t -> codegen_alignof_expr ctx expr.texpr_type t
  (* C23 6.5.5 Cast operators *)
  | Texpr_cast (target_type, sub_expr, cast_kind) ->
      codegen_cast_expr ctx cast_kind target_type sub_expr
  (* C23 6.5.16 Conditional operator *)
  | Texpr_conditional _ ->
      failwith "codegen_expr: conditional operator not implemented"
  (* C23 6.5.6 - 6.5.15 and 6.5.17 - 6.5.18 *)
  | Texpr_binary (op, lhs, rhs) -> codegen_binary_expr ctx op lhs rhs
  (* Others, internal expressions. *)
  | Texpr_implicit_cast (target_type, sub_expr, cast_kind) ->
      codegen_cast_expr ctx cast_kind target_type sub_expr

(* C23 6.5.2 Primary expressions *)
and codegen_bool_expr ctx v =
  if v then codegen_int_expr ctx Ttyp_bool Z.one
  else codegen_int_expr ctx Ttyp_bool Z.zero

(* C23 6.5.2 Primary expressions
   C23 6.4.4.2 Integer constants *)
and codegen_int_expr ctx t v =
  let ir_type = codegen_type t in
  Ir.Builder.emit_int_constant ctx.ctx_builder ir_type v

(* C23 6.5.2 Primary expressions
   C23 6.4.4.3 Floating constants *)
and codegen_float_expr ctx t v =
  let ir_type = codegen_type t in
  Ir.Builder.emit_float_constant ctx.ctx_builder ir_type v

(* C23 6.5.2 Primary expressions
   C23 6.4.4.5 Character constants *)
and _codegen_char_expr ctx t v = codegen_int_expr ctx t v

(* C23 6.5.2 Primary expressions
   C23 6.4.5 String literals *)
and codegen_string_expr ctx s =
  Ir.Builder.emit_string_constant ~nul_terminated:true ctx.ctx_builder s

(** Codegen a function/object declaration reference. *)
and codegen_decl_ref_expr ctx decl = Hashtbl.find ctx.ctx_values decl

(* C23 6.5.3.2  Array subscripting *)
and codegen_array_expr ctx base_expr index_expr =
  (* In C, both `array[index]` and `index[array]` are valid expressions.
     So, either [base_expr] or [index_expr] is the pointer to the array.
     We need to determine which is which. *)
  let real_base_expr, real_index_expr =
    if Type.is_pointer base_expr.texpr_type then (base_expr, index_expr)
    else (index_expr, base_expr)
  in
  let base_val = codegen_expr ctx real_base_expr in
  let index_val = codegen_expr ctx real_index_expr in
  let elem_type = Type.pointee base_expr.texpr_type in
  let ir_elem_type = codegen_type elem_type in
  let ptr =
    Ir.Builder.emit_array_addr ctx.ctx_builder ir_elem_type base_val index_val
  in
  ptr

(* C23 6.5.3.3 Function calls *)
and codegen_call_expr ctx callee args =
  let callee_val = codegen_expr ctx callee in

  (* Evaluate from left to right. *)
  let arg_vals = List.map (codegen_expr ctx) args in

  Ir.Builder.emit_call ctx.ctx_builder callee_val arg_vals

and codegen_member_expr_impl ctx base_type base_addr_value member =
  let ir_base_type = codegen_type base_type in
  let struct_decl = Type.destruct_struct base_type in

  if struct_decl.tstruct_is_union then
    (* In unions, all members share the same address. *)
    base_addr_value
  else
    let member_index =
      let rec find_index fields name index =
        match fields with
        | [] -> failwith "codegen_member_expr: Member not found"
        | field :: rest ->
            if field.tstruct_field_name = name then index
            else find_index rest name (index + 1)
      in
      let fields = Option.get struct_decl.tstruct_fields in
      find_index fields member.tstruct_field_name 0
    in
    Ir.Builder.emit_struct_field_addr ctx.ctx_builder ir_base_type
      base_addr_value member_index

(* C23 6.5.3.4 Structure and union members *)
and codegen_member_expr ctx base member =
  codegen_member_expr_impl ctx base.texpr_type (codegen_expr ctx base) member

(* C23 6.5.3.4 Structure and union members *)
and codegen_member_deref_expr ctx base member =
  (* FIXME: check if this is correct *)
  let struct_type = Type.pointee base.texpr_type in
  let base_ptr =
    Ir.Builder.emit_load ctx.ctx_builder Ityp_ptr (codegen_expr ctx base)
  in
  codegen_member_expr_impl ctx struct_type base_ptr member

(* C23 6.5.3.5 Postfix increment and decrement operators *)
and codegen_post_inc_dec ctx op e =
  if Type.is_pointer e.texpr_type then
    failwith "codegen_unary_expr: Pointer Increment/Decrement not implemented"
  else if Type.is_floating_point e.texpr_type then
    failwith
      "codegen_unary_expr: Floating-point Increment/Decrement not implemented"
  else
    let typ = codegen_type e.texpr_type in
    let addr = codegen_expr ctx e in
    let one = Ir.Builder.emit_constant_one ctx.ctx_builder typ in
    let old_value = Ir.Builder.emit_load ctx.ctx_builder typ addr in
    let new_value = Ir.Builder.emit_ibinary ctx.ctx_builder op old_value one in
    ignore (Ir.Builder.emit_store ctx.ctx_builder addr new_value);
    old_value

(* C23 6.5.4.1 Prefix increment and decrement operators *)
and codegen_pre_inc_dec ctx op e =
  if Type.is_pointer e.texpr_type then
    failwith "codegen_unary_expr: Pointer Increment/Decrement not implemented"
  else if Type.is_floating_point e.texpr_type then
    failwith
      "codegen_unary_expr: Floating-point Increment/Decrement not implemented"
  else
    let typ = codegen_type e.texpr_type in
    let addr = codegen_expr ctx e in
    let one = Ir.Builder.emit_constant_one ctx.ctx_builder typ in
    let old_value = Ir.Builder.emit_load ctx.ctx_builder typ addr in
    let new_value = Ir.Builder.emit_ibinary ctx.ctx_builder op old_value one in
    Ir.Builder.emit_store ctx.ctx_builder addr new_value;
    new_value

(* C23 6.5.4.2 Address and indirection operators *)
and codegen_deref_expr ctx e =
  let ptr = codegen_expr ctx e in
  let typ = codegen_type e.texpr_type in
  Ir.Builder.emit_load ctx.ctx_builder typ ptr

(* C23 6.5.4.2 Address and indirection operators *)
and codegen_addrof_expr ctx e =
  match (ignore_parens e).texpr_kind with
  | Texpr_unary (op, sub_expr) when op.punop_kind = Punop_deref ->
      (* C23 6.5.4.2 §3
         If the operand is the result of a unary * operator, neither that
         operator nor the & operator is evaluated and the result is as if both were omitted *)
      (* In other cases, &*x = x.
         This simplification is important, because it guarantees that &*null is
         valid and does not crash the program. *)
      codegen_expr ctx sub_expr
      (* TODO: handle the case where sub_expr is an array subscript expression,
               in that case &x[i] should be simplified to x + i. *)
  | _ -> codegen_expr ctx e

(* C23 6.5.4 Unary operators *)
and codegen_neg_expr ctx e =
  let v = codegen_expr ctx e in
  if Type.is_floating_point e.texpr_type then
    failwith "Floating-point negation not implemented"
  else Ir.Builder.emit_iunary ctx.ctx_builder Iunop_neg v

(* C23 6.5.4 Unary operators *)
and codegen_bitnot_expr ctx e =
  let v = codegen_expr ctx e in
  Ir.Builder.emit_iunary ctx.ctx_builder Iunop_not v

(* C23 6.5.4 Unary operators *)
and codegen_lognot_expr ctx e =
  let v = codegen_expr ctx e in
  if Type.is_floating_point e.texpr_type then
    failwith "Floating-point logical NOT not implemented"
  else
    let zero =
      Ir.Builder.emit_int_constant ctx.ctx_builder (Ir.Value.type_of v) Z.zero
    in
    Ir.Builder.emit_icmp ctx.ctx_builder Ir.Icmp_eq v zero

(* C23 6.5.3 Postfix operators *)
(* C23 6.5.4 Unary operators *)
and codegen_unary_expr ctx op expr sub_expr =
  match op.punop_kind with
  | Punop_plus -> codegen_expr ctx sub_expr
  | Punop_neg -> codegen_neg_expr ctx sub_expr
  | Punop_bitnot -> codegen_bitnot_expr ctx sub_expr
  | Punop_lognot -> codegen_lognot_expr ctx sub_expr
  | Punop_deref -> codegen_deref_expr ctx sub_expr
  | Punop_addrof -> codegen_addrof_expr ctx sub_expr
  | Punop_sizeof -> codegen_sizeof_expr ctx expr.texpr_type sub_expr.texpr_type
  | Punop_pre_inc -> codegen_pre_inc_dec ctx Ir.Ibinop_add sub_expr
  | Punop_pre_dec -> codegen_pre_inc_dec ctx Ir.Ibinop_sub sub_expr
  | Punop_post_inc -> codegen_post_inc_dec ctx Ir.Ibinop_add sub_expr
  | Punop_post_dec -> codegen_post_inc_dec ctx Ir.Ibinop_sub sub_expr

and codegen_sizeof_expr ctx int_type t =
  let ir_type = codegen_type t in
  let size = Machine_info.size_of ir_type in
  Ir.Builder.emit_int_constant ctx.ctx_builder (codegen_type int_type)
    (Z.of_int size)

and codegen_alignof_expr ctx int_type t =
  let ir_type = codegen_type t in
  let align = Machine_info.align_of ir_type in
  Ir.Builder.emit_int_constant ctx.ctx_builder (codegen_type int_type)
    (Z.of_int align)

(* C23 6.5.5 Cast operators *)
and codegen_cast_expr ctx cast_kind target_type sub_expr =
  let from_val = codegen_expr ctx sub_expr in
  codegen_cast ctx cast_kind sub_expr.texpr_type target_type from_val

(* C23 6.5.14 Logical AND operator *)
and codegen_logand_expr ctx lhs rhs =
  ignore ctx;
  ignore lhs;
  ignore rhs;
  failwith "codegen_logand_expr: Not implemented"

(* C23 6.5.15 Logical OR operator *)
and codegen_logor_expr ctx lhs rhs =
  ignore ctx;
  ignore lhs;
  ignore rhs;
  failwith "codegen_logor_expr: Not implemented"

and codegen_binary_expr ctx op e1 e2 =
  let codegen_arith_expr ctx ir_iop_s ir_iop_u e1 e2 =
    let v1 = codegen_expr ctx e1 in
    let v2 = codegen_expr ctx e2 in
    if Type.is_floating_point e1.texpr_type then
      failwith "Floating-point arithmetic not implemented"
    else if Type.is_signed e1.texpr_type then
      Ir.Builder.emit_ibinary ctx.ctx_builder ir_iop_s v1 v2
    else if Type.is_unsigned e1.texpr_type then
      Ir.Builder.emit_ibinary ctx.ctx_builder ir_iop_u v1 v2
    else assert false
  in

  let codegen_integer_expr ctx ir_iop e1 e2 =
    let v1 = codegen_expr ctx e1 in
    let v2 = codegen_expr ctx e2 in
    Ir.Builder.emit_ibinary ctx.ctx_builder ir_iop v1 v2
  in

  let codegen_cmp_expr ctx ir_icmp_s ir_icmp_u e1 e2 =
    let v1 = codegen_expr ctx e1 in
    let v2 = codegen_expr ctx e2 in
    if Type.is_floating_point e1.texpr_type then
      failwith "Floating-point arithmetic not implemented"
    else if Type.is_signed e1.texpr_type then
      Ir.Builder.emit_icmp ctx.ctx_builder ir_icmp_s v1 v2
    else if Type.is_unsigned e1.texpr_type then
      Ir.Builder.emit_icmp ctx.ctx_builder ir_icmp_u v1 v2
    else assert false
  in

  match op.pbinop_kind with
  | Pbinop_add -> codegen_arith_expr ctx Ir.Ibinop_add Ir.Ibinop_add e1 e2
  | Pbinop_sub -> codegen_arith_expr ctx Ir.Ibinop_sub Ir.Ibinop_sub e1 e2
  | Pbinop_mul -> codegen_arith_expr ctx Ir.Ibinop_mul Ir.Ibinop_mul e1 e2
  | Pbinop_div -> codegen_arith_expr ctx Ir.Ibinop_div_s Ir.Ibinop_div_u e1 e2
  | Pbinop_mod -> codegen_arith_expr ctx Ir.Ibinop_rem_s Ir.Ibinop_rem_u e1 e2
  | Pbinop_shl -> codegen_integer_expr ctx Ir.Ibinop_lsl e1 e2
  | Pbinop_shr -> codegen_integer_expr ctx Ir.Ibinop_lsr e1 e2
  | Pbinop_and -> codegen_integer_expr ctx Ir.Ibinop_and e1 e2
  | Pbinop_or -> codegen_integer_expr ctx Ir.Ibinop_or e1 e2
  | Pbinop_xor -> codegen_integer_expr ctx Ir.Ibinop_xor e1 e2
  | Pbinop_eq -> codegen_cmp_expr ctx Ir.Icmp_eq Ir.Icmp_eq e1 e2
  | Pbinop_ne -> codegen_cmp_expr ctx Ir.Icmp_ne Ir.Icmp_ne e1 e2
  | Pbinop_lt -> codegen_cmp_expr ctx Ir.Icmp_lt_s Ir.Icmp_lt_u e1 e2
  | Pbinop_le -> codegen_cmp_expr ctx Ir.Icmp_le_s Ir.Icmp_le_u e1 e2
  | Pbinop_gt -> codegen_cmp_expr ctx Ir.Icmp_gt_s Ir.Icmp_gt_u e1 e2
  | Pbinop_ge -> codegen_cmp_expr ctx Ir.Icmp_ge_s Ir.Icmp_ge_u e1 e2
  | Pbinop_logand -> codegen_logand_expr ctx e1 e2
  | Pbinop_logor -> codegen_logor_expr ctx e1 e2
  | Pbinop_assign -> codegen_assign_expr ctx e1 e2
  | Pbinop_assign_add ->
      failwith "codegen_binary_expr: Assignment (+=) not implemented"
  | Pbinop_assign_sub ->
      failwith "codegen_binary_expr: Assignment (-=) not implemented"
  | Pbinop_assign_mul ->
      failwith "codegen_binary_expr: Assignment (*=) not implemented"
  | Pbinop_assign_div ->
      failwith "codegen_binary_expr: Assignment (/=) not implemented"
  | Pbinop_assign_mod ->
      failwith "codegen_binary_expr: Assignment (%=) not implemented"
  | Pbinop_assign_and ->
      failwith "codegen_binary_expr: Assignment (&=) not implemented"
  | Pbinop_assign_or ->
      failwith "codegen_binary_expr: Assignment (|=) not implemented"
  | Pbinop_assign_xor ->
      failwith "codegen_binary_expr: Assignment (^=) not implemented"
  | Pbinop_assign_shl ->
      failwith "codegen_binary_expr: Assignment (<<=) not implemented"
  | Pbinop_assign_shr ->
      failwith "codegen_binary_expr: Assignment (>>=) not implemented"
  | Pbinop_comma ->
      let _ = codegen_expr ctx e1 in
      codegen_expr ctx e2

(* C23 6.5.17 Assignment operators *)
and codegen_assign_expr ctx lhs rhs =
  let lhs_val = codegen_expr ctx lhs in
  let rhs_val = codegen_expr ctx rhs in
  Ir.Builder.emit_store ctx.ctx_builder lhs_val rhs_val;
  lhs_val
