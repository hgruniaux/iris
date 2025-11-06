(* Copyright (C) 2025 Hubert Gruniaux
 * This file is part of Iris. *)

open Codegen_common
open Codegen_type
open Tast
open LibIris

(** Generate a constant zero value of the same type as [ir_val]. *)
let constant_zero_for ctx ir_val =
  Ir.Builder.emit_constant_zero ctx.ctx_builder (Ir.Value.type_of ir_val)

let codegen_cast_int2bool ctx int_val =
  let zero = constant_zero_for ctx int_val in
  Ir.Builder.emit_icmp ctx.ctx_builder Ir.Icmp_ne int_val zero

let codegen_cast_float2bool ctx float_val =
  ignore ctx;
  ignore float_val;
  failwith "codegen_cast: float to bool cast not implemented"

let codegen_cast_pointer2bool ctx ptr_val =
  let null_ptr = constant_zero_for ctx ptr_val in
  Ir.Builder.emit_icmp ctx.ctx_builder Ir.Icmp_ne ptr_val null_ptr

let codegen_cast_int2int ctx to_type int_val =
  let ir_to_type = codegen_type to_type in
  Ir.Builder.emit_cast_int ~signed:(Type.is_signed to_type) ctx.ctx_builder
    ir_to_type int_val

let codegen_cast_int2float ctx from_type to_type int_val =
  let ir_to_type = codegen_type to_type in
  let op =
    if Type.is_signed from_type then Ir.Icast_si2fp else Ir.Icast_ui2fp
  in
  Ir.Builder.emit_cast ctx.ctx_builder op ir_to_type int_val

let codegen_cast_int2pointer ctx to_type int_val =
  let ir_to_type = codegen_type to_type in
  Ir.Builder.emit_cast ctx.ctx_builder Ir.Icast_int2ptr ir_to_type int_val

let codegen_cast_float2float ctx to_type float_val =
  let ir_to_type = codegen_type to_type in
  Ir.Builder.emit_cast_float ctx.ctx_builder ir_to_type float_val

let codegen_cast_float2int ctx to_type float_val =
  let ir_to_type = codegen_type to_type in
  let op = if Type.is_signed to_type then Ir.Icast_fp2si else Ir.Icast_fp2ui in
  Ir.Builder.emit_cast ctx.ctx_builder op ir_to_type float_val

let codegen_cast_pointer2int ctx to_type ptr_val =
  let ir_to_type = codegen_type to_type in
  Ir.Builder.emit_cast ctx.ctx_builder Ir.Icast_ptr2int ir_to_type ptr_val

let codegen_cast_array2pointer ctx to_type array_val =
  let ir_to_type = codegen_type to_type in
  (* FIXME: implement array to pointer cast *)
  Ir.Builder.emit_cast ctx.ctx_builder Ir.Icast_bitcast ir_to_type array_val

let codegen_cast_lvalue2rvalue ctx to_type from_val =
  let typ = codegen_type to_type in
  Ir.Builder.emit_load ctx.ctx_builder typ from_val

let codegen_cast ctx cast_kind from_type to_type from_val =
  match cast_kind with
  | Tcast_invalid -> failwith "codegen_cast: invalid cast"
  | Tcast_void -> failwith "codegen_cast: cast to void"
  | Tcast_noop ->
      let ir_from_type = codegen_type from_type in
      let ir_to_type = codegen_type to_type in
      assert (ir_from_type = ir_to_type);
      from_val
  | Tcast_int2bool -> codegen_cast_int2bool ctx from_val
  | Tcast_float2bool -> codegen_cast_float2bool ctx from_val
  | Tcast_pointer2bool -> codegen_cast_pointer2bool ctx from_val
  | Tcast_int2int -> codegen_cast_int2int ctx to_type from_val
  | Tcast_int2float -> codegen_cast_int2float ctx from_type to_type from_val
  | Tcast_int2pointer -> codegen_cast_int2pointer ctx to_type from_val
  | Tcast_float2float -> codegen_cast_float2float ctx to_type from_val
  | Tcast_float2int -> codegen_cast_float2int ctx to_type from_val
  | Tcast_function2pointer -> from_val
  | Tcast_pointer2pointer -> from_val
  | Tcast_pointer2int -> codegen_cast_pointer2int ctx to_type from_val
  | Tcast_array2pointer -> codegen_cast_array2pointer ctx to_type from_val
  | Tcast_null2function ->
      let pointee = Type.pointee to_type in
      Ir.Builder.emit_cast ctx.ctx_builder Ir.Icast_bitcast
        (codegen_type pointee)
        (Ir.Builder.emit_constant_zero ctx.ctx_builder (codegen_type to_type))
  | Tcast_null2pointer ->
      Ir.Builder.emit_constant_zero ctx.ctx_builder (codegen_type to_type)
  | Tcast_lvalue2rvalue -> codegen_cast_lvalue2rvalue ctx to_type from_val
