(*
 * Copyright (c) 2025 Hubert Gruniaux
 * This file is part of the Iris project.
 *
 * This file contains the IR verifier. It checks that the IR is well-typed
 * and that all invariants are respected.
 *)

open Ir_base
module Value = Ir_value
module Printer = Ir_printer.Printer

exception Error of string

let error msg = raise (Error msg)

let expect_value_of_type v expected_type =
  let actual_type = Value.type_of v in
  if actual_type <> expected_type then
    let msg =
      Format.asprintf "Value %a expected to be of type %a but is of type %a"
        Printer.pp_value v Printer.pp_type expected_type Printer.pp_type
        actual_type
    in
    error msg

let expect_bool_value v = expect_value_of_type v Ityp_i1
let expect_pointer_value v = expect_value_of_type v Ityp_ptr
let expect_same_type v1 v2 = expect_value_of_type v1 (Value.type_of v2)

(** Raises an error if [v] is not an integer value. *)
let expect_integer_value v =
  let t = Value.type_of v in
  if not (Type.is_integer t) then
    let msg =
      Format.asprintf "Value %a expected to be an integer but is of type %a"
        Printer.pp_value v Printer.pp_type t
    in
    error msg

(** Raises an error if [v] is not a float value. *)
let expect_float_value v =
  let t = Value.type_of v in
  if not (Type.is_float t) then
    let msg =
      Format.asprintf "Value %a expected to be a float but is of type %a"
        Printer.pp_value v Printer.pp_type t
    in
    error msg

(** Returns the rank of an integer type.

    The rank is defined as follows: i1 < i8 < i16 < i32 < i64. *)
let integer_rank t =
  match t with
  | Ityp_i1 -> 1
  | Ityp_i8 -> 2
  | Ityp_i16 -> 3
  | Ityp_i32 -> 4
  | Ityp_i64 -> 5
  | _ -> 0

(** Returns the rank of a floating-point type.

    The rank is defined as follows: f32 < f64. *)
let float_rank t = match t with Ityp_f32 -> 1 | Ityp_f64 -> 2 | _ -> 0

let check_ibinary _op v1 v2 =
  expect_integer_value v1;
  expect_integer_value v2;
  expect_same_type v1 v2

let check_iunary _op v = expect_integer_value v

let check_icmp _op v1 v2 =
  expect_integer_value v1;
  expect_integer_value v2;
  expect_same_type v1 v2

let check_cast op target_type v =
  let expect_integer_target_type t =
    if not (Type.is_integer t) then
      let msg =
        Format.asprintf
          "Invalid cast: target type for %a must be an integer type, got %a"
          Printer.pp_castop op Printer.pp_type t
      in
      error msg
  in

  let expect_float_target_type t =
    if not (Type.is_float t) then
      let msg =
        Format.asprintf
          "Invalid cast: target type for %a must be a float type, got %a"
          Printer.pp_castop op Printer.pp_type t
      in
      error msg
  in

  match op with
  | Icast_extend_s | Icast_extend_u ->
      expect_integer_value v;
      expect_integer_target_type target_type;
      if integer_rank (Value.type_of v) >= integer_rank target_type then
        let msg =
          Format.asprintf
            "Invalid cast: cannot extend from %a to %a as source type is \
             larger than or equal to target type"
            Printer.pp_type (Value.type_of v) Printer.pp_type target_type
        in
        error msg
  | Icast_trunc ->
      expect_integer_value v;
      expect_integer_target_type target_type;
      if integer_rank (Value.type_of v) <= integer_rank target_type then
        let msg =
          Format.asprintf
            "Invalid cast: cannot truncate from %a to %a as source type is \
             smaller than or equal to target type"
            Printer.pp_type (Value.type_of v) Printer.pp_type target_type
        in
        error msg
  | Icast_promote ->
      expect_float_value v;
      expect_float_target_type target_type;
      if float_rank (Value.type_of v) >= float_rank target_type then
        let msg =
          Format.asprintf
            "Invalid cast: cannot promote from %a to %a as source type is \
             larger than or equal to target type"
            Printer.pp_type (Value.type_of v) Printer.pp_type target_type
        in
        error msg
  | Icast_demote ->
      expect_float_value v;
      expect_float_target_type target_type;
      if float_rank (Value.type_of v) <= float_rank target_type then
        let msg =
          Format.asprintf
            "Invalid cast: cannot demote from %a to %a as source type is \
             smaller than or equal to target type"
            Printer.pp_type (Value.type_of v) Printer.pp_type target_type
        in
        error msg
  | Icast_si2fp | Icast_ui2fp ->
      expect_integer_value v;
      expect_float_target_type target_type
  | Icast_fp2si | Icast_fp2ui ->
      expect_float_value v;
      expect_integer_target_type target_type
  | Icast_ptr2int ->
      expect_pointer_value v;
      expect_integer_target_type target_type
  | Icast_int2ptr ->
      expect_integer_value v;
      if target_type <> Ityp_ptr then
        let msg =
          Format.asprintf
            "Invalid cast: target type for int2ptr must be a pointer type, got \
             %a"
            Printer.pp_type target_type
        in
        error msg
  | Icast_bitcast ->
      let src_size = Machine_info.size_of (Value.type_of v) in
      let dst_size = Machine_info.size_of target_type in
      if src_size <> dst_size then
        let msg =
          Format.asprintf
            "Invalid cast: cannot bitcast from %a to %a as their sizes differ \
             (%d vs %d bytes)"
            Printer.pp_type (Value.type_of v) Printer.pp_type target_type
            src_size dst_size
        in
        error msg

let check_load _expected_type ptr_value = expect_pointer_value ptr_value
let check_store ptr_value _value = expect_pointer_value ptr_value

let check_call callee args =
  (* Check if the callee is a function *)
  let callee_type = Value.type_of callee in
  let param_types, is_variadic =
    match callee_type with
    | Ityp_func (param_types, _return_type, is_variadic) ->
        (param_types, is_variadic)
    | _ ->
        let msg =
          Format.asprintf "Callee %a is not of function type" Printer.pp_value
            callee
        in
        error msg
  in

  (* Check if the number of arguments is valid *)
  let args_len = List.length args in
  let param_len = List.length param_types in
  (if
     ((not is_variadic) && args_len <> param_len)
     || (is_variadic && args_len < param_len)
   then
     let msg =
       Format.asprintf
         "Function call to %a has %d arguments but expected %d parameters%s"
         Printer.pp_value callee args_len param_len
         (if is_variadic then " or more" else "")
     in
     error msg);

  (* Check if all argument types match the parameter types *)
  try
    List.iter2
      (fun arg param_typ -> expect_value_of_type arg param_typ)
      args param_types
  with Invalid_argument _ ->
    (* Both lists do not have the same length, this may be due to variadic arguments.
     We already checked if we have enough arguments, nor too many, above. Therefore,
      we can ignore this exception *)
    ()

let return_type_of_function fn =
  match fn.fn_type with
  | Ityp_func (_, ret_type, _) -> ret_type
  | _ -> assert false

let check_ret fn ret_value_opt =
  let expected_ret_type = return_type_of_function fn in
  match (expected_ret_type, ret_value_opt) with
  | Ityp_unit, None -> ()
  | Ityp_unit, Some _ ->
      let msg = "Return instruction should not return a value" in
      error msg
  | _, None ->
      let msg = "Return instruction should return a value" in
      error msg
  | _, Some ret_value -> expect_value_of_type ret_value expected_ret_type

let check_target_label fn target_label target_args =
  match LabelMap.find_opt target_label fn.fn_blocks with
  | None ->
      let msg =
        Format.asprintf "Branch target label %a does not exist in function %s"
          Printer.pp_label target_label fn.fn_name
      in
      error msg
  | Some target_bb ->
      let expected_args = Ir_block.params target_bb in
      let expected_len = List.length expected_args in
      let actual_len = List.length target_args in
      (if expected_len <> actual_len then
         let msg =
           Format.asprintf
             "Branch to label %a has %d arguments but target block expects %d \
              arguments"
             Printer.pp_label target_label actual_len expected_len
         in
         error msg);

      List.iter2
        (fun arg expected_arg ->
          expect_value_of_type arg (Reg.type_of expected_arg))
        target_args expected_args

let check_br fn target_label target_args =
  check_target_label fn target_label target_args

let check_br_if fn cond true_label true_args false_label false_args =
  expect_bool_value cond;
  check_target_label fn true_label true_args;
  check_target_label fn false_label false_args

let check_uses fn =
  let entry_bb = Option.get (Ir_function.entry_block fn) in
  let idom = Ir_cfg.Dominator.compute_idom fn entry_bb in
  let dom_tree = Ir_cfg.Dominator.idom_to_dom_tree fn idom in

  (* Algorithm:
   * - We traverse the dominator tree, keeping track of the set of defined registers.
   * - For each instruction, we check that all used registers are in the set of defined registers.
   * - We then add the defined registers to the set and continue traversing.
   * We need to also consider the block arguments as defined at the start of the block and
   * the function parameters as defined at the start of the function. *)
  let rec check_block acc_defs bb =
    let acc_defs_with_args =
      RegSet.union acc_defs (RegSet.of_list (Ir_block.params bb))
    in

    let acc_defs_at_end =
      Ir_block.fold_insts
        (fun acc_defs inst ->
          let uses = Ir_instruction.uses_reg inst in
          let defs = Ir_instruction.defs_reg inst in

          let bad_uses = RegSet.diff uses acc_defs in
          match RegSet.choose_opt bad_uses with
          | Some reg ->
              if Hashtbl.mem fn.fn_symbol_table reg then
                let msg =
                  Format.asprintf
                    "Use of register %a not dominated by its definition"
                    Printer.pp_register reg
                in
                error msg
              else
                let msg =
                  Format.asprintf "Use of unknown register %a"
                    Printer.pp_register reg
                in
                error msg
          | None -> RegSet.union acc_defs defs)
        acc_defs_with_args bb
    in

    List.iter (check_block acc_defs_at_end) (dom_tree bb)
  in
  check_block (RegSet.of_list fn.fn_params) entry_bb
