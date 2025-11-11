open Ir_base
module Value = Ir_value
module Instruction = Ir_instruction
module Block = Ir_block
module Function = Ir_function
module Module = Ir_module

type t = {
  builder_module : ctx;
  mutable builder_function : fn option;
  mutable builder_bb : bb option;
  builder_string_constants : (string, global) Hashtbl.t;
}

(** Creates an IR builder. *)
let create m =
  {
    builder_module = m;
    builder_function = None;
    builder_bb = None;
    builder_string_constants = Hashtbl.create 7;
  }

let current_module (b : t) = b.builder_module
let current_function (b : t) = b.builder_function
let current_block (b : t) = b.builder_bb
let set_current_block (b : t) (bb : bb) = b.builder_bb <- Some bb

(** Returns the return type of the current function. *)
let current_function_return_type (b : t) =
  let fn = Option.get b.builder_function in
  match fn.fn_type with
  | Ityp_func (_, ret_type, _) -> ret_type
  | _ -> assert false

let _is_int_value v = Type.is_integer (Value.type_of v)
let _is_float_value v = Type.is_float (Value.type_of v)
let _is_ptr_value v = Value.type_of v = Ityp_ptr

let _wrap_int typ c =
  let bw = Type.bitwidth typ in
  let mask = Z.(sub (shift_left one bw) one) in
  Ival_int (typ, Z.logand c mask)

let _bool_as_val b =
  if b then Ival_int (Ityp_i1, Z.one) else Ival_int (Ityp_i1, Z.zero)

(** Emits a boolean constant as an integer of type i1. *)
let emit_bool_constant _ b = _bool_as_val b

(** Emits an integer constant. The constant is wrapped-around if it exceeds the
    bit-width of the type. *)
let emit_int_constant _ typ c =
  assert (Type.is_integer typ);
  _wrap_int typ c

let emit_float_constant _ typ f =
  assert (Type.is_float typ);
  Ival_float (typ, f)

let emit_constant_zero _ typ =
  if Type.is_integer typ then Ival_int (typ, Z.zero)
  else if Type.is_float typ then Ival_float (typ, 0.0)
  else failwith "emit_zero_constant: Unsupported type"

let emit_constant_one _ typ =
  if Type.is_integer typ then Ival_int (typ, Z.one)
  else if Type.is_float typ then Ival_float (typ, 1.0)
  else failwith "emit_one_constant: Unsupported type"

(** Emits a string constant. If [unique] is true, the string constant is reused
    if it already exists. If [nul_terminated] is true, a null terminator is
    added to the string. *)
let emit_string_constant ?(unique = true) ?(nul_terminated = false) b s =
  let s = if nul_terminated then s ^ "\000" else s in
  if unique then (
    match Hashtbl.find_opt b.builder_string_constants s with
    | Some g -> Ival_global g
    | None ->
        let g = Module.create_string_constant b.builder_module s in
        Hashtbl.add b.builder_string_constants s g;
        Ival_global g)
  else
    let g = Module.create_string_constant b.builder_module s in
    Ival_global g

let emit_instruction b instruction =
  let bb = Option.get b.builder_bb in
  bb.block_insts <- bb.block_insts @ [ instruction ]

let emit_define b expression =
  let fn = Option.get b.builder_function in
  let reg = Function.fresh_register fn (Instruction.type_of expression) in
  emit_instruction b (Iinst_def (reg, expression));
  Ival_reg reg

let emit_define_in_entry_bb b expression =
  let fn = Option.get b.builder_function in
  let entry_bb = Option.get (Function.entry_block fn) in
  let reg = Function.fresh_register fn (Instruction.type_of expression) in
  let inst = Iinst_def (reg, expression) in
  entry_bb.block_insts <- inst :: entry_bb.block_insts;
  Ival_reg reg

(** Emits an alloca instruction at the start of the entry basic block. *)
let emit_alloca b alloc_typ =
  let alignment = Machine_info.align_of alloc_typ in
  emit_define_in_entry_bb b (Iexpr_alloca (alloc_typ, alignment))

let emit_load b loaded_typ addr =
  Ir_verifier.check_load loaded_typ addr;
  emit_define b (Iexpr_load (loaded_typ, addr))

let emit_store b addr value =
  Ir_verifier.check_store addr value;
  emit_instruction b (Iinst_store (addr, value))

let emit_ibinary b op v1 v2 =
  Ir_verifier.check_ibinary op v1 v2;
  match Ir_eval.eval_ibinary op v1 v2 with
  | Some v -> v
  | None -> emit_define b (Iexpr_ibinop (op, v1, v2))

let emit_iunary b op v =
  Ir_verifier.check_iunary op v;
  match Ir_eval.eval_iunary op v with
  | Some v -> v
  | None -> emit_define b (Iexpr_iunop (op, v))

let emit_icmp b cmp v1 v2 =
  Ir_verifier.check_icmp cmp v1 v2;
  match Ir_eval.eval_icmp cmp v1 v2 with
  | Some v -> v
  | None -> emit_define b (Iexpr_icmp (cmp, v1, v2))

let emit_cast b castop target_typ v =
  Ir_verifier.check_cast castop target_typ v;
  match Ir_eval.eval_cast castop target_typ v with
  | Some v -> v
  | None -> emit_define b (Iexpr_cast (castop, target_typ, v))

let emit_cast_int ~signed b target_typ v =
  assert (_is_int_value v);
  assert (Type.is_integer target_typ);
  let source_typ = Value.type_of v in
  if target_typ = source_typ then v
  else if Type.bitwidth target_typ > Type.bitwidth source_typ then
    if signed then emit_cast b Icast_extend_s target_typ v
    else emit_cast b Icast_extend_u target_typ v
  else if Type.bitwidth target_typ < Type.bitwidth source_typ then
    emit_cast b Icast_trunc target_typ v
  else v

let emit_cast_float b target_typ v =
  assert (_is_float_value v);
  assert (Type.is_float target_typ);
  let source_typ = Value.type_of v in
  if target_typ = source_typ then v
  else if Type.bitwidth target_typ > Type.bitwidth source_typ then
    emit_cast b Icast_promote target_typ v
  else if Type.bitwidth target_typ < Type.bitwidth source_typ then
    emit_cast b Icast_demote target_typ v
  else v

let emit_array_addr b elem_typ base index =
  assert (_is_ptr_value base);
  let element_size = Z.of_int (Machine_info.size_of elem_typ) in
  let element_size_val = Ival_int (Type.integer_pointer_type, element_size) in
  let casted_index =
    emit_cast_int ~signed:false b Type.integer_pointer_type index
  in
  let offset = emit_ibinary b Ibinop_mul casted_index element_size_val in
  let offset_ptr = emit_cast b Icast_int2ptr Ityp_ptr offset in
  emit_ibinary b Ibinop_add base offset_ptr

let emit_struct_field_addr b struct_typ base member_index =
  assert (_is_ptr_value base);

  let struct_fields =
    match struct_typ with Ityp_struct fields -> fields | _ -> assert false
  in

  let struct_layout =
    Machine_info.compute_struct_layout ~is_packed:false struct_fields
  in

  let member_offset, _member_size = List.nth struct_layout member_index in
  emit_ibinary b Ibinop_add base (Ival_int (Ityp_ptr, Z.of_int member_offset))

let emit_call b callee args =
  Ir_verifier.check_call callee args;
  emit_define b (Iexpr_call (callee, args))

let emit_term b term_kind =
  let fn = Option.get b.builder_function in
  let bb = Option.get b.builder_bb in
  Block.set_term fn bb term_kind;
  let new_bb = Block.create fn in
  Block.set_term fn new_bb Iterm_unreachable;
  b.builder_bb <- Some new_bb

let emit_unreachable b = emit_term b Iterm_unreachable

let emit_noreturn_call b callee args =
  emit_call b callee args |> ignore;
  emit_unreachable b

let is_block_in_current_function b bb =
  let fn = Option.get b.builder_function in
  LabelMap.mem (Block.label bb) fn.fn_blocks

let emit_ret b value_opt =
  let fn = Option.get b.builder_function in
  Ir_verifier.check_ret fn value_opt;
  emit_term b (Iterm_ret value_opt)

let emit_br b target_bb =
  let args = [] in
  let fn = Option.get b.builder_function in
  let target_label = Block.label target_bb in
  Ir_verifier.check_br fn target_label args;
  emit_term b (Iterm_br (target_label, args))

let emit_br_if b cond true_bb false_bb =
  let true_args = [] in
  let false_args = [] in
  let true_label = Block.label true_bb in
  let false_label = Block.label false_bb in
  let fn = Option.get b.builder_function in
  Ir_verifier.check_br_if fn cond true_label true_args false_label false_args;

  (* Constant folding *)
  match cond with
  | Ival_int (_, c) when Z.equal c Z.zero ->
      emit_term b (Iterm_br (false_label, false_args))
  | Ival_int (_, _) -> emit_term b (Iterm_br (true_label, true_args))
  | _ ->
      emit_term b
        (Iterm_br_if (cond, true_label, true_args, false_label, false_args))

let emit_extern_function b name function_type =
  Module.get_or_create_function ~is_external:true b.builder_module name
    function_type

let begin_function b name function_type =
  let global_fn =
    Module.get_or_create_function b.builder_module name function_type
  in
  let fn =
    match global_fn.global_kind with
    | Iglobal_function fn -> fn
    | _ -> assert false
  in

  b.builder_function <- Some fn;
  let entry_bb = Block.create fn in
  Label.set_name (Block.label entry_bb) "entry";
  fn.fn_entry <- Some (Block.label entry_bb);
  b.builder_bb <- Some entry_bb;
  (global_fn, fn.fn_params)

let remove_trivially_unreachable_blocks fn =
  let entry_bb = Option.get fn.fn_entry in

  let should_remove block =
    let label = Block.label block in
    (not (Label.equal label entry_bb)) && not (Block.has_preds block)
  in

  let blocks_to_remove =
    LabelMap.fold
      (fun _ bb acc -> if should_remove bb then bb :: acc else acc)
      fn.fn_blocks []
  in

  List.iter
    (fun bb ->
      LabelMap.iter
        (fun _ succ_bb ->
          succ_bb.block_pred <-
            LabelSet.remove (Block.label bb) succ_bb.block_pred)
        fn.fn_blocks)
    blocks_to_remove;

  fn.fn_blocks <-
    LabelMap.filter (fun _ bb -> not (should_remove bb)) fn.fn_blocks

let end_function b =
  let fn = Option.get b.builder_function in

  (* Remove blocks with no predecessors. This pass do not remove all unreachable blocks,
     this is done by optimization passes later. We just remove trivially unreachable
     blocks as the builder tends to create them when inserting some terminator instructions. *)
  remove_trivially_unreachable_blocks fn;

  b.builder_function <- None;
  b.builder_bb <- None

let fresh_block ?(name = None) b =
  let fn = Option.get b.builder_function in
  let bb = Block.create fn in
  Option.iter (fun name -> Label.set_name (Block.label bb) name) name;
  fn.fn_blocks <- LabelMap.add (Block.label bb) bb fn.fn_blocks;
  bb
