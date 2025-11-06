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

let _emit_inst b inst_kind =
  let fn = Option.get b.builder_function in
  let bb = Option.get b.builder_bb in
  let reg = Function.fresh_register fn (Instruction.type_of inst_kind) in
  let inst = Instruction.create fn reg inst_kind in
  bb.b_insts <- bb.b_insts @ [ inst ];
  Ival_reg reg

let _emit_inst_in_entry_bb b inst_kind =
  let fn = Option.get b.builder_function in
  let entry_bb = Option.get (Function.entry_block fn) in
  let reg = Function.fresh_register fn (Instruction.type_of inst_kind) in
  let inst = Instruction.create fn reg inst_kind in
  entry_bb.b_insts <- inst :: entry_bb.b_insts;
  Ival_reg reg

(** Emits an alloca instruction at the start of the entry basic block. *)
let emit_alloca b alloc_typ =
  let alignment = Machine_info.align_of alloc_typ in
  _emit_inst_in_entry_bb b (Iinst_alloca (alloc_typ, alignment))

let emit_load b loaded_typ addr =
  assert (_is_ptr_value addr);
  _emit_inst b (Iinst_load (loaded_typ, addr))

let emit_store b addr value =
  assert (_is_ptr_value addr);
  ignore (_emit_inst b (Iinst_store (addr, value)))

let emit_ibinary b op v1 v2 =
  assert (_is_int_value v1);
  assert (_is_int_value v2);
  assert (Value.type_of v1 = Value.type_of v2);

  match Ir_eval.eval_ibinary op v1 v2 with
  | Some v -> v
  | None -> _emit_inst b (Iinst_ibinop (op, v1, v2))

let emit_iunary b op v =
  assert (_is_int_value v);
  assert (_is_int_value v);
  match Ir_eval.eval_iunary op v with
  | Some v -> v
  | None -> _emit_inst b (Iinst_iunop (op, v))

let emit_icmp b cmp v1 v2 =
  assert (_is_int_value v1);
  assert (_is_int_value v2);
  assert (Value.type_of v1 = Value.type_of v2);

  match Ir_eval.eval_icmp cmp v1 v2 with
  | Some v -> v
  | None -> _emit_inst b (Iinst_icmp (cmp, v1, v2))

let emit_extend_s b target_typ v =
  assert (_is_int_value v);
  assert (Type.is_integer target_typ);
  if target_typ = Value.type_of v then v
  else
    let bw_target_typ = Type.bitwidth target_typ in
    let bw_source_typ = Type.bitwidth (Value.type_of v) in
    assert (bw_target_typ > bw_source_typ);

    match v with
    | Ival_int (_, c) ->
        let extended_c = Z.signed_extract c 0 bw_source_typ in
        Ival_int (target_typ, extended_c)
    | _ -> _emit_inst b (Iinst_cast (Icast_extend_s, target_typ, v))

let emit_extend_u b target_typ v =
  assert (_is_int_value v);
  assert (Type.is_integer target_typ);
  if target_typ = Value.type_of v then v
  else
    let bw_target_typ = Type.bitwidth target_typ in
    let bw_source_typ = Type.bitwidth (Value.type_of v) in
    assert (bw_target_typ > bw_source_typ);

    match v with
    | Ival_int (_, c) ->
        let extended_c = Z.extract c 0 bw_source_typ in
        Ival_int (target_typ, extended_c)
    | _ -> _emit_inst b (Iinst_cast (Icast_extend_u, target_typ, v))

let emit_trunc b target_typ v =
  assert (_is_int_value v);
  assert (Type.is_integer target_typ);
  if target_typ = Value.type_of v then v
  else
    let bw_target_typ = Type.bitwidth target_typ in
    let bw_source_typ = Type.bitwidth (Value.type_of v) in
    assert (bw_target_typ < bw_source_typ);

    match v with
    | Ival_int (_, c) ->
        let truncated_c = Z.extract c 0 bw_target_typ in
        Ival_int (target_typ, truncated_c)
    | _ -> _emit_inst b (Iinst_cast (Icast_trunc, target_typ, v))

let emit_fp_promote b target_typ v =
  assert (_is_float_value v);
  assert (Type.is_float target_typ);
  if target_typ = Value.type_of v then v
  else
    let bw_target_typ = Type.bitwidth target_typ in
    let bw_source_typ = Type.bitwidth (Value.type_of v) in
    assert (bw_target_typ > bw_source_typ);
    match v with
    | Ival_float (_, f) ->
        Ival_float (target_typ, f) (* TODO: is this really correct? *)
    | _ -> _emit_inst b (Iinst_cast (Icast_promote, target_typ, v))

let emit_fp_demote b target_typ v =
  assert (_is_float_value v);
  assert (Type.is_float target_typ);
  if target_typ = Value.type_of v then v
  else
    let bw_target_typ = Type.bitwidth target_typ in
    let bw_source_typ = Type.bitwidth (Value.type_of v) in
    assert (bw_target_typ < bw_source_typ);
    match v with
    | Ival_float (_, f) ->
        Ival_float (target_typ, f) (* TODO: is this really correct? *)
    | _ -> _emit_inst b (Iinst_cast (Icast_demote, target_typ, v))

let emit_fp2ui b target_typ v =
  assert (Type.is_integer target_typ);
  assert (_is_float_value v);
  _emit_inst b (Iinst_cast (Icast_fp2ui, target_typ, v))

let emit_fp2si b target_typ v =
  assert (Type.is_integer target_typ);
  assert (_is_float_value v);
  _emit_inst b (Iinst_cast (Icast_fp2si, target_typ, v))

let emit_ui2fp b target_typ v =
  assert (_is_int_value v);
  assert (Type.is_float target_typ);
  _emit_inst b (Iinst_cast (Icast_ui2fp, target_typ, v))

let emit_si2fp b target_typ v =
  assert (_is_int_value v);
  assert (Type.is_float target_typ);
  _emit_inst b (Iinst_cast (Icast_si2fp, target_typ, v))

let emit_ptr2int b target_typ v =
  assert (Type.is_integer target_typ);
  assert (_is_ptr_value v);
  _emit_inst b (Iinst_cast (Icast_ptr2int, target_typ, v))

let emit_int2ptr b target_typ v =
  assert (target_typ = Ityp_ptr);
  assert (_is_int_value v);
  match v with
  | Ival_int (_, c) ->
      let ptr_bitwidth = Type.bitwidth Ityp_ptr in
      let ptr_val = Z.extract c 0 ptr_bitwidth in
      Ival_int (Ityp_ptr, ptr_val)
  | _ -> _emit_inst b (Iinst_cast (Icast_int2ptr, target_typ, v))

let emit_cast b castop target_typ v =
  match castop with
  | Icast_extend_s -> emit_extend_s b target_typ v
  | Icast_extend_u -> emit_extend_u b target_typ v
  | Icast_trunc -> emit_trunc b target_typ v
  | Icast_promote -> emit_fp_promote b target_typ v
  | Icast_demote -> emit_fp_demote b target_typ v
  | Icast_fp2ui -> emit_fp2ui b target_typ v
  | Icast_fp2si -> emit_fp2si b target_typ v
  | Icast_ui2fp -> emit_ui2fp b target_typ v
  | Icast_si2fp -> emit_si2fp b target_typ v
  | Icast_ptr2int -> emit_ptr2int b target_typ v
  | Icast_int2ptr -> emit_int2ptr b target_typ v
  | Icast_bitcast -> _emit_inst b (Iinst_cast (Icast_bitcast, target_typ, v))

let emit_cast_int ~signed b target_typ v =
  assert (_is_int_value v);
  assert (Type.is_integer target_typ);
  let source_typ = Value.type_of v in
  if target_typ = source_typ then v
  else if Type.bitwidth target_typ > Type.bitwidth source_typ then
    if signed then emit_extend_s b target_typ v
    else emit_extend_u b target_typ v
  else if Type.bitwidth target_typ < Type.bitwidth source_typ then
    emit_trunc b target_typ v
  else v

let emit_cast_float b target_typ v =
  assert (_is_float_value v);
  assert (Type.is_float target_typ);
  let source_typ = Value.type_of v in
  if target_typ = source_typ then v
  else if Type.bitwidth target_typ > Type.bitwidth source_typ then
    emit_fp_promote b target_typ v
  else if Type.bitwidth target_typ < Type.bitwidth source_typ then
    emit_fp_demote b target_typ v
  else v

let emit_array_addr b elem_typ base index =
  assert (_is_ptr_value base);
  let element_size = Z.of_int (Machine_info.size_of elem_typ) in
  let element_size_val = Ival_int (Type.integer_pointer_type, element_size) in
  let casted_index =
    emit_cast_int ~signed:false b Type.integer_pointer_type index
  in
  let offset = emit_ibinary b Ibinop_mul casted_index element_size_val in
  let offset_ptr = emit_int2ptr b Ityp_ptr offset in
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
  let callee_type = Value.type_of callee in
  let param_types, is_variadic =
    match callee_type with
    | Ityp_func (param_types, _return_type, is_variadic) ->
        (param_types, is_variadic)
    | _ ->
        Format.eprintf "Callee type: %a@." Ir_printer.pp_type callee_type;
        assert false
  in

  let args_len = List.length args in
  let param_len = List.length param_types in
  assert (
    (is_variadic && args_len >= param_len)
    || ((not is_variadic) && args_len = param_len));

  (try
     List.iter2
       (fun arg param_typ -> assert (Value.type_of arg = param_typ))
       args param_types
   with Invalid_argument _ ->
     (* Both lists do not have the same length, this may be due to variadic arguments.
     We already checked if we have enough arguments, nor too many, above. Therefore,
      we can ignore this exception *)
     ());

  _emit_inst b (Iinst_call (callee, args))

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

let emit_ret b value_opt = emit_term b (Iterm_ret value_opt)

let emit_br b target_bb =
  assert (is_block_in_current_function b target_bb);
  emit_term b (Iterm_br (Block.label target_bb, []))

let emit_br_if b cond true_bb false_bb =
  assert (is_block_in_current_function b true_bb);
  assert (is_block_in_current_function b false_bb);

  let true_label = Block.label true_bb in
  let false_label = Block.label false_bb in

  match cond with
  | Ival_int (_, c) when Z.equal c Z.zero ->
      emit_term b (Iterm_br (false_label, []))
  | Ival_int (_, c) when Z.equal c Z.one ->
      emit_term b (Iterm_br (true_label, []))
  | _ -> emit_term b (Iterm_br_if (cond, true_label, [], false_label, []))

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
  fn.fn_blocks <-
    LabelMap.filter
      (fun _ bb -> Block.label bb = entry_bb || Block.has_preds bb)
      fn.fn_blocks

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
