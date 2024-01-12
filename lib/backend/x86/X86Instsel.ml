open Ir
open X86Mir

let prolog fn =
  let insts = ref [] in
  insert_frame_alloc insts fn;
  List.rev !insts

let epilog fn =
  let insts = ref [] in
  insert_frame_dealloc insts fn;
  List.rev !insts

let instsel_inst cc_info inst ~is_x64 =
  let r1 = inst.i_name in
  let insts = ref [] in
  (match inst.i_kind with
  | Iinst_cst cst -> (
      match Hashtbl.find_opt inst.i_bb.b_func.fn_ctx.ctx_constants cst with
      (* We avoid to load an integer from the data section and instead
         directly encode it as an immediate. *)
      | Some (Icst_int imm) -> insert_mov insts r1 (Iop_imm imm)
      | Some _ -> insert_mov_constant insts r1 cst
      | None -> assert false)
  | Iinst_loadi imm -> insert_mov insts r1 (Iop_imm imm)
  | Iinst_mov r2 -> insert_mov_regs insts r1 r2
  | Iinst_alloca _ -> failwith "TODO: instsel x86 alloca"
  | Iinst_load _ -> failwith "TODO: instsel x86 load"
  | Iinst_store _ -> failwith "TODO: instsel x86 store"
  | Iinst_extract_value _ -> failwith "TODO: instsel x86 extract value"
  | Iinst_binop (op, r2, r3) -> (
      match op with
      | Ibinop_add -> insert_add insts r1 r2 r3
      | Ibinop_sub -> insert_sub insts r1 r2 r3
      | Ibinop_mul -> insert_imul insts r1 r2 r3
      | Ibinop_udiv -> insert_div insts r1 r2 r3 ~is_x64
      | Ibinop_sdiv -> insert_idiv insts r1 r2 r3 ~is_x64
      | Ibinop_urem -> insert_rem insts r1 r2 r3 ~is_x64
      | Ibinop_srem -> insert_irem insts r1 r2 r3 ~is_x64
      | Ibinop_and -> insert_and insts r1 r2 r3
      | Ibinop_or -> insert_or insts r1 r2 r3
      | Ibinop_xor -> insert_xor insts r1 r2 r3
      | Ibinop_lsl -> insert_shl insts r1 r2 r3
      | Ibinop_lsr -> insert_shr insts r1 r2 r3
      | Ibinop_asr -> insert_sar insts r1 r2 r3)
  | Iinst_unop (op, r2) -> (
      match op with
      | Iunop_neg -> insert_neg insts r1 r2
      | Iunop_not -> insert_not insts r1 r2)
  | Iinst_cmp (cmp, r2, r3) -> (
      match cmp with
      (* For the flags used see https://stackoverflow.com/a/9617990 *)
      | Icmp_eq -> insert_cmp_util insts "e" r1 r2 r3
      | Icmp_ne -> insert_cmp_util insts "ne" r1 r2 r3
      | Icmp_ult -> insert_cmp_util insts "b" r1 r2 r3
      | Icmp_ule -> insert_cmp_util insts "be" r1 r2 r3
      | Icmp_ugt -> insert_cmp_util insts "a" r1 r2 r3
      | Icmp_uge -> insert_cmp_util insts "ae" r1 r2 r3
      | Icmp_slt -> insert_cmp_util insts "l" r1 r2 r3
      | Icmp_sle -> insert_cmp_util insts "le" r1 r2 r3
      | Icmp_sgt -> insert_cmp_util insts "g" r1 r2 r3
      | Icmp_sge -> insert_cmp_util insts "ge" r1 r2 r3)
  | Iinst_call (fname, args) ->
      insert_call insts cc_info r1 fname (List.map from_ir_operand args)
  | Iinst_phi _ -> failwith "PHI instructions should have been lowered"
  (* Terminator instructions. *)
  | Iinst_ret -> insert_ret insts cc_info
  | Iinst_retv value -> insert_ret_value insts cc_info value
  | Iinst_jmp l -> insert_jmp insts l
  | Iinst_jmpc (r, tl, el) -> insert_jmp_conditional insts r tl el
  | Iinst_unreachable -> ());
  List.rev !insts

let instsel_bb cc_info bb ~is_x64 =
  InstselCommon.instsel_bb bb (instsel_inst cc_info ~is_x64)

(** Converts the given IR function to its MIR counterpart by doing x86 instruction selection. *)
let instsel_fn ~is_x64 fn =
  let cc_info = if is_x64 then X86Mir.x64_cc_info else X86Mir.x86_cc_info in
  InstselCommon.instsel_fn fn (instsel_bb cc_info ~is_x64)
