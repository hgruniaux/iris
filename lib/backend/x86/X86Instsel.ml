open Ir
open X86Mir

let instsel_inst _ cc_info inst ~is_x64 =
  let r1 = inst.i_name in
  let insts = ref [] in
  (match inst.i_kind with
  | Iinst_alloca (t, _) ->
      insert_alloca insts r1 t (* FIXME: handle alignment *)
  | Iinst_value v -> insert_mov insts r1 v
  | Iinst_load (_, addr) -> (
      match addr with
      | Ival_reg addr -> insert_load_mem insts r1 addr 1 0
      | _ -> failwith "Expected register as address in load")
  | Iinst_store (addr, value) -> (
      match addr with
      | Ival_reg addr -> insert_store_mem insts addr 1 0 value
      | _ -> failwith "Expected register as address in store")
  | Iinst_ibinop (op, r2, r3) -> (
      match op with
      | Ibinop_add -> insert_add insts r1 r2 r3
      | Ibinop_sub -> insert_sub insts r1 r2 r3
      | Ibinop_mul -> insert_imul insts r1 r2 r3
      | Ibinop_div_u -> insert_div insts r1 r2 r3 ~is_x64
      | Ibinop_div_s -> insert_idiv insts r1 r2 r3 ~is_x64
      | Ibinop_rem_u -> insert_rem insts r1 r2 r3 ~is_x64
      | Ibinop_rem_s -> insert_irem insts r1 r2 r3 ~is_x64
      | Ibinop_and -> insert_and insts r1 r2 r3
      | Ibinop_or -> insert_or insts r1 r2 r3
      | Ibinop_xor -> insert_xor insts r1 r2 r3
      | Ibinop_lsl -> insert_shl insts r1 r2 r3
      | Ibinop_lsr -> insert_shr insts r1 r2 r3
      | Ibinop_asr -> insert_sar insts r1 r2 r3)
  | Iinst_iunop (op, r2) -> (
      match op with
      | Iunop_neg -> insert_neg insts r1 r2
      | Iunop_not -> insert_not insts r1 r2)
  | Iinst_icmp (cmp, r2, r3) -> (
      match cmp with
      (* For the flags used see https://stackoverflow.com/a/9617990 *)
      | Icmp_eq -> insert_cmp_util insts "e" r1 r2 r3
      | Icmp_ne -> insert_cmp_util insts "ne" r1 r2 r3
      | Icmp_lt_u -> insert_cmp_util insts "b" r1 r2 r3
      | Icmp_le_u -> insert_cmp_util insts "be" r1 r2 r3
      | Icmp_gt_u -> insert_cmp_util insts "a" r1 r2 r3
      | Icmp_ge_u -> insert_cmp_util insts "ae" r1 r2 r3
      | Icmp_lt_s -> insert_cmp_util insts "l" r1 r2 r3
      | Icmp_le_s -> insert_cmp_util insts "le" r1 r2 r3
      | Icmp_gt_s -> insert_cmp_util insts "g" r1 r2 r3
      | Icmp_ge_s -> insert_cmp_util insts "ge" r1 r2 r3)
  | Iinst_cast (castop, t, v) -> insert_cast insts r1 castop t v ~is_x64
  | Iinst_call (fname, args) ->
      insert_call insts cc_info r1 fname (List.map from_ir_operand args)
  | Iinst_phi _ -> failwith "PHI instructions should have been lowered");
  List.rev !insts

let instsel_term cc_info term =
  let insts = ref [] in
  (match term with
  | Iterm_ret None -> insert_ret insts cc_info
  | Iterm_ret (Some value) -> insert_ret_value insts cc_info value
  | Iterm_br (target_label, target_args) ->
      insert_jmp insts target_label target_args
  | Iterm_br_if (cond, true_label, true_args, false_label, false_args) ->
      insert_jmp_conditional insts cond true_label true_args false_label
        false_args
  | Iterm_unreachable -> ()
  | Iterm_br_table _ -> failwith "TODO: implement switch in x86");
  List.rev !insts

let instsel_bb ctx cc_info bb ~is_x64 =
  let mir_insts =
    List.fold_right
      (fun inst mir_insts -> instsel_inst ctx cc_info ~is_x64 inst @ mir_insts)
      bb.b_insts
      (instsel_term cc_info (Ir.Block.term bb))
  in
  {
    Mr.mbb_label = Ir.Block.label bb;
    Mr.mbb_insts = mir_insts;
    Mr.mbb_predecessors = Ir.Block.pred bb;
    Mr.mbb_successors = Ir.Block.succ bb;
  }

(** Converts the given IR function to its MR counterpart by doing x86
    instruction selection. *)
let instsel_fn ctx ~is_x64 fn =
  let cc_info = if is_x64 then X86Mir.x64_cc_info else X86Mir.x86_cc_info in
  let mir_blocks = LabelMap.map (instsel_bb ctx cc_info ~is_x64) fn.fn_blocks in
  {
    Mr.mfn_name = fn.fn_name;
    Mr.mfn_params = fn.fn_params;
    Mr.mfn_blocks = mir_blocks;
    Mr.mfn_entry = Option.get fn.fn_entry;
    Mr.mfn_cc_info = cc_info;
    Mr.mfn_frame = None;
  }
