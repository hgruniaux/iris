open Ir
open X86Mir

let prolog fn =
  (*
    push ebp
    mov ebp, esp
  *)
  ignore fn;
  (* FIXME: allocate space for local variables *)
  [ X86Mir.mk_push X86Regs.ebp; X86Mir.mk_mov X86Regs.ebp X86Regs.esp ]

let epilog fn =
  (*
    mov esp, ebp
    pop ebp
  *)
  ignore fn;
  [ X86Mir.mk_mov X86Regs.esp X86Regs.ebp; X86Mir.mk_pop X86Regs.ebp ]

let find_inst bb r = Hashtbl.find_opt bb.b_func.fn_symbol_table r

let get_reg op =
  match op with Iop_reg r -> r | _ -> failwith "not a register operand"

let from_ir_operand op =
  match op with Iop_imm i -> Mir.Oimm i | Iop_reg r -> Mir.Oreg r

let instsel_inst cc_info inst =
  let r1 = inst.i_name in
  match inst.i_kind with
  (* TODO: support string constant *)
  | Iinst_cst cst -> (
      match Hashtbl.find_opt inst.i_bb.b_func.fn_ctx.ctx_constants cst with
      | Some (Icst_int imm) -> [ mk_movi r1 imm ]
      | Some (_) -> [ mk_mov_constant r1 cst ]
      | None -> assert false)
  | Iinst_mov r2 -> [ mk_mov r1 r2 ]
  | Iinst_load r2 -> mk_load r1 r2
  | Iinst_store (r1, r2) -> mk_store r1 (get_reg r2)
  | Iinst_binop (op, r2, r3) -> (
      match op with
      | Ibinop_add -> (
          match r3 with Iop_imm i -> mk_addi r1 r2 i | _ -> mk_add r1 r2 r3)
      | Ibinop_sub -> (
          match r3 with Iop_imm i -> mk_subi r1 r2 i | _ -> mk_sub r1 r2 r3)
      | Ibinop_mul -> (
          match r3 with Iop_imm i -> mk_imuli r1 r2 i | _ -> mk_imul r1 r2 r3)
      | Ibinop_udiv -> mk_div r1 r2 r3
      | Ibinop_sdiv -> mk_idiv r1 r2 r3
      | Ibinop_urem -> mk_rem r1 r2 r3
      | Ibinop_srem -> mk_irem r1 r2 r3
      | Ibinop_and -> mk_and r1 r2 r3
      | Ibinop_or -> mk_or r1 r2 r3
      | Ibinop_xor -> mk_xor r1 r2 r3
      | Ibinop_lsl -> mk_shl r1 r2 r3
      | Ibinop_lsr -> mk_shr r1 r2 r3
      | Ibinop_asr -> mk_sar r1 r2 r3)
  | Iinst_unop (op, r2) -> (
      match op with Iunop_neg -> mk_neg r1 r2 | Iunop_not -> mk_not r1 r2)
  | Iinst_cmp (cmp, r2, r3) ->
    (match cmp with
    | Icmp_eq -> mk_cmp_util "e" r1 r2 r3
    | Icmp_ne -> mk_cmp_util "ne" r1 r2 r3
    | Icmp_slt -> mk_cmp_util "l" r1 r2 r3
    | Icmp_sle -> mk_cmp_util "le" r1 r2 r3
    | Icmp_sgt -> mk_cmp_util "g" r1 r2 r3
    | Icmp_sge -> mk_cmp_util "ge" r1 r2 r3
    | _ -> failwith "TODO: implement cmp instruction")
  | Iinst_call (fname, args) ->
      mk_call cc_info r1 fname (List.map from_ir_operand args)
  | Iinst_ret -> mk_ret cc_info
  | Iinst_retv value -> (
      match value with
      | Iop_imm i -> mk_reti cc_info i
      | Iop_reg r -> mk_retr cc_info r)
  | Iinst_jmp l -> mk_jmp l
  | Iinst_jmpc (r, tl, el) -> mk_jmpc (get_reg r) tl el
  | _ -> failwith "unsupported operation on x86-64"

let instsel_bb cc_info bb = InstselCommon.instsel_bb bb (instsel_inst cc_info)

(** Converts the given IR function to its MIR counterpart by doing x86 instruction selection. *)
let instsel_fn ~is_x64 fn =
  let cc_info = if is_x64 then X86Mir.x64_cc_info else X86Mir.x86_cc_info in
  InstselCommon.instsel_fn fn (instsel_bb cc_info)
