open Ir
open CPUlmMir

let operand_as_reg mir_insts op =
  match op with
  | Iop_imm i -> (
      match i with
      | 0n -> CPUlmRegs.r0
      | 1n -> CPUlmRegs.r1
      | _ ->
          let r = Reg.fresh () in
          mir_insts := mk_loadi r i @ !mir_insts;
          r)
  | Iop_reg r -> r

let instsel_inst inst =
  let r1 = inst.i_name in
  match inst.i_kind with
  | Iinst_cst 0n -> [ mk_mov r1 CPUlmRegs.r0 ]
  | Iinst_cst 1n -> [ mk_mov r1 CPUlmRegs.r1 ]
  | Iinst_cst imm -> mk_loadi r1 imm
  | Iinst_mov r2 -> [ mk_mov r1 r2 ]
  | Iinst_load r2 -> mk_load r1 r2
  | Iinst_store (r1, r2) ->
      let mir_insts = ref [] in
      !mir_insts @ mk_store r1 (operand_as_reg mir_insts r2)
  | Iinst_binop (op, r2, r3) -> (
      let mir_insts = ref [] in
      let r2 = operand_as_reg mir_insts r2 in
      let r3 = operand_as_reg mir_insts r3 in
      !mir_insts
      @
      match op with
      | Ibinop_add -> mk_add r1 r2 r3
      | Ibinop_sub -> mk_sub r1 r2 r3
      | Ibinop_mul -> mk_mul r1 r2 r3
      (* CPUlm does not support unsigned DIV and REM. *)
      | Ibinop_udiv | Ibinop_sdiv -> mk_div r1 r2 r3
      | Ibinop_urem | Ibinop_srem -> mk_rem r1 r2 r3
      | Ibinop_and -> mk_and r1 r2 r3
      | Ibinop_or -> mk_or r1 r2 r3
      | Ibinop_xor -> mk_xor r1 r2 r3
      | Ibinop_lsl -> mk_lsl r1 r2 r3
      | Ibinop_lsr -> mk_lsr r1 r2 r3
      | Ibinop_asr -> mk_asr r1 r2 r3)
  | Iinst_unop (op, r2) -> (
      let mir_insts = ref [] in
      let r2 = operand_as_reg mir_insts r2 in
      !mir_insts
      @ match op with Iunop_neg -> mk_neg r1 r2 | Iunop_not -> mk_not r1 r2)
  | Iinst_call (fname, args) ->
      let args_insts =
        InstselCommon.generate_caller_args cc_info args CPUlmMir.mk_mov
          CPUlmMir.mk_push
      in
      args_insts @ mk_call cc_info fname @ [ mk_mov r1 CPUlmRegs.return_reg ]
  | Iinst_ret -> mk_ret cc_info
  | Iinst_retv r ->
      let mir_insts = ref [] in
      !mir_insts @ mk_retv cc_info (operand_as_reg mir_insts r)
  | Iinst_jmp l -> mk_jmp l
  | Iinst_jmpc (r, tl, el) ->
      let mir_insts = ref [] in
      !mir_insts @ mk_jmpc (operand_as_reg mir_insts r) tl el
  | _ -> failwith "unsupported operation on CPUlm"

let instsel_bb bb =
  InstselCommon.instsel_bb bb instsel_inst

let instsel_fn fn = InstselCommon.instsel_fn fn instsel_bb
