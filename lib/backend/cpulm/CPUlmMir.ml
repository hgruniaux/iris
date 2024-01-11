open InstselCommon

let cc_info =
  {
    cc_caller_saved = CPUlmRegs.caller_saved;
    cc_callee_saved = CPUlmRegs.callee_saved;
    cc_args_regs = CPUlmRegs.args_regs;
    cc_args_regs_count = CPUlmRegs.args_regs_count;
    cc_args_stack_ltr = false;
    (* Right to left *)
    cc_return_reg = Some CPUlmRegs.return_reg;
    cc_caller_cleanup = true;
  }

let mk_mov r1 r2 =
  Mir.mk_inst "mov"
    [ Mir.Oreg r1; Mir.Oreg r2 ]
    ~defs:[ r1 ] ~uses:[ r2 ] ~is_mov:true

let mk_loadi r1 imm =
  Mir.mk_inst "loadi" [ Mir.Oreg r1; Mir.Oimm imm ] ~defs:[ r1 ] ~uses:[]

let mk_mov_operand r1 operand =
  match operand with
  | Mir.Oreg r2 -> mk_mov r1 r2
  | Mir.Oimm imm -> mk_loadi r1 imm
  | _ -> Mir.mk_inst "loadi" [ Mir.Oreg r1; operand ] ~defs:[ r1 ] ~uses:[]

let mk_load r1 r2 =
  [ Mir.mk_inst "load" [ Mir.Oreg r1; Mir.Oreg r2 ] ~defs:[ r1 ] ~uses:[ r2 ] ]

let mk_store r1 r2 =
  [ Mir.mk_inst "store" [ Mir.Oreg r1; Mir.Oreg r2 ] ~defs:[] ~uses:[ r1; r2 ] ]

let mk_binop_util kind r1 r2 r3 =
  [
    Mir.mk_inst kind
      [ Mir.Oreg r1; Mir.Oreg r2; Mir.Oreg r3 ]
      ~defs:[ r1 ] ~uses:[ r2; r3 ];
  ]

let mk_binop_imm_util kind r1 r2 imm =
  match imm with
  | 0n -> mk_binop_util kind r1 r2 CPUlmRegs.r0
  | 1n -> mk_binop_util kind r1 r2 CPUlmRegs.r1
  | _ ->
      let tmp = Mir.Reg.fresh () in
      mk_loadi tmp imm :: mk_binop_util kind r1 r2 tmp

let mk_add r1 r2 r3 = mk_binop_util "add" r1 r2 r3
let mk_addi r1 r2 imm = mk_binop_imm_util "add" r1 r2 imm
let mk_sub r1 r2 r3 = mk_binop_util "sub" r1 r2 r3
let mk_subi r1 r2 imm = mk_binop_imm_util "sub" r1 r2 imm
let mk_mul r1 r2 r3 = mk_binop_util "mul" r1 r2 r3
let mk_muli r1 r2 imm = mk_binop_imm_util "mul" r1 r2 imm
let mk_div r1 r2 r3 = mk_binop_util "div" r1 r2 r3
let mk_divi r1 r2 imm = mk_binop_imm_util "div" r1 r2 imm

let mk_rem r1 r2 r3 =
  (*
    We generate the following code:
        div %1 r2 r3
        mul %2 r3 %1
        sub r1 r2 %2
    Which corresponds to "r2 rem r3 == r2 - ((r2 / r3) * r3)"
  *)
  let tmp1 = Mir.Reg.fresh () in
  let tmp2 = Mir.Reg.fresh () in
  mk_div tmp1 r2 r3 @ mk_mul tmp2 r3 tmp1 @ mk_sub r1 r2 tmp2

let mk_and r1 r2 r3 = mk_binop_util "and" r1 r2 r3
let mk_or r1 r2 r3 = mk_binop_util "or" r1 r2 r3
let mk_xor r1 r2 r3 = mk_binop_util "xor" r1 r2 r3
let mk_lsl r1 r2 r3 = mk_binop_util "lsl" r1 r2 r3
let mk_lsr r1 r2 r3 = mk_binop_util "lsr" r1 r2 r3
let mk_asr r1 r2 r3 = mk_binop_util "asr" r1 r2 r3

let mk_unop_util kind r1 r2 =
  [ Mir.mk_inst kind [ Mir.Oreg r1; Mir.Oreg r2 ] ~defs:[ r1 ] ~uses:[ r2 ] ]

let mk_not r1 r2 = mk_unop_util "not" r1 r2
let mk_neg r1 r2 = mk_unop_util "neg" r1 r2

let mk_push r1 =
  Mir.mk_inst "push" [ Oreg r1 ] ~defs:[ CPUlmRegs.rsp ]
    ~uses:[ CPUlmRegs.rsp; r1 ]

let mk_pop r1 =
  Mir.mk_inst "pop" [ Oreg r1 ] ~defs:[ CPUlmRegs.rsp; r1 ]
    ~uses:[ CPUlmRegs.rsp ]

let mk_pop_many count =
  mk_subi CPUlmRegs.rsp CPUlmRegs.rsp (Nativeint.of_int count)

let mk_call cc_info r1 fn args =
  [
    Mir.mk_inst "call"
      (Oreg r1 :: Ofunc fn :: args)
      ~defs:(Mir.Reg.Set.elements cc_info.cc_caller_saved)
      ~uses:[];
  ]

let mk_ret cc_info =
  [
    Mir.mk_inst "ret" [] ~defs:[]
      ~uses:(Mir.Reg.Set.elements cc_info.cc_callee_saved);
  ]

let mk_retv cc_info r1 =
  [
    mk_mov CPUlmRegs.return_reg r1;
    Mir.mk_inst "ret" [] ~defs:[]
      ~uses:
        (CPUlmRegs.return_reg :: Mir.Reg.Set.elements cc_info.cc_callee_saved);
  ]

let mk_jmp l = [ Mir.mk_inst "jmp" [ Olabel l ] ~defs:[] ~uses:[] ]

let mk_jmpc r tl el =
  [
    Mir.mk_inst "test" [ Oreg r; Oreg r ] ~defs:[] ~uses:[ r ];
    Mir.mk_inst "jmp.z" [ Olabel el; Olabel tl ] ~defs:[] ~uses:[ r ];
  ]
