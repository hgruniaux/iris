open InstselCommon

let x86_cc_info =
  {
    cc_caller_saved = X86Regs.x86_caller_saved;
    cc_callee_saved = X86Regs.x86_callee_saved;
    cc_args_regs = X86Regs.x86_args_regs;
    cc_args_regs_count = X86Regs.x86_args_regs_count;
    cc_return_reg = Some X86Regs.return_reg;
  }

let x64_cc_info =
  {
    cc_caller_saved = X86Regs.x64_caller_saved;
    cc_callee_saved = X86Regs.x64_callee_saved;
    cc_args_regs = X86Regs.x64_args_regs;
    cc_args_regs_count = X86Regs.x64_args_regs_count;
    cc_return_reg = Some X86Regs.return_reg;
  }

let mk_mov r1 r2 =
  Mir.mk_inst "mov"
    [ Mir.Oreg r1; Mir.Oreg r2 ]
    ~defs:[ r1 ] ~uses:[ r2 ] ~is_mov:true

let mk_movi r1 imm =
  Mir.mk_inst "mov" [ Mir.Oreg r1; Mir.Oimm imm ] ~defs:[ r1 ] ~uses:[]

let mk_load r1 r2 =
  [ Mir.mk_inst "load" [ Mir.Oreg r1; Mir.Oreg r2 ] ~defs:[ r1 ] ~uses:[ r2 ] ]

let mk_store r1 r2 =
  [ Mir.mk_inst "store" [ Mir.Oreg r1; Mir.Oreg r2 ] ~defs:[] ~uses:[ r1; r2 ] ]

let mk_binop_util kind r1 r2 r3 =
  [
    mk_mov r1 r2;
    Mir.mk_inst kind [ Mir.Oreg r1; Mir.Oreg r3 ] ~defs:[ r1 ] ~uses:[ r1; r3 ];
  ]

let mk_add r1 r2 r3 = mk_binop_util "add" r1 r2 r3

let mk_addi r1 r2 imm =
  [
    mk_mov r1 r2;
    Mir.mk_inst "add" [ Mir.Oreg r1; Mir.Oimm imm ] ~defs:[ r1 ] ~uses:[ r1 ];
  ]

let mk_sub r1 r2 r3 = mk_binop_util "sub" r1 r2 r3

let mk_subi r1 r2 imm =
  [
    mk_mov r1 r2;
    Mir.mk_inst "sub" [ Mir.Oreg r1; Mir.Oimm imm ] ~defs:[ r1 ] ~uses:[ r1 ];
  ]

let mk_imul r1 r2 r3 = mk_binop_util "imul" r1 r2 r3

let mk_imuli r1 r2 imm =
  [
    Mir.mk_inst "imul"
      [ Mir.Oreg r1; Mir.Oreg r2; Mir.Oimm imm ]
      ~defs:[ r1 ] ~uses:[ r1; r2 ];
  ]

let mk_cqo () =
  Mir.mk_inst "cqo" [] ~defs:[ X86Regs.eax; X86Regs.edx ] ~uses:[ X86Regs.eax ]

let mk_div_like_util kind rout r1 r2 r3 =
  [
    mk_mov X86Regs.eax r2;
    (* FIXME: CQO or CQD depending on x86 or x86-64 *)
    mk_cqo ();
    Mir.mk_inst kind [ Mir.Oreg r3 ] ~defs:[ X86Regs.eax; X86Regs.edx ] ~uses:[];
    mk_mov r1 rout;
  ]

let mk_div r1 r2 r3 = mk_div_like_util "div" X86Regs.eax r1 r2 r3
let mk_rem r1 r2 r3 = mk_div_like_util "div" X86Regs.edx r1 r2 r3
let mk_idiv r1 r2 r3 = mk_div_like_util "idiv" X86Regs.eax r1 r2 r3
let mk_irem r1 r2 r3 = mk_div_like_util "idiv" X86Regs.edx r1 r2 r3
let mk_and r1 r2 r3 = mk_binop_util "and" r1 r2 r3
let mk_or r1 r2 r3 = mk_binop_util "or" r1 r2 r3
let mk_xor r1 r2 r3 = mk_binop_util "xor" r1 r2 r3
let mk_xor r1 r2 r3 = mk_binop_util "xor" r1 r2 r3

let mk_shift_util kind r1 r2 r3 =
  let cl = X86Regs.ecx in
  [
    mk_mov cl r3;
    mk_mov r1 r2;
    Mir.mk_inst kind [ Mir.Oreg r1 ] ~uses:[ r1; cl ] ~defs:[ r1 ];
  ]

let mk_sal r1 r2 r3 = mk_shift_util "sal" r1 r2 r3
let mk_shl r1 r2 r3 = mk_shift_util "shl" r1 r2 r3
let mk_sar r1 r2 r3 = mk_shift_util "sar" r1 r2 r3
let mk_shr r1 r2 r3 = mk_shift_util "shr" r1 r2 r3

let mk_unop_util kind r1 r2 =
  [ mk_mov r1 r2; Mir.mk_inst kind [ Mir.Oreg r1 ] ~uses:[ r1 ] ~defs:[ r1 ] ]

let mk_not r1 r2 = mk_unop_util "not" r1 r2
let mk_neg r1 r2 = mk_unop_util "neg" r1 r2

let mk_push r1 =
  Mir.mk_inst "push" [ Oreg r1 ] ~defs:[ X86Regs.esp ] ~uses:[ X86Regs.esp; r1 ]

let mk_pop r1 =
  Mir.mk_inst "pop" [ Oreg r1 ] ~defs:[ X86Regs.esp; r1 ] ~uses:[ X86Regs.esp ]

let mk_call cc_info fname =
  ignore fname;
  [
    Mir.mk_inst "call" [ Ofunc fname ]
      ~defs:(Mir.Reg.Set.elements cc_info.cc_caller_saved)
      ~uses:[];
  ]

let mk_ret _ (* cc_info *) =
  [
    Mir.mk_inst "ret" []
      ~defs:[] (* FIXME: ~uses:(Mir.Reg.Set.elements cc_info.cc_callee_saved) *)
      ~uses:[];
  ]

(* Return with register. *)
let mk_retr cc_info r1 = mk_mov X86Regs.return_reg r1 :: mk_ret cc_info

(* Return with immediate. *)
let mk_reti cc_info imm = mk_movi X86Regs.return_reg imm :: mk_ret cc_info
let mk_jmp l = [ Mir.mk_inst "jmp" [ Olabel l ] ~defs:[] ~uses:[] ]

let mk_jmpc r tl el =
  [
    Mir.mk_inst "cmp" [ Oreg r; Oimm 0n ] ~defs:[] ~uses:[ r ];
    Mir.mk_inst "jz" [ Olabel el; Olabel tl ] ~defs:[] ~uses:[];
  ]
