open InstselCommon

let x86_cc_info =
  {
    cc_caller_saved = X86Regs.x86_caller_saved;
    cc_callee_saved = X86Regs.x86_callee_saved;
    cc_args_regs = X86Regs.x86_args_regs;
    cc_args_regs_count = X86Regs.x86_args_regs_count;
    cc_args_stack_ltr = false;
    (* Right to left *)
    cc_return_reg = Some X86Regs.return_reg;
    cc_caller_cleanup = true;
  }

let x64_cc_info =
  {
    cc_caller_saved = X86Regs.x64_caller_saved;
    cc_callee_saved = X86Regs.x64_callee_saved;
    cc_args_regs = X86Regs.x64_args_regs;
    cc_args_regs_count = X86Regs.x64_args_regs_count;
    cc_args_stack_ltr = false;
    (* Right to left *)
    cc_return_reg = Some X86Regs.return_reg;
    cc_caller_cleanup = true;
  }

let from_ir_operand op =
  match op with Ir.Iop_reg r -> Mir.Oreg r | Ir.Iop_imm i -> Mir.Oimm i

let reg_of_operand op =
  match op with
  | Ir.Iop_reg r -> r
  | _ -> failwith "expected a register operand"

let uses_from_operands ops =
  let rec loop ops acc =
    match ops with
    | [] -> []
    | op :: r -> (
        match op with Ir.Iop_reg r -> loop r (r :: acc) | _ -> loop r acc)
  in
  loop ops []

let use_from_operand op = match op with Ir.Iop_reg r -> [ r ] | _ -> []

let insert_mov insts r1 r2 =
  let r1 = reg_of_operand r1 in
  insts :=
    Mir.mk_inst "mov"
      [ Mir.Oreg r1; from_ir_operand r2 ]
      ~defs:[ r1 ] ~uses:(use_from_operand r2) ~is_mov:true
    :: !insts

let insert_binop_util insts kind r1 r2 r3 =
  insert_mov insts r1 r2;
  let r1 = reg_of_operand r1 in
  insts :=
    Mir.mk_inst kind
      [ Mir.Oreg r1; from_ir_operand r3 ]
      ~defs:[ r1 ] ~uses:(use_from_operand r3)
    :: !insts

let insert_add insts r1 r2 r3 = insert_binop_util insts "add" r1 r2 r3
let insert_sub insts r1 r2 r3 = insert_binop_util insts "sub" r1 r2 r3
let insert_imul insts r1 r2 r3 = insert_binop_util insts "imul" r1 r2 r3
let insert_and insts r1 r2 r3 = insert_binop_util insts "and" r1 r2 r3
let insert_or insts r1 r2 r3 = insert_binop_util insts "or" r1 r2 r3
let insert_xor insts r1 r2 r3 = insert_binop_util insts "xor" r1 r2 r3

let insert_div_util insts kind rout r1 r2 r3 =
  insert_mov insts r1 r2;
  let r1 = reg_of_operand r1 in


let mk_mov r1 r2 =
  Mir.mk_inst "mov"
    [ Mir.Oreg r1; Mir.Oreg r2 ]
    ~defs:[ r1 ] ~uses:[ r2 ] ~is_mov:true

let mk_movi r1 imm =
  Mir.mk_inst "mov" [ Mir.Oreg r1; Mir.Oimm imm ] ~defs:[ r1 ] ~uses:[]

let mk_mov_constant r1 cst =
  Mir.mk_inst "mov" [ Mir.Oreg r1; Mir.Oconst cst ] ~defs:[ r1 ] ~uses:[]

let mk_mov_operand r1 operand =
  match operand with
  | Mir.Oreg r2 -> mk_mov r1 r2
  | _ -> Mir.mk_inst "mov" [ Mir.Oreg r1; operand ] ~defs:[ r1 ] ~uses:[]

let mk_load r1 r2 =
  [ Mir.mk_inst "load" [ Mir.Oreg r1; Mir.Oreg r2 ] ~defs:[ r1 ] ~uses:[ r2 ] ]

let mk_store r1 r2 =
  [ Mir.mk_inst "store" [ Mir.Oreg r1; Mir.Oreg r2 ] ~defs:[] ~uses:[ r1; r2 ] ]

let mk_binop_util kind r1 r2 r3 =
  let r2, insts =
    match r2 with
    | Ir.Iop_reg r -> (r, [])
    | Ir.Iop_imm imm ->
        let tmp = Mir.Reg.fresh () in
        (tmp, [ mk_movi tmp imm ])
  in
  let r3, insts =
    match r3 with
    | Ir.Iop_reg r -> (r, insts)
    | Ir.Iop_imm imm ->
        let tmp = Mir.Reg.fresh () in
        (tmp, mk_movi tmp imm :: insts)
  in
  insts
  @ [
      mk_mov r1 r2;
      Mir.mk_inst kind
        [ Mir.Oreg r1; Mir.Oreg r3 ]
        ~defs:[ r1 ] ~uses:[ r1; r3 ];
    ]

let mk_binop_imm_util kind r1 r2 imm =
  let r2, insts =
    match r2 with
    | Ir.Iop_reg r -> (r, [])
    | Ir.Iop_imm imm ->
        let tmp = Mir.Reg.fresh () in
        (tmp, [ mk_movi tmp imm ])
  in
  insts
  @ [
      mk_mov r1 r2;
      Mir.mk_inst kind [ Mir.Oreg r1; Mir.Oimm imm ] ~defs:[ r1 ] ~uses:[ r1 ];
    ]

let mk_add r1 r2 r3 = mk_binop_util "add" r1 r2 r3
let mk_addi r1 r2 imm = mk_binop_imm_util "add" r1 r2 imm
let mk_sub r1 r2 r3 = mk_binop_util "sub" r1 r2 r3
let mk_subi r1 r2 imm = mk_binop_imm_util "sub" r1 r2 imm
let mk_imul r1 r2 r3 = mk_binop_util "imul" r1 r2 r3

let mk_imuli r1 r2 imm =
  let r2, insts =
    match r2 with
    | Ir.Iop_reg r -> (r, [])
    | Ir.Iop_imm imm ->
        let tmp = Mir.Reg.fresh () in
        (tmp, [ mk_movi tmp imm ])
  in
  insts
  @ [
      Mir.mk_inst "imul"
        [ Mir.Oreg r1; Mir.Oreg r2; Mir.Oimm imm ]
        ~defs:[ r1 ] ~uses:[ r1; r2 ];
    ]

let mk_cqo () =
  Mir.mk_inst "cqo" [] ~defs:[ X86Regs.eax; X86Regs.edx ] ~uses:[ X86Regs.eax ]

let mk_div_like_util kind rout r1 r2 r3 =
  let r2, insts =
    match r2 with
    | Ir.Iop_reg r -> (r, [])
    | Ir.Iop_imm imm ->
        let tmp = Mir.Reg.fresh () in
        (tmp, [ mk_movi tmp imm ])
  in
  let r3, insts =
    match r3 with
    | Ir.Iop_reg r -> (r, insts)
    | Ir.Iop_imm imm ->
        let tmp = Mir.Reg.fresh () in
        (tmp, mk_movi tmp imm :: insts)
  in
  insts
  @ [
      mk_mov X86Regs.eax r2;
      (* FIXME: CQO or CQD depending on x86 or x86-64 *)
      mk_cqo ();
      Mir.mk_inst kind [ Mir.Oreg r3 ]
        ~defs:[ X86Regs.eax; X86Regs.edx ]
        ~uses:[ r3 ];
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
  let r2, insts =
    match r2 with
    | Ir.Iop_reg r -> (r, [])
    | Ir.Iop_imm imm ->
        let tmp = Mir.Reg.fresh () in
        (tmp, [ mk_movi tmp imm ])
  in
  let r3, insts =
    match r3 with
    | Ir.Iop_reg r -> (r, insts)
    | Ir.Iop_imm imm ->
        let tmp = Mir.Reg.fresh () in
        (tmp, mk_movi tmp imm :: insts)
  in
  insts
  @
  let cl = X86Regs.ecx in
  [
    mk_mov cl r3;
    mk_mov r1 r2;
    Mir.mk_inst kind [ Mir.Oreg r1; Mir.Oreg cl ] ~uses:[ r1; cl ] ~defs:[ r1 ];
  ]

let mk_sal r1 r2 r3 = mk_shift_util "sal" r1 r2 r3
let mk_shl r1 r2 r3 = mk_shift_util "shl" r1 r2 r3
let mk_sar r1 r2 r3 = mk_shift_util "sar" r1 r2 r3
let mk_shr r1 r2 r3 = mk_shift_util "shr" r1 r2 r3

let mk_unop_util kind r1 r2 =
  let r2 =
    match r2 with Ir.Iop_reg r -> Mir.Oreg r | Ir.Iop_imm imm -> Mir.Oimm imm
  in
  [
    mk_mov_operand r1 r2;
    Mir.mk_inst kind [ Mir.Oreg r1 ] ~uses:[ r1 ] ~defs:[ r1 ];
  ]

let mk_not r1 r2 = mk_unop_util "not" r1 r2
let mk_neg r1 r2 = mk_unop_util "neg" r1 r2

let mk_cmp_util cc r1 r2 r3 =
  let r2, uses =
    match r2 with
    | Ir.Iop_reg r -> (Mir.Oreg r, [ r ])
    | Ir.Iop_imm imm -> (Mir.Oimm imm, [])
  in
  let r3, uses =
    match r3 with
    | Ir.Iop_reg r -> (Mir.Oreg r, r :: uses)
    | Ir.Iop_imm imm -> (Mir.Oimm imm, uses)
  in
  [
    (* FIXME: defs and uses *)
    Mir.mk_inst "cmp" [ r2; r3 ] ~defs:[] ~uses;
    Mir.mk_inst ("set" ^ cc) [ Oreg r1 ] ~defs:[ r1 ] ~uses:[];
    Mir.mk_inst "and" [ Oreg r1; Oimm 0xffn ] ~defs:[ r1 ] ~uses:[ r1 ];
  ]

let mk_push r1 =
  Mir.mk_inst "push" [ Oreg r1 ] ~defs:[ X86Regs.esp ] ~uses:[ X86Regs.esp; r1 ]

let mk_pop r1 =
  Mir.mk_inst "pop" [ Oreg r1 ] ~defs:[ X86Regs.esp; r1 ] ~uses:[ X86Regs.esp ]

let mk_pop_many arch count =
  (* TODO: fix item size * count *)
  ignore arch;
  mk_subi X86Regs.esp (Ir.Iop_reg X86Regs.esp) (Nativeint.of_int count)

let mk_call cc_info r1 fname args =
  [
    Mir.mk_inst "call"
      (Oreg r1 :: Ofunc fname :: args)
      ~defs:(Mir.Reg.Set.elements cc_info.cc_caller_saved)
      ~uses:[];
  ]

let mk_ret cc_info =
  [
    Mir.mk_inst "ret" [] ~defs:[]
      ~uses:(Mir.Reg.Set.elements cc_info.cc_callee_saved);
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
