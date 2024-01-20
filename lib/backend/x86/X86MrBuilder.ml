open Mr
open X86Regs

let spiller_registers = X86Regs.spill_regs

(** Returns the calling convention used for the given [fn]. *)
let cc_info_of _ = X86Mir.x64_cc_info

(** Returns the size, in bytes, of the given [operand]. *)
let sizeof_operand _ = 8

(** Checks if [minst] represents a x86 CALL instruction. *)
let is_call minst = minst.mi_kind = "call"

(** Creates a x86 MOV instruction moving the operand [op] to the register [reg]. *)
let mk_mov_operand reg op =
  let use = match op with Oreg r -> [ r ] | _ -> [] in
  [ Mr.mk_inst "mov" [ Mr.Oreg reg; op ] ~defs:[ reg ] ~uses:use ~is_mov:true ]

(** Creates a x86 MOV instruction moving the register [reg_in] to the register [reg_out]. *)
let mk_mov_reg reg_out reg_in =
  [
    Mr.mk_inst "mov"
      [ Mr.Oreg reg_out; Mr.Oreg reg_in ]
      ~defs:[ reg_out ] ~uses:[ reg_in ] ~is_mov:true;
  ]

(** Creates a x86 PUSH instruction pushing the given [op] to the stack. *)
let mk_push_operand op =
  let use = match op with Oreg r -> [ r ] | _ -> [] in
  [ Mr.mk_inst "push" [ op ] ~defs:[ X86Regs.rsp ] ~uses:use ]

(** Creates a x86 PUSH instruction pushing the given [reg] to the stack. *)
let mk_push_reg reg =
  [ Mr.mk_inst "push" [ Mr.Oreg reg ] ~defs:[ X86Regs.rsp ] ~uses:[ reg ] ]

(** Creates a x86 instruction poping the given amount of bytes.
    This lower to an ADD instruction. *)
let mk_pop_bytes count =
  let insts = ref [] in
  X86Mir.insert_add insts X86Regs.rsp (Ir.Iop_reg X86Regs.rsp)
    (Ir.Iop_imm (Z.of_int count));
  !insts

(** Creates a x86 POP instruction poping the top element of stack and storing in
    the given register. *)
let mk_pop_register reg =
  [ Mr.mk_inst "pop" [ Mr.Oreg reg ] ~defs:[ X86Regs.rsp; reg ] ~uses:[] ]

(** Creates a x86 CALL instruction. *)
let mk_call callee reg_defs =
  [ mk_inst "call" [ Ofunc callee ] ~defs:(Reg.Set.elements reg_defs) ~uses:[] ]

(** Creates a x86 instruction that loads the requested frame object into [reg]. *)
let mk_frame_load reg frame_idx =
  [
    Mr.mk_inst "mov" [ Mr.Oreg reg; Mr.Oframe frame_idx ] ~defs:[ reg ] ~uses:[];
  ]

(** Creates a x86 instruction that stores the given [reg] into the requested frame slot. *)
let mk_frame_store frame_idx reg =
  [
    Mr.mk_inst "mov" [ Mr.Oframe frame_idx; Mr.Oreg reg ] ~defs:[] ~uses:[ reg ];
  ]

(** Generates the x86 prologue for the given [mfn]. *)
let prolog mfn =
  let insts = ref [] in
  X86Mir.insert_frame_alloc insts mfn;
  List.rev !insts

(** Generates the x86 epilogue for the given [mfn]. *)
let epilog mfn =
  let insts = ref [] in
  X86Mir.insert_frame_dealloc insts mfn;
  List.rev !insts
