module Label = Ir.Label
module LabelSet = Ir.LabelSet
module LabelMap = Ir.LabelMap

type x64_gpr =
  | X64_gpr_rax
  | X64_gpr_rbx
  | X64_gpr_rcx
  | X64_gpr_rdx
  | X64_gpr_rsi
  | X64_gpr_rdi
  | X64_gpr_rbp
  | X64_gpr_rsp
  | X64_gpr_r8
  | X64_gpr_r9
  | X64_gpr_r10
  | X64_gpr_r11
  | X64_gpr_r12
  | X64_gpr_r13
  | X64_gpr_r14
  | X64_gpr_r15

type x64_fpr =
  | X64_fpr_xmm0
  | X64_fpr_xmm1
  | X64_fpr_xmm2
  | X64_fpr_xmm3
  | X64_fpr_xmm4
  | X64_fpr_xmm5
  | X64_fpr_xmm6
  | X64_fpr_xmm7
  | X64_fpr_xmm8
  | X64_fpr_xmm9
  | X64_fpr_xmm10
  | X64_fpr_xmm11
  | X64_fpr_xmm12
  | X64_fpr_xmm13
  | X64_fpr_xmm14
  | X64_fpr_xmm15

type x64_gpr_size = X64_gpr_64 | X64_gpr_32 | X64_gpr_16 | X64_gpr_8
type x64_fpr_size = X64_fpr_128 | X64_fpr_256 | X64_fpr_512

type x64_physical_reg =
  | X64_gpr of x64_gpr * x64_gpr_size
  | X64_fpr of x64_fpr * x64_fpr_size

type x64_reg = { reg_id : int; reg_physical : x64_physical_reg option }
type x64_imm = Z.t

type x64_mem = {
  base : x64_reg option;
  index : x64_reg option;
  scale : int;
  displacement : int;
}

(** Checks if [mem] is a well-formed x64 memory operand. *)
let check_x64_mem mem =
  (match (mem.base, mem.index) with
  | None, None ->
      if mem.displacement < 0 then
        failwith
          "Invalid x64 memory operand: absolute addressing with negative \
           displacement"
  | None, Some _ -> failwith "Invalid x64 memory operand: no base with index"
  | _ -> ());

  match mem.scale with
  | 1 | 2 | 4 | 8 -> ()
  | _ -> failwith "Invalid x64 memory operand: scale must be 1, 2, 4, or 8"

let fit_mem_displacement disp = disp >= Int32.min_int && disp <= Int32.max_int

type reg_mem = X64_rom_reg of x64_reg | X64_rom_mem of x64_mem

type reg_mem_imm =
  | X64_rmi_reg of x64_reg
  | X64_rmi_mem of x64_mem
  | X64_rmi_imm of x64_imm

(** X64 condition codes for conditional instructions. *)
type x64_cc = X64_cc_e | X64_cc_ne

type x64_inst =
  | X64_inst_nop
  | X64_inst_mov of reg_mem * reg_mem_imm  (** [X64_inst_mov dst src]. *)
  | X64_inst_lea of x64_reg * x64_mem
  | X64_inst_add of reg_mem * reg_mem_imm
  | X64_inst_sub of reg_mem * reg_mem_imm
  | X64_inst_jmp of Label.t
  | X64_inst_jmpcc of x64_cc * Label.t * Label.t
      (** [X64_inst_jmpcc cc true_label false_label]. *)
  | X64_inst_ret
  | X64_inst_ud2

type x64_basic_block = { x64_bb_label : Label.t; x64_bb_insts : x64_inst list }
type x64_frame = { x64_frame_items : x64_reg list }

type x64_function = {
  x64_fn_name : string;
  x64_fn_blocks : x64_basic_block LabelMap.t;
  x64_fn_entry : Label.t;
  x64_fn_frame : x64_frame;
}
