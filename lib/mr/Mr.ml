(* This file defines the MR, the machine representation. Unlike,
 * the IR this representation is backend dependent and is not anymore in SSA
 * form. However, the representation is backend dependent but in the same way
 * shared by all backends. That is, the MR instructions are defined in a very
 * abstract way as an opcode with a list of defined registers and a list of
 * used registers. What is backend dependent, is the mean and the definition
 * of opcodes.
 *
 * The MR is still a CFG with basic blocks. Moreover, a program is still
 * decomposed into functions just like the IR. What really change is the
 * way instructions are represented inside basic blocks.
 *
 * This representation is generally generated from the IR by the backend's
 * specific instruction-sel pass (instsel). *)

module Reg = Ir.Reg
module RegSet = Ir.RegSet
module RegMap = Ir.RegMap

module Label = Ir.Label
module LabelMap = Ir.LabelMap
module LabelSet = Ir.LabelSet

module Global = Ir.Global

type reg = Ir.reg
type label = Label.t
type imm = Z.t
type constant = Global.t
type global = Ir.global

type frame = {
  frame_params : int;  (** Count of parameters stored in frame. *)
  frame_locals : int;  (** Count of local variables stored in frame. *)
}

type calling_convention_info = {
  cc_caller_saved : RegSet.t;  (** The caller-saved (volatile) registers. *)
  cc_callee_saved : RegSet.t;  (** The callee-saved (non-volatile) registers. *)
  cc_args_regs : Reg.t list;
      (** A list of physical registers used to pass arguments, in order. *)
  cc_args_regs_count : int;
      (** Count of arguments that are passed by physical registers.
          Must be the length of [cc_args_regs]. *)
  cc_args_stack_ltr : bool;
      (** True if the arguments in the stack are passed from left to right; false otherwise. *)
  cc_return_reg : Reg.t option;
      (** The physical register where the function's return value must be stored.
      None if the return value is stored in the stack. *)
  cc_caller_cleanup : bool;
      (** True if the caller has to clean the stack (pop arguments passed in the stack if any),
          false if it is the callee. *)
}

type operand =
  | Oreg of reg (* a register *)
  | Oframe of int (* a frame index (for register spilling for example) *)
  | Oimm of imm (* an immediate *)
  | Oglobal of global (* a global label *)
  | Olabel of label (* a basic block label (for jump instructions) *)
  | Ofunc of Ir.fn (* a function (for call instructions) *)
  | Omem of reg * int * int

and minst = {
  mi_kind : string;
      (** The instruction's kind. It is an opaque string which the real meaning
          is only known by the backend that created this instruction. *)
  mutable mi_operands : operand list;
      (** The operands of this instruction. They can be registers, immediates or labels. *)
  mutable mi_defs : RegSet.t;
      (** The registers defined by this instruction. Used for liveness analysis. *)
  mutable mi_uses : RegSet.t;
      (** The registers used by this instruction. Used for liveness analysis. *)
  mi_is_mov : bool;
      (** True if this instruction implements a trivial register move operation.
          That is, it simply copies the second operand (a register) to the first
          operand (a register also). Such instructions are treated specifically by the
          interference graph maker. *)
}
(** Machine instruction. The exact format of instructions is specific to the backend. *)

and mbb = {
  mbb_label : Label.t;  (** The basic block label (unique name). *)
  mutable mbb_insts : minst list;
      (** The list of instructions of this basic block. *)
  mbb_predecessors : LabelSet.t;  (** The set of basic block predecessors. *)
  mbb_successors : LabelSet.t;  (** The set of basic block successors. *)
}
(** Machine basic block. It corresponds to the vertices of the CFG. *)

and mfn = {
  mfn_name : string;
  mfn_params : reg list;
  mfn_blocks : mbb LabelMap.t;
  mfn_entry : Label.t;
  mfn_cc_info : calling_convention_info;
  mutable mfn_frame : frame option;
}
(** Machine function. *)

let mk_inst ?(is_mov = false) kind operands ~defs ~uses =
  {
    mi_kind = kind;
    mi_defs = RegSet.of_list defs;
    mi_uses = RegSet.of_list uses;
    mi_operands = operands;
    mi_is_mov = is_mov;
  }

let mk_mov target source =
  mk_inst ~is_mov:true "mov"
    [ Oreg target; Oreg source ]
    ~defs:[ target ] ~uses:[ source ]

let mk_push source =
  let uses = match source with Oreg r -> [ r ] | _ -> [] in
  mk_inst "push" [ source ] ~defs:[] ~uses

let mk_pop target =
  let defs = match target with Oreg r -> [ r ] | _ -> [] in
  mk_inst "pop" [ target ] ~defs ~uses:[]

let mk_stack_load output_reg stack_idx =
  {
    mi_kind = "mov";
    mi_defs = RegSet.singleton output_reg;
    mi_uses = RegSet.empty;
    mi_operands = [ Oreg output_reg; Oframe stack_idx ];
    mi_is_mov = true;
  }

let mk_stack_store stack_idx input_reg =
  {
    mi_kind = "mov";
    mi_defs = RegSet.empty;
    mi_uses = RegSet.singleton input_reg;
    mi_operands = [ Oframe stack_idx; Oreg input_reg ];
    mi_is_mov = true;
  }

(** Collects all pseudo registers used in the given [bb]. *)
let collect_pseudo_registers_in_bb bb =
  List.fold_left
    (fun regs inst ->
      List.fold_left
        (fun regs op ->
          match op with
          | Oreg r when Reg.is_pseudo r -> RegSet.add r regs
          | _ -> regs)
        regs inst.mi_operands)
    RegSet.empty bb.mbb_insts

(** Collects all pseudo registers used in the given [fn]. *)
let collect_pseudo_registers_in_fn fn =
  LabelMap.fold
    (fun _ bb regs -> RegSet.union regs (collect_pseudo_registers_in_bb bb))
    fn.mfn_blocks RegSet.empty
