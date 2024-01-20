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
module Label = Ir.Label
module Constant = Ir.Constant

type reg = Ir.reg
type label = Label.t
type imm = Z.t
type constant = Constant.t

type frame = {
  frame_params : int;  (** Count of parameters stored in frame. *)
  frame_locals : int;  (** Count of local variables stored in frame. *)
}

type calling_convention_info = {
  cc_caller_saved : Reg.set;  (** The caller-saved (volatile) registers. *)
  cc_callee_saved : Reg.set;  (** The callee-saved (non-volatile) registers. *)
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
  | Oconst of constant (* a constant label *)
  | Olabel of label (* a basic block label (for jump instructions) *)
  | Ofunc of Ir.fn (* a function (for call instructions) *)
  | Omem of reg * int * int

and minst = {
  mi_kind : string;
      (** The instruction's kind. It is an opaque string which the real meaning
          is only known by the backend that created this instruction. *)
  mutable mi_operands : operand list;
      (** The operands of this instruction. They can be registers, immediates or labels. *)
  mutable mi_defs : Reg.set;
      (** The registers defined by this instruction. Used for liveness analysis. *)
  mutable mi_uses : Reg.set;
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
  mbb_predecessors : Label.set;  (** The set of basic block predecessors. *)
  mbb_successors : Label.set;  (** The set of basic block successors. *)
}
(** Machine basic block. It corresponds to the vertices of the CFG. *)

and mfn = {
  mfn_name : string;
  mfn_params : reg list;
  mfn_blocks : mbb Label.map;
  mfn_entry : label;
  mfn_cc_info : calling_convention_info;
  mutable mfn_frame : frame option;
}
(** Machine function. *)

let mk_inst ?(is_mov = false) kind operands ~defs ~uses =
  {
    mi_kind = kind;
    mi_defs = Reg.Set.of_list defs;
    mi_uses = Reg.Set.of_list uses;
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
    mi_defs = Reg.Set.singleton output_reg;
    mi_uses = Reg.Set.empty;
    mi_operands = [ Oreg output_reg; Oframe stack_idx ];
    mi_is_mov = true;
  }

let mk_stack_store stack_idx input_reg =
  {
    mi_kind = "mov";
    mi_defs = Reg.Set.empty;
    mi_uses = Reg.Set.singleton input_reg;
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
          | Oreg r when Reg.is_pseudo r -> Reg.Set.add r regs
          | _ -> regs)
        regs inst.mi_operands)
    Reg.Set.empty bb.mbb_insts

(** Collects all pseudo registers used in the given [fn]. *)
let collect_pseudo_registers_in_fn fn =
  Label.Map.fold
    (fun _ bb regs -> Reg.Set.union regs (collect_pseudo_registers_in_bb bb))
    fn.mfn_blocks Reg.Set.empty
