(* This file defines the MIR, the machine intermediate representation. Unlike,
 * the IR this representation is backend dependent and may not be anymore in SSA
 * form. However, the representation is backend dependent but in the same way
 * shared by all backends. That is, the MIR instructions are defined in a very
 * abstract way as an opcode with a list of defined registers and a list of
 * used registers. What is backend dependent, is the mean and the definition
 * of opcodes.
 *
 * The MIR is still a CFG with basic blocks. Moreover, a program is still
 * decomposed into functions just like the IR. What really change is the
 * way instructions are represented inside basic blocks.
 *
 * This representation is generally generated from the IR by the backend's
 * specific instruction-sel pass (instsel). *)

open IrPP
module Reg = Ir.Reg
module Label = Ir.Label
module Constant = Ir.Constant

type reg = Ir.reg
type label = Label.t
type imm = Z.t
type constant = Constant.t

type operand =
  | Oreg of reg (* a register *)
  | Oframe of int (* a frame index (for register spilling for example) *)
  | Oimm of imm (* an immediate *)
  | Oconst of constant (* a constant label *)
  | Olabel of label (* a basic block label (for jump instructions) *)
  | Ofunc of Ir.fn (* a function (for call instructions) *)

type inst = {
  i_kind : string;
      (** The instruction's kind. It is an opaque string which the real meaning
          is only known by the backend that created this instruction. *)
  mutable i_operands : operand list;
      (** The operands of this instruction. They can be registers, immediates or labels. *)
  mutable i_defs : Reg.set;
      (** The registers defined by this instruction. Used for liveness analysis. *)
  mutable i_uses : Reg.set;
      (** The registers used by this instruction. Used for liveness analysis. *)
  i_is_mov : bool;
      (** True if this instruction implements a trivial register move operation.
          That is, it simply copies the second operand (a register) to the first
          operand (a register also). Such instructions are treated specifically by the
          interference graph maker. *)
}

type frame = {
  frame_params : int;  (** Count of parameters stored in frame. *)
  frame_locals : int;  (** Count of local variables stored in frame. *)
}

type bb = {
  bb_label : Label.t;  (** The basic block label (unique name). *)
  mutable bb_insts : inst list;
      (** The list of instructions of this basic block. *)
  bb_predecessors : Label.set;  (** The set of basic block predecessors. *)
  bb_successors : Label.set;  (** The set of basic block successors. *)
}

and fn = {
  fn_name : string;
  fn_params : reg list;
  fn_blocks : bb Label.map;
  fn_entry : label;
  mutable fn_frame : frame option;
}

let mk_inst ?(is_mov = false) kind operands ~defs ~uses =
  {
    i_kind = kind;
    i_defs = Reg.Set.of_list defs;
    i_uses = Reg.Set.of_list uses;
    i_operands = operands;
    i_is_mov = is_mov;
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

let stack_load_kind = "__load_stack"
let stack_store_kind = "__store_stack"

let mk_stack_load output_reg stack_idx =
  {
    i_kind = "mov";
    i_defs = Reg.Set.singleton output_reg;
    i_uses = Reg.Set.empty;
    i_operands = [ Oreg output_reg; Oframe stack_idx ];
    i_is_mov = true;
  }

let mk_stack_store stack_idx input_reg =
  {
    i_kind = "mov";
    i_defs = Reg.Set.empty;
    i_uses = Reg.Set.singleton input_reg;
    i_operands = [ Oframe stack_idx; Oreg input_reg ];
    i_is_mov = true;
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
        regs inst.i_operands)
    Reg.Set.empty bb.bb_insts

(** Collects all pseudo registers used in the given [fn]. *)
let collect_pseudo_registers_in_fn fn =
  Label.Map.fold
    (fun _ bb regs -> Reg.Set.union regs (collect_pseudo_registers_in_bb bb))
    fn.fn_blocks Reg.Set.empty

(*
 * Pretty printer for debugging purposes:
 *)

let pp_operand ppf op =
  match op with
  | Oreg r -> pp_register ppf r
  | Oframe n -> Format.fprintf ppf "STACK[%d]" n
  | Oimm i -> Z.pp_print ppf i
  | Oconst c -> Format.fprintf ppf "%a" pp_constant c
  | Olabel l -> Format.fprintf ppf "%a" pp_label l
  | Ofunc fn -> Format.fprintf ppf "%s" fn.fn_name

let pp_inst ppf inst =
  Format.fprintf ppf "%s %a" inst.i_kind (pp_list pp_operand) inst.i_operands

let pp_bb pp_extra_bb pp_extra_inst ppf bb =
  Format.fprintf ppf "%a: %a@." pp_label bb.bb_label pp_extra_bb bb;
  List.iter
    (fun inst -> Format.fprintf ppf "  %a %a@." pp_inst inst pp_extra_inst inst)
    bb.bb_insts

let pp_preds_and_succs ppf bb =
  if not (Label.Set.is_empty bb.bb_predecessors) then
    Format.fprintf ppf "; preds = %a" pp_labelset bb.bb_predecessors

let pp_defs_and_uses ppf inst =
  Format.fprintf ppf "; defs = {%a}, uses = {%a}" pp_registerset inst.i_defs
    pp_registerset inst.i_uses

let pp_fn pp_extra_bb pp_extra_inst ppf fn =
  Format.fprintf ppf "fn %s() {\n" fn.fn_name;
  Label.Map.iter
    (fun _ bb -> pp_bb pp_extra_bb pp_extra_inst ppf bb)
    fn.fn_blocks;
  Format.fprintf ppf "}"

let pp ppf fns =
  List.iter (fun fn -> pp_fn (fun _ _ -> ()) (fun _ _ -> ()) ppf fn) fns

let pp_extra ppf fns =
  List.iter (fun fn -> pp_fn pp_preds_and_succs pp_defs_and_uses ppf fn) fns

let dump_mir fns = Format.printf "%a@." pp_extra fns
