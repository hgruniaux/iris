open Ir

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

let instsel_bb bb instsel_inst =
  let mir_insts =
    (* FIXME: mir_insts @ ... is SLOW, we should use fold_right *)
    Ir.fold_insts (fun mir_insts inst -> mir_insts @ instsel_inst inst) [] bb
  in
  {
    Mir.bb_label = bb.b_label;
    Mir.bb_insts = mir_insts;
    Mir.bb_predecessors = bb.b_predecessors;
    Mir.bb_successors = bb.b_successors;
  }

let instsel_fn fn instsel_bb =
  let mir_blocks = Label.Map.map instsel_bb fn.fn_blocks in
  {
    Mir.fn_name = fn.fn_name;
    Mir.fn_params = fn.fn_params;
    Mir.fn_blocks = mir_blocks;
    Mir.fn_entry = Option.get fn.fn_entry;
    Mir.fn_frame = None;
  }
