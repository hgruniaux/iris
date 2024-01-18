open Ir

(** Checks if the [term] instruction is a return. Also returns the
    return value in case of a value return instruction (Iinst_retv). *)
let is_ret term =
  match term with
  | Iterm_ret -> (true, None)
  | Iterm_retv v -> (true, Some v)
  | Iterm_jmp _ | Iterm_jmpc _ | Iterm_switch _ | Iterm_unreachable -> (false, None)

(** This pass moves all occurrences of ret or retv instructions to a same and
    unique basic block.

    This optimization is intended to simplify other optimizations and the CFG.
    Also, in case of architectures where the function is prologue is heavy,
    this pass avoids generating multiple times.

    The SimplifyCFG pass should be called after, as this pass may mess up the CFG. *)
let pass_fn am fn =
  let ret_bb = BasicBlock.create fn in

  let predecessors = ref [] in
  Label.Map.iter
    (fun _ bb ->
      let is_ret, ret_value = is_ret bb.b_term in
      if is_ret then
        match ret_value with
        | None -> BasicBlock.set_term fn bb (Iterm_jmp ret_bb.b_label)
        | Some v ->
            predecessors := (v, bb.b_label) :: !predecessors;
            BasicBlock.set_term fn bb (Iterm_jmp ret_bb.b_label))
    fn.fn_blocks;

  (if !predecessors = [] then BasicBlock.set_term fn ret_bb Iterm_ret
   else
     let ret_value = Instruction.insert_phi fn ret_bb !predecessors in
     BasicBlock.set_term fn ret_bb (Iterm_retv (Iop_reg ret_value)));

  AnalysisManager.mark_as_dirty am;
  am.am_dirty
