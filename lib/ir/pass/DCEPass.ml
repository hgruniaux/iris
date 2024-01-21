open Ir

(** This pass removes dead instructions if possible.

    All instructions that have no side effects and that are not used in
    the program are removed. *)
let pass_fn am fn =
  let used_names = Hashtbl.create 17 in

  (*
    We use a kind of Mark & Sweep algorithm:
      - We first iterate all the instruction to mark the used ones.
      - Then, we delete all instructions that were not marked and that
        have no side effects.
      - We eventually repeat the process, because removing one instruction
        may render other instructions dead.
  *)
  let mark_reg r = Hashtbl.add used_names r true in
  let mark_operand o = match o with Iop_reg r -> mark_reg r | _ -> () in

  (* Mark used instructions. *)
  Label.Map.iter
    (fun _ bb ->
      let mark_inst inst =
        match inst.i_kind with
        | Iinst_loadi _ | Iinst_cst _ -> ()
        | Iinst_mov r | Iinst_load (_, r) | Iinst_loadfield (_, r, _) ->
            mark_reg r
        | Iinst_store (r, o) | Iinst_storefield (_, r, _, o) ->
            mark_reg r;
            mark_operand o
        | Iinst_binop (_, o1, o2) | Iinst_cmp (_, o1, o2) ->
            mark_operand o1;
            mark_operand o2
        | Iinst_unop (_, o) -> mark_operand o
        | Iinst_call (_, args) -> List.iter mark_operand args
        | Iinst_phi args -> List.iter (fun (o, _) -> mark_operand o) args
      in

      List.iter mark_inst bb.b_phi_insts;
      List.iter mark_inst bb.b_insts;
      match bb.b_term with
      | Iterm_jmpc (o, _, _) | Iterm_retv o | Iterm_switch (o, _, _) ->
          mark_operand o
      | Iterm_jmp _ | Iterm_ret | Iterm_unreachable -> ())
    fn.fn_blocks;

  let removed_insts = ref 0 in

  (* Then sweep unused instructions that have no side effects. *)
  Label.Map.iter
    (fun _ bb ->
      (* Sweep PHI instructions (easy, they can not have side effects). *)
      bb.b_phi_insts <-
        List.filter
          (fun inst ->
            let keep = Hashtbl.mem used_names inst.i_name in
            if not keep then (
              incr removed_insts;
              Hashtbl.remove fn.fn_symbol_table inst.i_name);
            keep)
          bb.b_phi_insts;

      (* Sweep unused instructions but keeping the ones with side effects. *)
      bb.b_insts <-
        List.filter
          (fun inst ->
            let keep =
              Instruction.may_have_side_effects inst.i_kind
              || Hashtbl.mem used_names inst.i_name
            in
            if not keep then (
              incr removed_insts;
              Hashtbl.remove fn.fn_symbol_table inst.i_name);
            keep)
          bb.b_insts)
    (* Terminator instructions have side effects by definition. They can not be removed. *)
    fn.fn_blocks;

  AnalysisManager.keep_cfg am;
  (* The code has changed if we have removed at least one instruction. *)
  if !removed_insts > 0 then (
    AnalysisManager.mark_as_dirty am;
    true)
  else false
