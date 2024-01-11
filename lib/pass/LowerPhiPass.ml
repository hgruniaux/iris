open Ir

(** This pass lowers PHI nodes. After this call, there is no more PHI
    instructions in the IR code. Also the IR code clearly do not follow
    SSA form anymore.

    Lowering a PHI node is quite simple as we don't need to preserve
    SSA form anymore. The idea is to insert a move instruction at end of
    each predecessor basic block. So, for example, let consider:
      L1:
        %1 = ...
        jmp L3
      L2:
        %2 = ...
        jmp L3
      L3:
        %3 = phi [(%1, L1), (%2, L2)]
        ret %3
    Will be lowered to:
      L1:
        %1 = ...
        %3 = mov %1
        jmp L3
      L2:
        %2 = ...
        %3 = mov %2
        jmp L3
      L3:
        ret %3
    *)
let pass_fn fn =
  Label.Map.iter
    (fun _ bb ->
      iter_phis
        (fun phi ->
          let predecessors =
            match phi.i_kind with
            | Iinst_phi predecessors -> predecessors
            | _ -> failwith "expected a PHI instruction"
          in

          (* Insert mov instructions at end of predecessors blocks. *)
          List.iter
            (fun (operand, label) ->
              let pred = Label.Map.find label fn.fn_blocks in
              let mov_kind =
                match operand with
                | Iop_imm i -> Iinst_cst (Ir.get_int_constant fn.fn_ctx i)
                | Iop_reg r -> Iinst_mov r
              in
              (* It is important that the move instruction has the same name
                 as the PHI node. *)
              let mov_inst = mk_inst pred phi.i_name mov_kind in
              insert_at_end pred mov_inst)
            predecessors;

          (* Remove the PHI node from the symbol table so it can
             be GCed later. *)
          Hashtbl.remove fn.fn_symbol_table phi.i_name)
        bb;

      (* Remove all PHI nodes. *)
      bb.b_first_phi <- None)
    fn.fn_blocks
