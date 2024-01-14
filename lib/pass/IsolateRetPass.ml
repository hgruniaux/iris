open Ir

let is_ret term =
  match term with
  | None -> (false, None)
  | Some term -> (
      match term.i_kind with
      | Iinst_ret -> (true, None)
      | Iinst_retv v -> (true, Some v)
      | _ -> (false, None))

(** This pass moves all occurrences of ret or retv instructions to a same and
    unique basic block.

    The SimplifyCFG pass should be called after as this pass may mess up the CFG. *)
let pass_fn fn =
  let ret_bb = Ir.mk_bb fn in

  let predecessors = ref [] in
  Label.Map.iter
    (fun _ bb ->
      let is_ret, ret_value = is_ret bb.b_term in
      if is_ret then
        match ret_value with
        | None -> set_term bb (Iinst_jmp ret_bb.b_label)
        | Some v ->
            predecessors := (v, bb.b_label) :: !predecessors;
            set_term bb (Iinst_jmp ret_bb.b_label))
    fn.fn_blocks;

  (if !predecessors = [] then set_term ret_bb Iinst_ret
   else
     let ret_value = insert_phi ret_bb !predecessors in
     set_term ret_bb (Iinst_retv (Iop_reg ret_value)));

  (* As a simplification, we suppose that this pass always modify the code. *)
  true
