open Ir

let range_and_density_of targets =
  match targets with
  | [] -> failwith "empty targets for switch terminator"
  | (first_value, _) :: _ ->
      let min_index, max_index, count =
        List.fold_left
          (fun (min_index, max_index, count) (value, _) ->
            let min_index = Z.min min_index value in
            let max_index = Z.max max_index value in
            let count = count + 1 in
            (min_index, max_index, count))
          (first_value, first_value, 0)
          targets
      in

      let range = Z.(max_index - min_index) in
      (* Density is a measure of how much there is holes in the index set. *)
      let density = Q.(Q.of_bigint range / Q.of_int count) in
      (range, density)

let lower_to_linear_search fn bb index_reg otherwise targets =
  let last_bb =
    List.fold_left
      (fun bb (value, label) ->
        let new_bb = BasicBlock.create fn in
        let cond_reg = Reg.fresh () in
        let cond_inst =
          Instruction.create fn cond_reg
            (Iinst_cmp (Icmp_eq, Iop_reg index_reg, Iop_imm value))
        in
        bb.b_insts <- bb.b_insts @ [ cond_inst ];
        BasicBlock.set_term fn bb
          (Iterm_jmpc (Iop_reg cond_reg, label, new_bb.b_label));
        new_bb)
      bb targets
  in
  BasicBlock.set_term fn last_bb (Iterm_jmp otherwise)

let lower_switch fn bb index otherwise targets =
  match index with
  | Iop_imm imm -> (
      (* The index is constant, the switch instruction can be lowered to a single jump! *)
      let target_label =
        List.find_opt (fun (value, _) -> Z.(value = imm)) targets
      in
      match target_label with
      | None -> BasicBlock.set_term fn bb (Iterm_jmp otherwise)
      | Some (_, target_label) ->
          BasicBlock.set_term fn bb (Iterm_jmp target_label))
  | Iop_reg index_reg ->
      (* The index is not constant... *)
      let range, density = range_and_density_of targets in
      ignore range;
      ignore density;
      (* TODO: support jump tables and binary search for switch terminator *)
      lower_to_linear_search fn bb index_reg otherwise targets

let pass_fn am fn =
  Label.Map.iter
    (fun _ bb ->
      match bb.b_term with
      | Iterm_switch (index, otherwise, targets) ->
          lower_switch fn bb index otherwise targets;
          AnalysisManager.mark_as_dirty am
      | _ -> ())
    fn.fn_blocks;

  am.am_dirty
