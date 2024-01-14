open Ir

(** Checks if [bb] is unreachable.
    A basic block is considered unreachable if:
      - it has no predecessors (except the entry block)
      - it has only itself as predecessor *)
let is_unreachable bb =
  let is_predecessors_itself =
    Label.Set.cardinal bb.b_predecessors = 1
    && Label.Set.choose bb.b_predecessors = bb.b_label
  in
  (not (is_entry_bb bb))
  && (Label.Set.is_empty bb.b_predecessors || is_predecessors_itself)

let update_terminator bb old_label new_label =
  let term = Option.get bb.b_term in
  let replace_label label = if label = old_label then new_label else label in
  match term.i_kind with
  | Iinst_jmp label -> Ir.set_term bb (Iinst_jmp (replace_label label))
  | Iinst_jmpc (r, tl, el) ->
      let tl = replace_label tl in
      let el = replace_label el in
      if tl = el then Ir.set_term bb (Iinst_jmp tl)
      else Ir.set_term bb (Iinst_jmpc (r, tl, el))
  | _ -> ()

let merge_two_bbs from_bb to_bb =
  (* Move PHI nodes from [from_bb] to [to_bb]. *)
  List.iter (fun inst -> inst.i_bb <- to_bb) from_bb.b_phi_insts;
  to_bb.b_phi_insts <- from_bb.b_phi_insts @ to_bb.b_phi_insts;

  (* Move regular instructions. *)
  List.iter (fun inst -> inst.i_bb <- to_bb) from_bb.b_insts;
  to_bb.b_insts <- from_bb.b_insts @ to_bb.b_insts

let merge_into_successor bb succ =
  merge_two_bbs bb succ;

  succ.b_predecessors <- Label.Set.empty;

  (* Update bb's predecessor to point to [succ] now. *)
  Label.Set.iter
    (fun pred_label ->
      let pred = Label.Map.find pred_label bb.b_func.fn_blocks in
      succ.b_predecessors <- Label.Set.add pred_label succ.b_predecessors;
      update_terminator pred bb.b_label succ.b_label)
    bb.b_predecessors;

  bb.b_successors <- Label.Set.empty;
  bb.b_predecessors <- Label.Set.empty

let remove_bb fn bb =
  fn.fn_blocks <- Reg.Map.remove bb.b_label fn.fn_blocks;

  (* Update PHI nodes *)
  Reg.Set.iter
    (fun succ_label ->
      let succ = Reg.Map.find succ_label fn.fn_blocks in
      List.iter
        (fun phi ->
          phi.i_kind <-
            (match phi.i_kind with
            | Iinst_phi operands ->
                let new_operands =
                  List.filter (fun (_, label) -> label <> bb.b_label) operands
                in
                if new_operands != operands then Iinst_phi new_operands
                else phi.i_kind
            | _ -> failwith "expected a PHI instruction"))
        succ.b_phi_insts)
    bb.b_successors

(** This pass simplifies the CFG. It does simple dead code elimination and
    basic block merging following the below rules:
      - Basic blocks with no predecessors, except the function's entry, are
        unreachable and therefore removed.
      - Basic blocks with a single predecessor and the predecessor only has one
        successor can be merged.
*)
let pass_fn fn =
  let changed = ref false in
  let worklist = Queue.create () in
  Label.Map.iter (fun _ bb -> Queue.add bb worklist) fn.fn_blocks;

  while not (Queue.is_empty worklist) do
    let bb = Queue.take worklist in

    (* Check if bb was not already removed in a previous iteration. *)
    let still_exist = Label.Map.mem bb.b_label fn.fn_blocks in
    if not still_exist then ()
      (* Remove unreachable basic blocks. See is_unreachable for a definition of unreachable. *)
    else if is_unreachable bb then (
      changed := true;
      (* Remove bb from its successors' predecessors list. *)
      Label.Set.iter
        (fun succ_label ->
          let succ = Label.Map.find succ_label fn.fn_blocks in
          succ.b_predecessors <- Label.Set.remove bb.b_label succ.b_predecessors;
          Queue.add succ worklist)
        bb.b_successors;

      (* Remove all bb's instructions from the symbol table. *)
      iter_insts (fun inst -> Hashtbl.remove fn.fn_symbol_table inst.i_name) bb;

      remove_bb fn bb)
    else if
      (* Merges basic blocks into their predecessor if they have a single distinct
         predecessor and if the predecessor only has a single successor. Also, we
         don't merge such basic blocks when there is PHI nodes. *)
      Label.Set.cardinal bb.b_predecessors = 1 && bb.b_phi_insts = []
    then
      let pred_label = Label.Set.choose bb.b_predecessors in
      let pred = Label.Map.find pred_label bb.b_func.fn_blocks in

      if Label.Set.cardinal pred.b_successors = 1 then (
        changed := true;
        merge_into_successor pred bb;
        remove_bb fn pred;

        if Option.get fn.fn_entry = pred_label then
          fn.fn_entry <- Some bb.b_label;

        Label.Set.iter
          (fun succ_label ->
            let succ = Label.Map.find succ_label fn.fn_blocks in
            Queue.add succ worklist)
          bb.b_successors;
        Queue.add bb worklist)
  done;

  !changed
