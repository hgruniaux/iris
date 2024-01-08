open Ir

(** Checks if [bb] is unreachable. A basic block is considered unreachable if:
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
  let it = ref from_bb.b_first_phi in
  while Option.is_some !it do
    let phi = Option.get !it in
    it := phi.i_next;
    phi.i_bb <- to_bb;
    match to_bb.b_first_phi with
    | None -> to_bb.b_first_phi <- Some phi
    | Some first_phi ->
        insert_before first_phi phi;
        to_bb.b_first_phi <- Some phi
  done;

  (* Move regular instructions. *)
  it := from_bb.b_last_inst;
  while Option.is_some !it do
    let inst = Option.get !it in
    it := inst.i_prev;
    inst.i_bb <- to_bb;
    insert_at_start to_bb inst
  done

let merge_into_successor bb succ =
  merge_two_bbs bb succ;

  succ.b_predecessors <- Label.Set.empty;

  (* Update bb's predecessor to point to [succ] now. *)
  Label.Set.iter
    (fun pred_label ->
      let pred = Label.Map.find pred_label bb.b_func.fn_blocks in
      succ.b_predecessors <- Label.Set.singleton pred_label;
      update_terminator pred bb.b_label succ.b_label)
    bb.b_predecessors;

  bb.b_successors <- Label.Set.empty;
  bb.b_predecessors <- Label.Set.empty

(** This pass simplifies the CFG. It does simple dead code elimination and
    basic block merging following the below rules:
      - Basic blocks with no predecessors, except the function's entry, are
        unreachable and therefore removed.
      - Basic blocks with a single predecessor and the predecessor only has one
        successor can be merged.
    *)
let pass_fn fn =
  let worklist = Queue.create () in
  Label.Map.iter (fun _ bb -> Queue.add bb worklist) fn.fn_blocks;

  while not (Queue.is_empty worklist) do
    let bb = Queue.take worklist in

    (* Check if bb was not already removed in a previous iteration. *)
    let still_exist = Label.Map.mem bb.b_label fn.fn_blocks in
    if not still_exist then ()
      (* Remove unreachable basic blocks. See is_unreachable for a definition of unreachable. *)
    else if is_unreachable bb then (
      (* Remove bb from its successors' predecessors list. *)
      Label.Set.iter
        (fun succ_label ->
          let succ = Label.Map.find succ_label fn.fn_blocks in
          succ.b_predecessors <- Label.Set.remove bb.b_label succ.b_predecessors;
          Queue.add succ worklist)
        bb.b_successors;
      fn.fn_blocks <- Label.Map.remove bb.b_label fn.fn_blocks)
    else if
      (* Merges basic blocks into their predecessor if they have a single distinct
         predecessor and if the predecessor only has a single successor. Also, we
         don't merge such basic blocks when there is PHI nodes. *)
      Label.Set.cardinal bb.b_predecessors = 1 && Option.is_none bb.b_first_phi
    then
      let pred_label = Label.Set.choose bb.b_predecessors in
      let pred = Label.Map.find pred_label bb.b_func.fn_blocks in

      if Label.Set.cardinal pred.b_successors = 1 then (
        merge_into_successor pred bb;
        fn.fn_blocks <- Label.Map.remove pred_label fn.fn_blocks;

        Label.Set.iter
          (fun succ_label ->
            let succ = Label.Map.find succ_label fn.fn_blocks in
            Queue.add succ worklist)
          bb.b_successors;
        Queue.add bb worklist)
  done
