(*
 * Copyright (c) 2025 Hubert Gruniaux
 * This file is part of the Iris project.
 *)

open Ir_base
module Cfg = Ir_cfg
module Function = Ir_function
module Block = Ir_block

module BlockReachabilityAnalysis = struct
  type g = Cfg.t
  type vertex = Cfg.V.t
  type edge = Cfg.E.t
  type data = bool

  let direction = Graph.Fixpoint.Forward
  let join d1 d2 = d1 || d2
  let equal d1 d2 = d1 = d2
  let analyze _ = fun x -> x
end

module BlockReachability = Cfg.Fixpoint (BlockReachabilityAnalysis)

(** Removes all basic blocks that are unreachable on the provided function. It
    does this by performing a reachability analysis. Returns [true] if some
    modification was made. *)
let remove_unreachable_blocks fn =
  let is_entry bb = bb.block_label = Option.get fn.fn_entry in
  let is_reachable = BlockReachability.analyze is_entry fn in
  let has_changed = ref false in
  fn.fn_blocks <-
    LabelMap.filter
      (fun _ bb ->
        let to_keep = is_reachable bb in
        if not to_keep then has_changed := true;
        to_keep)
      fn.fn_blocks;
  !has_changed

(** Merges basic blocks that belong to the same leader list into a single basic
    block. Returns [true] if some modification was made. *)
let merge_leader_list fn =
  let entry_block = Option.get (Function.entry_block fn) in
  let meta_basic_blocks = Cfg.Leaderlist.leader_lists fn entry_block in
  let blocks_to_remove = ref LabelSet.empty in
  List.iter
    (fun meta_basic_block ->
      let leader_block = List.hd meta_basic_block in
      leader_block.block_insts <-
        List.flatten (List.map (fun bb -> bb.block_insts) meta_basic_block);

      let tail_block = List.hd (List.rev meta_basic_block) in
      let new_term = tail_block.block_term in
      (* Sets the terminator of the tail block to unreachable to removes
         tail block from the predecessors of the successor of the meta block.
         This is required because tail block will be removed and we don't
         want danling references. *)
      Block.set_term fn tail_block Iterm_unreachable;

      (* Mark all blocks of this meta basic block, except the leader one, to be removed! *)
      List.iter
        (fun bb ->
          blocks_to_remove := LabelSet.add bb.block_label !blocks_to_remove)
        (List.tl meta_basic_block);

      Block.set_term fn leader_block new_term)
    meta_basic_blocks;

  (* Remove blocks marked to be removed *)
  fn.fn_blocks <-
    LabelMap.filter
      (fun label _ -> not (LabelSet.mem label !blocks_to_remove))
      fn.fn_blocks;

  not (LabelSet.is_empty !blocks_to_remove)

(** Merges basic blocks that have no instructions and only an unconditional
    branch as terminator with their predecessors. *)
let merge_trivial_blocks fn =
  (* Step 1: Find trivial basic blocks.
     [trivial_blocks] is a mapping from a trivial basic block label to its
     target and target' arguments. *)
  let trivial_blocks = Hashtbl.create 16 in
  Function.iter_blocks
    (fun block ->
      (* TODO: It should be easy to add support for block parameters *)
      if block.block_insts = [] && block.block_params = [] then
        match block.block_term with
        | Iterm_br (target_label, args) ->
            Hashtbl.add trivial_blocks block.block_label (target_label, args);
            (* Set the terminator to unreachable to update predecessors and successors
             information as this block will be removed. *)
            Block.set_term fn block Iterm_unreachable
        | _ -> ())
    fn;

  (* Step 2: Remove trivial basic blocks, they are no more needed. *)
  fn.fn_blocks <-
    LabelMap.filter
      (fun label _ -> not (Hashtbl.mem trivial_blocks label))
      fn.fn_blocks;

  (* Step 3: Replace jumps to trivial basic blocks by jumps to the trivial basic block successor. *)
  let map_label label args =
    match Hashtbl.find_opt trivial_blocks label with
    | Some (target_label, target_args) -> (target_label, target_args)
    | None -> (label, args)
  in

  Function.iter_blocks
    (fun block ->
      let new_term =
        match block.block_term with
        | Iterm_br (target_label, args) ->
            let new_target_label, new_args = map_label target_label args in
            Iterm_br (new_target_label, new_args)
        | Iterm_br_if (cond, true_label, true_args, false_label, false_args) ->
            let new_true_label, new_true_args =
              map_label true_label true_args
            in
            let new_false_label, new_false_args =
              map_label false_label false_args
            in
            Iterm_br_if
              ( cond,
                new_true_label,
                new_true_args,
                new_false_label,
                new_false_args )
        | Iterm_br_table (cond, default_label, default_args, cases) ->
            let new_default_label, new_default_args =
              map_label default_label default_args
            in
            let new_cases =
              List.map
                (fun (case_cond, case_label, case_args) ->
                  let new_case_label, new_case_args =
                    map_label case_label case_args
                  in
                  (case_cond, new_case_label, new_case_args))
                cases
            in
            Iterm_br_table (cond, new_default_label, new_default_args, new_cases)
        | _ -> block.block_term
      in
      block.block_succ <- LabelSet.empty;
      Block.set_term fn block new_term)
    fn;

  Hashtbl.length trivial_blocks > 0

let simplify_cfg fn =
  let has_changed = ref true in
  while !has_changed do
    has_changed := remove_unreachable_blocks fn;
    has_changed := !has_changed || merge_leader_list fn;
    has_changed := !has_changed || merge_trivial_blocks fn
  done

let name = "simplcfg"
let pass_fn = simplify_cfg
