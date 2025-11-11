open Ir_base
module Function = Ir_function
module Terminator = Ir_terminator

type t = bb

let compare a b = Stdlib.compare a.block_label b.block_label
let equal a b = a.block_label = b.block_label
let hash fn = Hashtbl.hash fn.block_label
let label bb = bb.block_label
let params bb = bb.block_params
let term bb = bb.block_term
let pred bb = bb.block_pred
let succ bb = bb.block_succ

(** Creates a new basic block and adds it to [fn]. *)
let create ?(params = []) fn =
  let label = Function.fresh_label fn in
  let bb =
    {
      block_label = label;
      block_params = params;
      block_insts = [];
      block_term = Iterm_unreachable;
      block_pred = LabelSet.empty;
      block_succ = LabelSet.empty;
    }
  in
  fn.fn_blocks <- LabelMap.add label bb fn.fn_blocks;
  bb

let iter_insts f bb = List.iter f bb.block_insts
let fold_insts f init bb = List.fold_left f init bb.block_insts
let filter_map_insts f bb = bb.block_insts <- List.filter_map f bb.block_insts

(** Checks whether [bb] belongs to [fn]. *)
let has_preds bb = not (LabelSet.is_empty bb.block_pred)

(** Return the predecessor of [bb] if it has a single predecessor. *)
let single_predecessor bb =
  if LabelSet.cardinal bb.block_pred = 1 then
    Some (LabelSet.choose bb.block_pred)
  else None

(** Return the successor of [bb] if it has a single successor. *)
let single_successor bb =
  if LabelSet.cardinal bb.block_succ = 1 then
    Some (LabelSet.choose bb.block_succ)
  else None

(** Sets the [bb]'s terminator to the given [term]. *)
let set_term fn bb term =
  assert (is_bb_from bb fn);
  bb.block_term <- term;

  (* Update successors. *)

  (* First we remove [bb] from its successors' predecessors list. *)
  LabelSet.iter
    (fun succ_label ->
      let succ = LabelMap.find succ_label fn.fn_blocks in
      succ.block_pred <- LabelSet.remove bb.block_label succ.block_pred)
    bb.block_succ;

  (* Then we add [bb] to its new successors. *)
  bb.block_succ <- Terminator.succs term;
  LabelSet.iter
    (fun succ_label ->
      let succ = LabelMap.find succ_label fn.fn_blocks in
      succ.block_pred <- LabelSet.add bb.block_label succ.block_pred)
    bb.block_succ
