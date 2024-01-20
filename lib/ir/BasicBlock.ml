open Ir

type t = Ir.bb

let compare a b = Stdlib.compare a.Ir.b_label b.Ir.b_label
let equal a b = a.Ir.b_label = b.Ir.b_label
let hash fn = Hashtbl.hash fn.Ir.b_label

(** Creates a new basic block and adds it to [fn]. *)
let create fn =
  let label = Label.fresh () in
  let bb =
    {
      b_label = label;
      b_phi_insts = [];
      b_insts = [];
      b_term = Iterm_unreachable;
      b_predecessors = Label.Set.empty;
      b_successors = Label.Set.empty;
    }
  in
  fn.fn_blocks <- Label.Map.add label bb fn.fn_blocks;
  bb

let iter_phis f bb = List.iter f bb.b_phi_insts
let iter_insts f bb = List.iter f bb.b_insts

(** Return the predecessor of [bb] if it has a single predecessor. *)
let single_predecessor bb =
  if Ir.Label.Set.cardinal bb.b_predecessors = 1 then
    Some (Ir.Label.Set.choose bb.b_predecessors)
  else None

(** Return the successor of [bb] if it has a single successor. *)
let single_successor bb =
  if Ir.Label.Set.cardinal bb.b_successors = 1 then
    Some (Ir.Label.Set.choose bb.b_successors)
  else None

(** Sets the [bb]'s terminator to the given [term]. *)
let set_term fn bb term =
  assert (is_bb_from bb fn);
  bb.b_term <- term;

  (* Update successors. *)

  (* First we remove [bb] from its successors' predecessors list. *)
  Label.Set.iter
    (fun succ_label ->
      let succ = Label.Map.find succ_label fn.fn_blocks in
      succ.b_predecessors <- Label.Set.remove bb.b_label succ.b_predecessors)
    bb.b_successors;

  (* Then we add [bb] to its new successors. *)
  bb.b_successors <- Terminator.successors_of term;
  Label.Set.iter
    (fun succ_label ->
      let succ = Label.Map.find succ_label fn.fn_blocks in
      succ.b_predecessors <- Label.Set.add bb.b_label succ.b_predecessors)
    bb.b_successors

(** Verify if [bb] is well-formed (do assert checks).
    Has no effect in release builds. *)
let verify bb = ignore bb
