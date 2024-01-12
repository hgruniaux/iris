(* This module contains the algorithms to compute the liveness information of
 * registers in a MIR function declaration/basic blocks. Because the MIR
 * representation stores uses and defs sets, the Liveness analysis is target
 * independent.

 * These functions are mainly used by the Interference module to build the
 * interference graph later used for register allocation.

 * For performance reasons, we compute the liveness information at each
 * basic block boundaries using the Kildall algorithm. Inside basic blocks,
 * liveness information can be quickly determined from the boundary information
 * as each instruction only has a single successor and predecessor. *)

open Mir

type inst_liveinfo = { inst_live_in : Reg.set; inst_live_out : Reg.set }

type bb_lifeinfo = {
  mutable bb_live_in : Reg.set;
  mutable bb_live_out : Reg.set;
  mutable bb_inst_liveinfo : (inst, inst_liveinfo) Hashtbl.t;
}

type t = (Mir.bb, bb_lifeinfo) Hashtbl.t

(** Returns the variables alive at input of the basic block [bb]
     given the variables alive [live_out] at output. *)
let live_in_of_bb bb live_out =
  let insts_liveinfo = Hashtbl.create 17 in
  let rec loop live_in live_out insts =
    match insts with
    | [] -> live_in
    | inst :: remaining ->
        let live_out = Reg.Set.union live_in live_out in
        let def, use = (inst.i_defs, inst.i_uses) in
        let live_in = Reg.Set.union use (Reg.Set.diff live_out def) in
        let inst_liveinfo =
          { inst_live_in = live_in; inst_live_out = live_out }
        in
        Hashtbl.add insts_liveinfo inst inst_liveinfo;
        loop live_in live_out remaining
  in
  let live_in = loop Reg.Set.empty live_out (List.rev bb.bb_insts) in
  (live_in, insts_liveinfo)

(** Computes liveness info for the given function definition [funcdef]. *)
let compute fn =
  (* We implement the Kiskall algorithm: we use a worklist to iteratively
     approximate the data flow equations. Moreover, we consider basic blocks
     as the base unit for the algorithm. *)
  let worklist = Queue.create () in
  let liveinfo = Hashtbl.create 17 in

  let liveinfo_of bb =
    match Hashtbl.find_opt liveinfo bb with
    | Some bb -> bb
    | None ->
        let bb_liveinfo =
          {
            bb_live_in = Reg.Set.empty;
            bb_live_out = Reg.Set.empty;
            bb_inst_liveinfo = Hashtbl.create 17;
          }
        in
        Hashtbl.add liveinfo bb bb_liveinfo;
        bb_liveinfo
  in

  (* Populate the worklist by adding basic blocks from the exit points to the entry bb. *)
  let visited_bb_labels = ref Label.Set.empty in
  let rec dfs bb =
    if not (Label.Set.mem bb.bb_label !visited_bb_labels) then (
      visited_bb_labels := Label.Set.add bb.bb_label !visited_bb_labels;
      Label.Set.iter
        (fun label ->
          let bb = Label.Map.find label fn.fn_blocks in
          dfs bb)
        bb.bb_successors;
      Queue.add bb.bb_label worklist)
  in
  let entry_bb = Label.Map.find fn.fn_entry fn.fn_blocks in
  dfs entry_bb;

  (* We expected that there is no more trivially unreachable
     basic blocks (with no predecessors). *)

  (* Loop until worklist is not empty. *)
  while not (Queue.is_empty worklist) do
    let bb_label = Queue.take worklist in
    let bb = Label.Map.find bb_label fn.fn_blocks in

    let bb_liveinfo = liveinfo_of bb in
    let old_live_in = bb_liveinfo.bb_live_in in

    (* Computes live_out variables of bb as the union of live_in of
       each of its successors. *)
    let live_out =
      Label.Set.fold
        (fun label succs ->
          let bb = Label.Map.find label fn.fn_blocks in
          Reg.Set.union succs (liveinfo_of bb).bb_live_in)
        bb.bb_successors Reg.Set.empty
    in

    (* Then compute basic block's live_in and per instruction liveness info. *)
    let live_in, inst_liveinfo = live_in_of_bb bb live_out in

    (* Update liveness info of bb. *)
    bb_liveinfo.bb_live_out <- live_out;
    bb_liveinfo.bb_live_in <- live_in;
    bb_liveinfo.bb_inst_liveinfo <- inst_liveinfo;

    (* If live_in changed, then we need to update all the predecessors. *)
    if not (Reg.Set.equal live_in old_live_in) then
      (* Add predecessors of bb to worklist. *)
      Label.Set.iter (fun l -> Queue.add l worklist) bb.bb_predecessors
  done;

  liveinfo

let dump_liveinfo fn liveinfo =
  let pp_registerset = IrPP.pp_registerset in
  let cur_bb_liveinfo = ref None in
  let pp_inst_extra ppf inst =
    let inst_liveinfo =
      Hashtbl.find (Option.get !cur_bb_liveinfo).bb_inst_liveinfo inst
    in
    Format.fprintf ppf "; in = {%a}, out = {%a}" pp_registerset
      inst_liveinfo.inst_live_in pp_registerset inst_liveinfo.inst_live_out
  in
  let pp_bb_extra ppf bb =
    let bb_liveinfo = Hashtbl.find liveinfo bb in
    cur_bb_liveinfo := Some bb_liveinfo;
    Format.fprintf ppf "; in = {%a}, out = {%a}" pp_registerset
      bb_liveinfo.bb_live_in pp_registerset bb_liveinfo.bb_live_out
  in
  Format.printf "%a@." (Mir.pp_fn pp_bb_extra pp_inst_extra) fn
