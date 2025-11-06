(*
 * Copyright (c) 2025 Hubert Gruniaux
 * This file is part of the Iris project.
 *
 * This file contains the implementation of the CFG simplication pass on the IR.
 * It removes unreachable basic blocks and merges those that can.
 *)

val remove_unreachable_blocks : Ir.fn -> unit
(** Removes all basic blocks that are unreachable on the provided function. It
    does this by performing a reachability analysis. *)

val merge_leader_list : Ir.fn -> unit
val simplify_cfg : Ir.fn -> unit
