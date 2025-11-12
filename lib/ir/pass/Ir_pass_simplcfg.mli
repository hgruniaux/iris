(*
 * Copyright (c) 2025 Hubert Gruniaux
 * This file is part of the Iris project.
 *
 * This file contains the implementation of the CFG simplication pass on the IR.
 * It removes unreachable basic blocks and merges those that can.
 *)

val remove_unreachable_blocks : Ir_base.fn -> bool
(** Remove unreachable basic blocks from the function's control flow graph.
    Returns [true] if at least one unreachable basic block was removed. *)

val name : string
(** The pass name, for debugging and identification purposes. *)

val pass_fn : Ir_base.fn -> unit
(** The function pass implementing the CFG simplification. *)
