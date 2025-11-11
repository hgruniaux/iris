(*
 * Copyright (c) 2025 Hubert Gruniaux
 * This file is part of the Iris project.
 *
 * Implementation of the Mem2Reg optimization pass. It transforms alloca variables
 * into SSA registers.
 *)

val name : string
(** The pass name, for debugging and identification purposes. *)

val pass_fn : Ir.fn -> unit
(** Runs the Mem2Reg optimization pass on the given function. The function is
    modified in place. *)
