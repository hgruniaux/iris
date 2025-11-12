(*
 * Copyright (c) 2025 Hubert Gruniaux
 * This file is part of the Iris project.
 *
 * Implementation of the inlining optimization pass. It inlines function calls
 * where appropriate.
 *)

val name : string
(** The pass name, for debugging and identification purposes. *)

val pass_fn : Ir.fn -> unit
(** Runs the inlining optimization pass on the given function. The function is
    modified in place. *)
