(* Copyright (C) 2025 Hubert Gruniaux
 * This file is part of Iris. *)

open Codegen_common
open Tast
open LibIris

val codegen_expr : codegen_context -> texpr -> Ir.Value.t
(** Generate IR code for a given expression, and returns the corresponding IR
    value. *)
