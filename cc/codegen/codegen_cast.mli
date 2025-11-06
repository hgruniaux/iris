(* Copyright (C) 2025 Hubert Gruniaux
 * This file is part of Iris. *)

open Codegen_common
open Tast
open LibIris

val codegen_cast :
  codegen_context -> cast_kind -> ttype -> ttype -> Ir.Value.t -> Ir.Value.t
(** [codegen_cast ctx kind src_ty dest_ty value]

    Generate IR code for a given cast. The returned value is the IR
    representation of the casted expression. It is the responsibility of the
    caller to ensure that the cast is valid. *)
