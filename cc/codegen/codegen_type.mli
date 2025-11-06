(* Copyright (C) 2025 Hubert Gruniaux
 * This file is part of Iris. *)

open Tast
open LibIris

val codegen_type : ttype -> Ir.Type.t
(** Returns the corresponding IR type for a given type. *)
