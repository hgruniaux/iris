(*
 * Copyright (c) 2025 Hubert Gruniaux
 * This file is part of the Iris project.
 *)

open Ir

let remove_unreachable_blocks _ = ()
let merge_leader_list _ = ()

let simplify_cfg fn =
  remove_unreachable_blocks fn;
  merge_leader_list fn
