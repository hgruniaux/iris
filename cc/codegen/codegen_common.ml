(* Copyright (C) 2025 Hubert Gruniaux
 * This file is part of Iris. *)

open LibIris
open Tast

type loop_labels = { break_bb : Ir.Block.t; continue_bb : Ir.Block.t }

type codegen_context = {
  ctx_module : Ir.ctx;
  mutable ctx_builder : Ir.Builder.t;
  ctx_values : (tvalue_decl, Ir.Value.t) Hashtbl.t;
  ctx_labels : (tlabel_declaration, Ir.Block.t) Hashtbl.t;
  mutable ctx_loop_labels : loop_labels list;
  ctx_type_cache : (ttype, Ir.typ) Hashtbl.t;
}

let push_loop_labels ctx break_bb continue_bb =
  let labels = { break_bb; continue_bb } in
  ctx.ctx_loop_labels <- labels :: ctx.ctx_loop_labels

let pop_loop_labels ctx =
  match ctx.ctx_loop_labels with
  | [] -> failwith "pop_loop_labels: No loop labels to pop"
  | _ :: rest -> ctx.ctx_loop_labels <- rest

let fresh_block ctx name =
  Ir.Builder.fresh_block ctx.ctx_builder ~name:(Some name)
