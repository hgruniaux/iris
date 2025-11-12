(*
 * Copyright (c) 2025 Hubert Gruniaux
 * This file is part of the Iris project.
 *)

open Ir

let name = "copy-propagation"

let map_value mapping v =
  match v with
  | Ival_reg r -> (
      match Hashtbl.find_opt mapping r with Some new_v -> new_v | None -> v)
  | _ -> v

let rec traverse_block mapping dom_tree block =
  Block.filter_map_insts
    (fun inst ->
      match inst with
      | Iinst_def (name, Iexpr_value new_value) ->
          Format.printf "Propagating copy of %a\n" Ir_printer.pp_value new_value;
          Hashtbl.add mapping name (map_value mapping new_value);
          None
      | _ -> Some (Instruction.map_values (map_value mapping) inst))
    block;

  block.block_term <- Terminator.map_values (map_value mapping) block.block_term;

  List.iter (traverse_block mapping dom_tree) (dom_tree block)

let pass_fn fn =
  Printf.printf "Running copy propagation on function %s\n" fn.fn_name;
  let idom = Cfg.compute_idom fn in
  let dom_tree = Cfg.Dominator.idom_to_dom_tree fn idom in

  let entry_block = Option.get (Function.entry_block fn) in
  let mapping = Hashtbl.create 16 in
  traverse_block mapping dom_tree entry_block
