(*
 * Copyright (c) 2025 Hubert Gruniaux
 * This file is part of the Iris project.
 *)

open Ir

(** Inline functions whose instruction count is below this threshold. *)
let inline_threshold = 15

type inline_candidate = {
  inl_call_block : bb;
  inl_call_name : reg;
  inl_call_inst_index : int;
  inl_called_fn : fn;
  inl_called_args : value list;
}

(** Check if a function should be inlined. Multiple heuristics may be used. In
    all cases, the function must have a definition and not be external to be
    inlined. *)
let should_inline parent_fn called_fn =
  if called_fn.fn_is_external then false
  else if called_fn.fn_blocks = LabelMap.empty || called_fn.fn_entry = None then
    false
  else if parent_fn == called_fn then false (* avoid recursive calls *)
  else
    let inst_count =
      Function.fold_blocks
        (fun bb acc -> acc + List.length bb.block_insts)
        called_fn 0
    in
    inst_count <= inline_threshold

(** Prepare [called_fn] to be inlined in [parent_fn] with the provided [args].
    Returns the entry basic block of [parent_fn] that contains the cloned
    [called_fn] entry basic block, and an exit block that merges all returns
    from the [called_fn]. It also returns the virtual register in [parent_fn]
    that contains the return value of [called_fn] (that can be used in the
    returned exit block and its dominated blocks). *)
let prepare_inline_call parent_fn called_fn args =
  (* Map old registers and labels to new ones. *)
  let reg_mapping = Hashtbl.create 16 in
  let label_mapping = Hashtbl.create 16 in

  List.iter2
    (fun param_reg arg -> Hashtbl.add reg_mapping param_reg arg)
    called_fn.fn_params args;

  let map_only_reg r =
    match Hashtbl.find_opt reg_mapping r with
    | Some (Ival_reg new_r) -> new_r
    | Some _ -> failwith "Cannot map non-register value to register"
    | None ->
        let new_r = Function.fresh_register parent_fn (Reg.type_of r) in
        (match Reg.name r with
        | Some name -> Reg.set_name new_r name
        | None -> ());
        Hashtbl.add reg_mapping r (Ival_reg new_r);
        new_r
  in

  let map_reg r =
    match Hashtbl.find_opt reg_mapping r with
    | Some v -> v
    | None -> Ival_reg (map_only_reg r)
  in

  let map_value v = match v with Ival_reg r -> map_reg r | _ -> v in

  let map_label l = Hashtbl.find label_mapping l in

  (* Copy blocks, remapping registers and labels to fresh ones from the called function to the parent function. *)
  let ret_count = ref 0 in
  let new_blocks =
    Function.map_blocks
      (fun old_bb ->
        let params = List.map map_only_reg (Block.params old_bb) in
        let new_bb = Block.create ~params parent_fn in
        (match Label.name old_bb.block_label with
        | Some name ->
            let new_name = name ^ ".inl" in
            Label.set_name new_bb.block_label new_name
        | None -> ());

        new_bb.block_insts <-
          List.map
            (Instruction.map_values_and_def map_value map_only_reg)
            old_bb.block_insts;
        (* Keep old terminator, not updated, for now as we don't have a full label mapping (we
           are created it). We will update all terminators at once later with all the new information. *)
        new_bb.block_term <- old_bb.block_term;
        if Terminator.is_ret old_bb.block_term then incr ret_count;
        Hashtbl.add label_mapping old_bb.block_label new_bb.block_label;
        new_bb)
      called_fn
  in

  (* Create a new exit block that will serve as the target for all return branches.
     If we have more than once return and the inlined function returns a value, then
     we need to add a block argument to preserve SSA form. *)
  let exit_block = Block.create parent_fn in
  Label.set_name exit_block.block_label (called_fn.fn_name ^ ".exit");
  let exit_value = ref None in
  if !ret_count > 1 then (
    match Function.return_type_of called_fn with
    | Ityp_unit -> ()
    | ret_type ->
        let ret_reg = Function.fresh_register parent_fn ret_type in
        exit_value := Some (Ival_reg ret_reg);
        exit_block.block_params <- [ ret_reg ])
  else ();

  (* Now that we have a full mapping of labels, update terminators to use new labels.
     We also convert ret terminators to branches to a block in parent_fn. *)
  List.iter
    (fun bb ->
      let new_term =
        match bb.block_term with
        | Iterm_ret None -> Iterm_br (exit_block.block_label, [])
        | Iterm_ret (Some value) ->
            let mapped_value = map_value value in
            if !ret_count > 1 then
              Iterm_br (exit_block.block_label, [ mapped_value ])
            else (
              exit_value := Some mapped_value;
              Iterm_br (exit_block.block_label, []))
        | term -> Terminator.map_values_and_labels map_value map_label term
      in
      (* Temporarily set to unreachable to avoid issues during updates as
         the previous terminator refers to old labels of another function. *)
      bb.block_term <- Iterm_unreachable;
      bb.block_succ <- LabelSet.empty;
      bb.block_pred <- LabelSet.empty;
      Block.set_term parent_fn bb new_term)
    new_blocks;

  let old_entry_label = Option.get called_fn.fn_entry in
  let new_entry_label = Hashtbl.find label_mapping old_entry_label in
  let entry_block = LabelMap.find new_entry_label parent_fn.fn_blocks in

  (entry_block, exit_block, !exit_value)

(** Inlines a call instruction within a block. *)
let inline_call_in_block parent_fn block call_inst_index call_inst_name
    called_fn args =
  assert (call_inst_index < List.length block.block_insts);

  let prev_insts = List.take call_inst_index block.block_insts in
  let after_insts = List.drop (call_inst_index + 1) block.block_insts in

  let entry_block, exit_block, exit_value =
    prepare_inline_call parent_fn called_fn args
  in

  block.block_insts <- prev_insts;
  let term = block.block_term in
  Block.set_term parent_fn block (Iterm_br (entry_block.block_label, []));

  exit_block.block_insts <-
    (match exit_value with
    | Some value -> Iinst_def (call_inst_name, Iexpr_value value) :: after_insts
    | None -> after_insts);

  Block.set_term parent_fn exit_block term

(** Collect all direct call instructions that may be possible candidates for
    inlining in the provided function [fn]. *)
let collect_candidates fn =
  Function.fold_blocks
    (fun block candidates ->
      let i = ref 0 in
      Block.fold_insts
        (fun candidates inst ->
          let new_candidates =
            match inst with
            | Iinst_def (ret_reg, Iexpr_call (Ival_global global, args)) -> (
                match global.global_kind with
                | Iglobal_function called_fn ->
                    {
                      inl_call_block = block;
                      inl_call_name = ret_reg;
                      inl_call_inst_index = !i;
                      inl_called_fn = called_fn;
                      inl_called_args = args;
                    }
                    :: candidates
                | _ -> candidates)
            | _ -> candidates
          in
          incr i;
          new_candidates)
        candidates block)
    fn []

(** Among the inlining candidates, select which ones to actually inline. *)
let select_candidates parent_fn candidates =
  List.filter
    (fun candidate -> should_inline parent_fn candidate.inl_called_fn)
    candidates

(** Traverse [fn] and inline profitable calls. If at least one call was inlined,
    returns true. *)
let inline_calls fn =
  let candidates = collect_candidates fn in
  let calls_to_inline =
    (* We sort calls in the same block in descending order of their instruction index,
       so we apply inline at end of basic blocks after the ones at the start. *)
    List.fast_sort
      (fun c1 c2 ->
        if Block.equal c1.inl_call_block c2.inl_call_block then
          c2.inl_call_inst_index - c1.inl_call_inst_index
        else Block.compare c1.inl_call_block c2.inl_call_block)
      (select_candidates fn candidates)
  in

  List.iter
    (fun c ->
      inline_call_in_block fn c.inl_call_block c.inl_call_inst_index
        c.inl_call_name c.inl_called_fn c.inl_called_args)
    calls_to_inline;

  not (List.is_empty calls_to_inline)

let name = "inline"

let pass_fn fn =
  let has_inlined = ref true in
  while !has_inlined do
    has_inlined := inline_calls fn
  done
