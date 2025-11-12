(*
 * Copyright (c) 2025 Hubert Gruniaux
 * This file is part of the Iris project.
 *)

open Ir

(** Collects all alloca instructions in the entry block of the given function.
    It returns the name of the alloca and its type. *)
let collect_allocas fn =
  let entry_block = Option.get (Function.entry_block fn) in
  Block.fold_insts
    (fun acc inst ->
      match inst with
      | Iinst_def (name, Iexpr_alloca (t, _)) -> (name, t) :: acc
      | _ -> acc)
    [] entry_block

(** Checks if an alloca of the given type [t] can be promoted to a virtual
    register. In particular, we don't want to promote aggregate types or other
    no primitive types. *)
let is_promotable_type t = Type.is_integer t || Type.is_float t

(** Filters the allocas that can be promoted to registers. An alloca can be
    promoted if its type is promotable (see [is_promotable_type]) and if its
    uses are only loads and stores. *)
let filter_promotable_allocas allocas escaped_allocas =
  List.filter
    (fun (alloca_addr, alloca_typ) ->
      is_promotable_type alloca_typ
      && not (RegSet.mem alloca_addr escaped_allocas))
    allocas

(** Traverse the function and finds all uses of the provided allocas. In
    particular, it returns the list of allocas addresses that are used outside
    of a store/load instruction as the address operand (allocas addresses that
    may escape or used indirectly). It also returns a mapping from an alloca
    address and the blocks that contains stores to the address. *)
let find_allocas_uses fn allocas_set =
  let escaped_allocas = ref RegSet.empty in

  let store_blocks = Hashtbl.create 16 in

  Function.iter_blocks
    (fun block ->
      Block.iter_insts
        (fun inst ->
          match inst with
          | Iinst_store (addr, value) -> (
              (* If we store the address of an alloca into another alloca, it escapes...
                 Therefore, we cannot promote it to a register. *)
              (match value with
              | Ival_reg value_reg when RegSet.mem value_reg allocas_set ->
                  escaped_allocas := RegSet.add value_reg !escaped_allocas
              | _ -> ());

              match addr with
              | Ival_reg addr_reg when RegSet.mem addr_reg allocas_set -> (
                  let prev_store_blocks =
                    Hashtbl.find_opt store_blocks addr_reg
                  in
                  match prev_store_blocks with
                  | Some blocks ->
                      Hashtbl.replace store_blocks addr_reg (block :: blocks)
                  | None -> Hashtbl.add store_blocks addr_reg [ block ])
              | _ -> ())
          | Iinst_def (_, Iexpr_load (_typ, _addr)) -> ()
          | _ ->
              let uses = Instruction.uses_reg inst in
              let escaped_allocas_in_inst = RegSet.inter uses allocas_set in
              escaped_allocas :=
                RegSet.union !escaped_allocas escaped_allocas_in_inst)
        block)
    fn;

  (!escaped_allocas, store_blocks)

(** Finds good insertion points for placing phi nodes. *)
let find_insertion_points dom_frontier store_blocks promotable_allocas =
  let insertion_points = Hashtbl.create 16 in
  List.iter
    (fun (alloca_addr, _) ->
      let work = Queue.create () in
      let visited = ref LabelSet.empty in
      List.iter
        (fun b -> Queue.add b work)
        (Hashtbl.find store_blocks alloca_addr);

      while not (Queue.is_empty work) do
        let block = Queue.pop work in
        List.iter
          (fun df_block ->
            let df_block_label = Block.label df_block in
            if not (LabelSet.mem df_block_label !visited) then (
              (match Hashtbl.find_opt insertion_points alloca_addr with
              | Some prev_points ->
                  Hashtbl.replace insertion_points alloca_addr
                    (df_block :: prev_points)
              | None -> Hashtbl.add insertion_points alloca_addr [ df_block ]);
              visited := LabelSet.add df_block_label !visited;
              Queue.add df_block work))
          (dom_frontier block)
      done)
    promotable_allocas;
  insertion_points

let push_rename rename_stack alloca_addr value =
  let stack = Hashtbl.find rename_stack alloca_addr in
  Hashtbl.replace rename_stack alloca_addr (value :: stack)

let pop_rename rename_stack alloca_addr =
  let stack = Hashtbl.find rename_stack alloca_addr in
  match stack with
  | [] -> failwith "pop_rename: empty stack"
  | _ :: rest -> Hashtbl.replace rename_stack alloca_addr rest

let current_rename rename_stack alloca_addr =
  let stack = Hashtbl.find rename_stack alloca_addr in
  match stack with
  | [] -> failwith "current_rename: empty stack"
  | value :: _ -> value

let rec rename_block rename_stack param_for dom_tree allocas_set block =
  (* Before instructions: for block parameters — push each param as current value *)
  RegSet.iter
    (fun alloca_addr ->
      match Hashtbl.find_opt param_for (block.block_label, alloca_addr) with
      | Some param_reg ->
          push_rename rename_stack alloca_addr (Ival_reg param_reg)
      | None -> ())
    allocas_set;

  (* Removes stores and loads and propagates values *)
  Block.filter_map_insts
    (fun inst ->
      match inst with
      | Iinst_store (Ival_reg addr_reg, value)
        when RegSet.mem addr_reg allocas_set ->
          push_rename rename_stack addr_reg value;
          None
      | Iinst_def (name, Iexpr_load (_typ, Ival_reg addr_reg))
        when RegSet.mem addr_reg allocas_set ->
          let current_value = current_rename rename_stack addr_reg in
          Some (Iinst_def (name, Iexpr_value current_value))
      | _ -> Some inst)
    block;

  (* Update terminators: supply correct arguments for each successor *)
  let update_succ_args label old_args =
    (* For a given successor label [label] and its existing arguments [old_args],
       build new args list by appending the current value for each promotable alloca
       **if** that successor block has a parameter for that alloca. *)
    RegSet.fold
      (fun alloca_addr acc ->
        match Hashtbl.find_opt param_for (label, alloca_addr) with
        | Some _ ->
            let cur_val = current_rename rename_stack alloca_addr in
            acc @ [ cur_val ]
        | None -> acc)
      allocas_set old_args
  in

  (* Update terminators *)
  (match block.block_term with
  | Iterm_br (label, args) ->
      let new_args = update_succ_args label args in
      block.block_term <- Iterm_br (label, new_args)
  | Iterm_br_if (cond, true_label, true_args, false_label, false_args) ->
      let new_true_args = update_succ_args true_label true_args in
      let new_false_args = update_succ_args false_label false_args in
      block.block_term <-
        Iterm_br_if
          (cond, true_label, new_true_args, false_label, new_false_args)
  | Iterm_br_table (value, default_label, default_args, cases) ->
      let new_default_args = update_succ_args default_label default_args in
      let new_cases =
        List.map
          (fun (case_value, case_label, case_args) ->
            let new_case_args = update_succ_args case_label case_args in
            (case_value, case_label, new_case_args))
          cases
      in
      block.block_term <-
        Iterm_br_table (value, default_label, new_default_args, new_cases)
  | Iterm_ret _ | Iterm_unreachable -> ());

  (* Recurse on dominated blocks *)
  List.iter
    (fun block ->
      rename_block rename_stack param_for dom_tree allocas_set block)
    (dom_tree block);

  (* After finishing the block: pop any parameters we pushed *)
  RegSet.iter
    (fun alloca_addr ->
      match Hashtbl.find_opt param_for (block.block_label, alloca_addr) with
      | Some _ -> pop_rename rename_stack alloca_addr
      | None -> ())
    allocas_set

(** Removes alloca instructions for promoted allocas in the entry block. *)
let remove_allocas fn allocas_set =
  let entry_block = Option.get (Function.entry_block fn) in
  Block.filter_map_insts
    (fun inst ->
      match inst with
      | Iinst_def (name, Iexpr_alloca (_typ, _)) ->
          if RegSet.mem name allocas_set then None else Some inst
      | _ -> Some inst)
    entry_block

(** The mem2reg pass implementation. *)
let mem2reg fn =
  (* Step 1: Compute dominator tree and dominance frontier *)
  let idom = Cfg.compute_idom fn in
  let dom_tree = Cfg.Dominator.idom_to_dom_tree fn idom in
  let dom_frontier = Cfg.Dominator.compute_dom_frontier fn dom_tree idom in

  (* Step 2: Find good alloca candidates for promotion *)
  let allocas_candidates = collect_allocas fn in
  let allocas_set = RegSet.of_list (List.map fst allocas_candidates) in
  let escaped_allocas, store_blocks = find_allocas_uses fn allocas_set in
  let promotable_allocas =
    filter_promotable_allocas allocas_candidates escaped_allocas
  in

  (* Step 3: Find good insertion points for placing phi nodes (or equivalently block arguments) *)
  let insertion_points =
    find_insertion_points dom_frontier store_blocks promotable_allocas
  in

  (* Add block parameters for promoted allocas. *)
  let param_for = Hashtbl.create 16 in
  List.iter
    (fun (alloca_addr, typ) ->
      let insertion_points =
        Hashtbl.find_opt insertion_points alloca_addr
        |> Option.value ~default:[]
      in
      List.iter
        (fun block ->
          let new_reg = Function.fresh_register fn typ in
          block.block_params <- new_reg :: block.block_params;
          Hashtbl.add param_for (block.block_label, alloca_addr) new_reg)
        insertion_points)
    promotable_allocas;

  (* Step 4: Rename variables and insert phi nodes (we use block arguments) *)
  let entry_block = Option.get (Function.entry_block fn) in
  let rename_stack = Hashtbl.create 16 in
  List.iter
    (fun (alloca_addr, typ) ->
      let reg = Function.fresh_register fn typ in
      Hashtbl.add rename_stack alloca_addr [ Ival_reg reg ])
    promotable_allocas;

  rename_block rename_stack param_for dom_tree allocas_set entry_block;

  (* Step 5: Remove alloca instructions for promoted allocas *)
  remove_allocas fn allocas_set

let name = "mem2reg"
let pass_fn = mem2reg
