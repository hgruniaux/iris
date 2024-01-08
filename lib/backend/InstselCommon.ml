open Ir

type calling_convention_info = {
  cc_caller_saved : Reg.set;
  cc_callee_saved : Reg.set;
  cc_args_regs : Reg.t list;
  cc_args_regs_count : int;
  cc_return_reg : Reg.t option;
}

let get_reg op =
  match op with Iop_reg r -> r | _ -> failwith "not a register operand"

(** Returns the first [k] elements of [xs] and the remaining elements.
    If [xs] is too small, then [xs] is returned. *)
let rec firstk k xs =
  match xs with
  | [] -> ([], [])
  | x :: xs ->
      if k = 0 then ([], x :: xs)
      else if k = 1 then ([ x ], xs)
      else
        let f, r = firstk (k - 1) xs in
        (x :: f, r)

(** Same as List.map2 but stop when l1 is empty (therefore both lists may
    have a different length). *)
let rec map2 f l1 l2 =
  match (l1, l2) with
  | [], _ -> []
  | _, [] -> failwith "not enough elements in l2"
  | x1 :: r1, x2 :: r2 -> f x1 x2 :: map2 f r1 r2

(** Generates a sequence of mov and pop instructions (created using [mk_mov] and [mk_pop])
    that retrieve the concrete arguments of the given [ir_fn] (according to [cc_info])
    and stores them in the IR arguments pseudo registers. Notably, this function
    explicify the calling convention. *)
let generate_callee_args cc_info ir_fn mk_mov mk_pop =
  let args_in_regs, args_in_stack =
    firstk cc_info.cc_args_regs_count ir_fn.Ir.fn_params
  in

  (* Moves the arguments lying in registers to their IR pseudo register. *)
  let mov_insts =
    map2 (fun arg reg -> mk_mov arg reg) args_in_regs cc_info.cc_args_regs
  in

  (* Pop from stack the remaining arguments, from right to left. *)
  let mov_and_pop_insts =
    List.fold_right
      (fun reg insts -> mk_pop reg @ insts)
      args_in_stack mov_insts
  in

  mov_and_pop_insts

let generate_caller_args cc_info args mk_mov mk_push =
  let args_in_regs, args_in_stack = firstk cc_info.cc_args_regs_count args in

  let mov_insts =
    map2
      (fun arg reg ->
        let mov_inst = mk_mov reg (get_reg arg) in
        mov_inst)
      args_in_regs cc_info.cc_args_regs
  in

  (* Push the remaining arguments to stack, from right to left. *)
  let mov_and_push_insts =
    List.fold_right
      (fun reg insts -> mk_push (get_reg reg) :: insts)
      args_in_stack mov_insts
  in

  mov_and_push_insts

let instsel_bb bb instsel_inst =
  let mir_insts =
    (* FIXME: mir_insts @ ... is SLOW, we should use fold_right *)
    Ir.fold_insts (fun mir_insts inst -> mir_insts @ instsel_inst inst) [] bb
  in
  {
    Mir.bb_label = bb.b_label;
    Mir.bb_insts = mir_insts;
    Mir.bb_predecessors = bb.b_predecessors;
    Mir.bb_successors = bb.b_successors;
  }

let instsel_fn fn instsel_bb =
  let mir_blocks = Label.Map.map instsel_bb fn.fn_blocks in
  {
    Mir.fn_name = fn.fn_name;
    Mir.fn_params = fn.fn_params;
    Mir.fn_blocks = mir_blocks;
    Mir.fn_entry = Option.get fn.fn_entry;
  }
