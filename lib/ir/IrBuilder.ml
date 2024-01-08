open Ir

type t = {
  ctx : ctx;
  mutable cur_bb : bb option;
  mutable cur_func_decl : fn option;
  (* Used to implement break and continue statements. *)
  break_labels : Ir.label Stack.t;
  continue_labels : Ir.label Stack.t;
}

let create ctx =
  {
    ctx = ctx;
    cur_bb = None;
    cur_func_decl = None;
    break_labels = Stack.create ();
    continue_labels = Stack.create ();
  }

let create_for_fn fn =
  {
    ctx = fn.fn_ctx;
    cur_bb = None;
    cur_func_decl = Some fn;
    break_labels = Stack.create ();
    continue_labels = Stack.create ();
  }

(** Adds the instruction [inst] that writes to [reg] into the current
    basic block of the given IR Builder [ib]. *)
let add_inst ib reg inst_kind =
  let bb = Option.get ib.cur_bb in
  let inst = Ir.mk_inst bb reg inst_kind in
  (match bb.b_last_inst with
  | None ->
      bb.b_first_inst <- Some inst;
      bb.b_last_inst <- Some inst
  | Some last_inst -> insert_after last_inst inst);
  reg

let find_inst_kind ib reg =
  let fn = Option.get ib.cur_func_decl in
  Option.map (fun inst -> inst.i_kind) (Hashtbl.find_opt fn.fn_symbol_table reg)

let mk_const ib c =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_cst c)

let mk_mov ib r1 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_mov r1)

let mk_load ib r1 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_load r1)

let mk_store ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_store (r1, Iop_reg r2))

let mk_neg ib r1 =
  let rout = Reg.fresh () in
  let e1 = find_inst_kind ib r1 in
  match e1 with
  (* constant folding *)
  | Some (Iinst_cst a) -> add_inst ib rout (Iinst_cst (Nativeint.neg a))
  (* general case *)
  | _ -> add_inst ib rout (Iinst_unop (Iunop_neg, Iop_reg r1))

let mk_add ib r1 r2 =
  let rout = Reg.fresh () in
  let e1 = find_inst_kind ib r1 in
  let e2 = find_inst_kind ib r2 in
  match (e1, e2) with
  (* constant folding *)
  | Some (Iinst_cst a), Some (Iinst_cst b) ->
      add_inst ib rout (Iinst_cst (Nativeint.add a b))
  (* x + 0 = 0 + x = x *)
  | Some (Iinst_cst 0n), Some e | Some e, Some (Iinst_cst 0n) ->
      add_inst ib rout e
  (* general case *)
  | _ -> add_inst ib rout (Iinst_binop (Ibinop_add, Iop_reg r1, Iop_reg r2))

let mk_sub ib r1 r2 =
  let rout = Reg.fresh () in
  let e1 = find_inst_kind ib r1 in
  let e2 = find_inst_kind ib r2 in
  match (e1, e2) with
  (* constant folding *)
  | Some (Iinst_cst a), Some (Iinst_cst b) ->
      add_inst ib rout (Iinst_cst (Nativeint.sub a b))
  (* x - 0 = x *)
  | _, Some (Iinst_cst 0n) -> r1
  (* 0 - x = -x *)
  | Some (Iinst_cst 0n), _ ->
      add_inst ib rout (Iinst_unop (Iunop_neg, Iop_reg r2))
  (* general case *)
  | _ -> add_inst ib rout (Iinst_binop (Ibinop_sub, Iop_reg r1, Iop_reg r2))

let mk_mul ib r1 r2 =
  let rout = Reg.fresh () in
  let e1 = find_inst_kind ib r1 in
  let e2 = find_inst_kind ib r2 in
  match (e1, e2) with
  (* constant folding *)
  | Some (Iinst_cst a), Some (Iinst_cst b) ->
      add_inst ib rout (Iinst_cst (Nativeint.mul a b))
  (* x * 0 = 0 * x = 0 *)
  | Some (Iinst_cst 0n), _ | _, Some (Iinst_cst 0n) ->
      add_inst ib rout (Iinst_cst 0n)
  (* x * 1 = 1 * x = x *)
  | Some (Iinst_cst 1n), _ -> r2
  | _, Some (Iinst_cst 1n) -> r1
  (* general case *)
  | _ -> add_inst ib rout (Iinst_binop (Ibinop_mul, Iop_reg r1, Iop_reg r2))

let mk_sdiv ib r1 r2 =
  let rout = Reg.fresh () in
  let e1 = find_inst_kind ib r1 in
  let e2 = find_inst_kind ib r2 in
  match (e1, e2) with
  (* special case for division by zero, we do not optimize it *)
  | _, Some (Iinst_cst 0n) ->
      add_inst ib rout (Iinst_binop (Ibinop_udiv, Iop_reg r1, Iop_reg r2))
  (* constant folding *)
  | Some (Iinst_cst a), Some (Iinst_cst b) ->
      add_inst ib rout (Iinst_cst (Nativeint.unsigned_div a b))
  (* x / x = 1 *)
  | _, _ when r1 = r2 -> add_inst ib rout (Iinst_cst 1n)
  (* 0 / x = 0 *)
  | Some (Iinst_cst 0n), _ -> add_inst ib rout (Iinst_cst 0n)
  (* x / 1 = x *)
  | _, Some (Iinst_cst 1n) -> r1
  (* general case *)
  | _ -> add_inst ib rout (Iinst_binop (Ibinop_udiv, Iop_reg r1, Iop_reg r2))

let mk_sdiv ib r1 r2 =
  let rout = Reg.fresh () in
  let e1 = find_inst_kind ib r1 in
  let e2 = find_inst_kind ib r2 in
  match (e1, e2) with
  (* special case for division by zero, we do not optimize it *)
  | _, Some (Iinst_cst 0n) ->
      add_inst ib rout (Iinst_binop (Ibinop_sdiv, Iop_reg r1, Iop_reg r2))
  (* constant folding *)
  | Some (Iinst_cst a), Some (Iinst_cst b) ->
      add_inst ib rout (Iinst_cst (Nativeint.div a b))
  (* x / x = 1 *)
  | _, _ when r1 = r2 -> add_inst ib rout (Iinst_cst 1n)
  (* 0 / x = 0 *)
  | Some (Iinst_cst 0n), _ -> add_inst ib rout (Iinst_cst 0n)
  (* x / 1 = x *)
  | _, Some (Iinst_cst 1n) -> r1
  (* general case *)
  | _ -> add_inst ib rout (Iinst_binop (Ibinop_sdiv, Iop_reg r1, Iop_reg r2))

let mk_urem ib r1 r2 =
  let rout = Reg.fresh () in
  let e1 = find_inst_kind ib r1 in
  let e2 = find_inst_kind ib r2 in
  match (e1, e2) with
  (* special case for division by zero, we do not optimize it *)
  | _, Some (Iinst_cst 0n) ->
      add_inst ib rout (Iinst_binop (Ibinop_urem, Iop_reg r1, Iop_reg r2))
  (* constant folding *)
  | Some (Iinst_cst a), Some (Iinst_cst b) ->
      add_inst ib rout (Iinst_cst (Nativeint.unsigned_rem a b))
  (* x mod 1 = 0 *)
  | _, Some (Iinst_cst 1n) -> add_inst ib rout (Iinst_cst 0n)
  (* general case *)
  | _ -> add_inst ib rout (Iinst_binop (Ibinop_urem, Iop_reg r1, Iop_reg r2))

let mk_srem ib r1 r2 =
  let rout = Reg.fresh () in
  let e1 = find_inst_kind ib r1 in
  let e2 = find_inst_kind ib r2 in
  match (e1, e2) with
  (* special case for division by zero, we do not optimize it *)
  | _, Some (Iinst_cst 0n) ->
      add_inst ib rout (Iinst_binop (Ibinop_srem, Iop_reg r1, Iop_reg r2))
  (* constant folding *)
  | Some (Iinst_cst a), Some (Iinst_cst b) ->
      add_inst ib rout (Iinst_cst (Nativeint.rem a b))
  (* x mod 1 = 0 *)
  | _, Some (Iinst_cst 1n) -> add_inst ib rout (Iinst_cst 0n)
  (* general case *)
  | _ -> add_inst ib rout (Iinst_binop (Ibinop_srem, Iop_reg r1, Iop_reg r2))

let mk_asr ib r1 r2 =
  let rout = Reg.fresh () in
  let e1 = find_inst_kind ib r1 in
  let e2 = find_inst_kind ib r2 in
  match (e1, e2) with
  (* constant folding *)
  | Some (Iinst_cst a), Some (Iinst_cst b) when b >= 0n && b <= 32n ->
      add_inst ib rout
        (Iinst_cst (Nativeint.shift_right a (Nativeint.to_int b)))
  (* x >> 0 = x *)
  | _, Some (Iinst_cst 0n) -> r1
  (* general case *)
  | _ -> add_inst ib rout (Iinst_binop (Ibinop_asr, Iop_reg r1, Iop_reg r2))

let mk_lsr ib r1 r2 =
  let rout = Reg.fresh () in
  let e1 = find_inst_kind ib r1 in
  let e2 = find_inst_kind ib r2 in
  match (e1, e2) with
  (* constant folding *)
  | Some (Iinst_cst a), Some (Iinst_cst b) when b >= 0n && b <= 32n ->
      add_inst ib rout
        (Iinst_cst (Nativeint.shift_right_logical a (Nativeint.to_int b)))
  (* x >> 0 = x *)
  | _, Some (Iinst_cst 0n) -> r1
  (* general case *)
  | _ -> add_inst ib rout (Iinst_binop (Ibinop_lsr, Iop_reg r1, Iop_reg r2))

let mk_lsl ib r1 r2 =
  let rout = Reg.fresh () in
  let e1 = find_inst_kind ib r1 in
  let e2 = find_inst_kind ib r2 in
  match (e1, e2) with
  (* constant folding *)
  | Some (Iinst_cst a), Some (Iinst_cst b) when b >= 0n && b <= 32n ->
      add_inst ib rout (Iinst_cst (Nativeint.shift_left a (Nativeint.to_int b)))
  (* x << 0 = x *)
  | _, Some (Iinst_cst 0n) -> r1
  (* general case *)
  | _ -> add_inst ib rout (Iinst_binop (Ibinop_lsl, Iop_reg r1, Iop_reg r2))

let mk_not ib r1 =
  let rout = Reg.fresh () in
  let e1 = find_inst_kind ib r1 in
  match e1 with
  (* constant folding *)
  | Some (Iinst_cst a) -> add_inst ib rout (Iinst_cst (Nativeint.lognot a))
  (* general case *)
  | _ -> add_inst ib rout (Iinst_unop (Iunop_not, Iop_reg r1))

let mk_and ib r1 r2 =
  let rout = Reg.fresh () in
  let e1 = find_inst_kind ib r1 in
  let e2 = find_inst_kind ib r2 in
  match (e1, e2) with
  (* constant folding *)
  | Some (Iinst_cst a), Some (Iinst_cst b) ->
      add_inst ib rout (Iinst_cst (Nativeint.logand a b))
  (* x & 0 = 0 *)
  | _, Some (Iinst_cst 0n) -> add_inst ib rout (Iinst_cst 0n)
  (* general case *)
  | _ -> add_inst ib rout (Iinst_binop (Ibinop_and, Iop_reg r1, Iop_reg r2))

let mk_nand ib r1 r2 = mk_not ib (mk_and ib r1 r2)

let mk_or ib r1 r2 =
  let rout = Reg.fresh () in
  let e1 = find_inst_kind ib r1 in
  let e2 = find_inst_kind ib r2 in
  match (e1, e2) with
  (* constant folding *)
  | Some (Iinst_cst a), Some (Iinst_cst b) ->
      add_inst ib rout (Iinst_cst (Nativeint.logor a b))
  (* x | 0 = x *)
  | _, Some (Iinst_cst 0n) -> r1
  (* 0 | x = x *)
  | Some (Iinst_cst 0n), _ -> r2
  (* general case *)
  | _ -> add_inst ib rout (Iinst_binop (Ibinop_or, Iop_reg r1, Iop_reg r2))

let mk_nor ib r1 r2 = mk_not ib (mk_or ib r1 r2)

let mk_xor ib r1 r2 =
  let rout = Reg.fresh () in
  let e1 = find_inst_kind ib r1 in
  let e2 = find_inst_kind ib r2 in
  match (e1, e2) with
  (* constant folding *)
  | Some (Iinst_cst a), Some (Iinst_cst b) ->
      add_inst ib rout (Iinst_cst (Nativeint.logxor a b))
  (* general case *)
  | _ -> add_inst ib rout (Iinst_binop (Ibinop_xor, Iop_reg r1, Iop_reg r2))

let mk_xnor ib r1 r2 = mk_not ib (mk_xor ib r1 r2)

let mk_call ib fname args =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_call (fname, args))

let set_terminator ib term_kind =
  let cur_bb = Option.get ib.cur_bb in
  Ir.set_term cur_bb term_kind;
  ib.cur_bb <- None

let set_bb ib bb = ib.cur_bb <- Some bb

let mk_bb ib =
  let fn = Option.get ib.cur_func_decl in
  Ir.mk_bb fn

let mk_fn ib name arity =
  let fn = Ir.mk_fn ib.ctx name arity in
  ib.cur_func_decl <- Some fn;
  let entry_bb = mk_bb ib in
  fn.fn_entry <- Some entry_bb.b_label;
  set_bb ib entry_bb;
  fn

let enter_loop ib continue_label break_label =
  Stack.push continue_label ib.continue_labels;
  Stack.push break_label ib.break_labels

let exit_loop ib =
  ignore (Stack.pop ib.continue_labels);
  ignore (Stack.pop ib.break_labels)

(** WARNING: This change the current basic block. *)
let mk_break ib =
  let next_bb = mk_bb ib in
  set_terminator ib (Iinst_jmp (Stack.top ib.break_labels));
  set_bb ib next_bb

(** WARNING: This change the current basic block. *)
let mk_continue ib =
  let next_bb = mk_bb ib in
  set_terminator ib (Iinst_jmp (Stack.top ib.continue_labels));
  set_bb ib next_bb

(** WARNING: This change the current basic block. *)
let mk_loop ib body_gf =
  (* Generate the following code:
   * CURRENT_BB:
   *   ...
   *   jmp L1
   * L1: ; the loop
   *   @body_gf@
   *   jmp L1
   * L2: ; exit bb
   *   ...
   *)
  let loop_bb = mk_bb ib in
  let exit_bb = mk_bb ib in
  set_terminator ib (Iinst_jmp loop_bb.b_label);
  enter_loop ib loop_bb.b_label exit_bb.b_label;
  set_bb ib loop_bb;
  body_gf ib;
  set_terminator ib (Iinst_jmp loop_bb.b_label);
  exit_loop ib;
  set_bb ib exit_bb

(** Removes the given [bb] from the current function. There is some restrictions
    on this function including that: [bb] must not be the entry bb of the function
    nor have any predecessors. *)
let remove_bb ib bb =
  let cur_func_decl = Option.get ib.cur_func_decl in
  (* Can not remove entry basic block. *)
  assert (not (is_entry_bb bb));
  (* Nor a basic block with predecessors. The reason it that
     we then need to update the predecessors, including updating
     their terminator. But it is undefined how to update them. *)
  assert (Label.Set.is_empty bb.b_predecessors);

  (* Removes the bb from the function's map. *)
  cur_func_decl.fn_blocks <- Label.Map.remove bb.b_label cur_func_decl.fn_blocks;
  (* And updates successors (simply remove bb from their predecessors set). *)
  Label.Set.iter
    (fun succ_label ->
      let succ = Label.Map.find succ_label cur_func_decl.fn_blocks in
      succ.b_predecessors <- Label.Set.remove bb.b_label succ.b_predecessors)
    bb.b_successors
