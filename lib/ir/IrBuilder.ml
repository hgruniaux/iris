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
    ctx;
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

(** The true constant. *)
let mk_true ib = mk_const ib (Ir.get_int_constant ib.ctx 1n)

(** The false constant. *)
let mk_false ib = mk_const ib (Ir.get_int_constant ib.ctx 0n)

(** An integer constant. *)
let mk_int ib imm = mk_const ib (Ir.get_int_constant ib.ctx imm)

(** A string constant. The final NUL byte is not added by this function,
    so if you want a NUL-terminated string either call mk_zstring or
    add it explicitly yourself. *)
let mk_string ib str = mk_const ib (Ir.get_string_constant ib.ctx str)

(** Same as [mk_string] but add an explicit final NUL character to generate
          NUL-terminated string. *)
let mk_zstring ib str = mk_string ib (str ^ "\x00")

let mk_extract_value ib cst idx =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_extract_value (cst, idx))

let mk_mov ib r1 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_mov r1)

(** Loads the value stored in RAM at the address pointed by [r1]. *)
let mk_load ib r1 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_load r1)

(** Stores the value of [r1] in RAM at the address pointed by [r2]. *)
let mk_store ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_store (r1, Iop_reg r2))

(** Negates the value of [r1]. *)
let mk_neg ib r1 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_unop (Iunop_neg, Iop_reg r1))

let mk_add ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_binop (Ibinop_add, Iop_reg r1, Iop_reg r2))

let mk_sub ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_binop (Ibinop_sub, Iop_reg r1, Iop_reg r2))

let mk_mul ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_binop (Ibinop_mul, Iop_reg r1, Iop_reg r2))

let mk_sdiv ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_binop (Ibinop_udiv, Iop_reg r1, Iop_reg r2))

let mk_sdiv ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_binop (Ibinop_sdiv, Iop_reg r1, Iop_reg r2))

let mk_urem ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_binop (Ibinop_urem, Iop_reg r1, Iop_reg r2))

let mk_srem ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_binop (Ibinop_srem, Iop_reg r1, Iop_reg r2))

let mk_eq ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_cmp (Icmp_eq, Iop_reg r1, Iop_reg r2))

let mk_ne ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_cmp (Icmp_ne, Iop_reg r1, Iop_reg r2))

(** Signed less than. *)
let mk_slt ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_cmp (Icmp_slt, Iop_reg r1, Iop_reg r2))

(** Signed less than or equal to. *)
let mk_sle ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_cmp (Icmp_sle, Iop_reg r1, Iop_reg r2))

(** Signed greater than. *)
let mk_sgt ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_cmp (Icmp_sgt, Iop_reg r1, Iop_reg r2))

(** Signed greater than or equal to. *)
let mk_sge ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_cmp (Icmp_sge, Iop_reg r1, Iop_reg r2))

let mk_asr ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_binop (Ibinop_asr, Iop_reg r1, Iop_reg r2))

(** Logical right shift. *)
let mk_lsr ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_binop (Ibinop_lsr, Iop_reg r1, Iop_reg r2))

(** Logical left shift. *)
let mk_lsl ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_binop (Ibinop_lsl, Iop_reg r1, Iop_reg r2))

(** Bitwise NOT. *)
let mk_not ib r1 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_unop (Iunop_not, Iop_reg r1))

(** Bitwise AND. *)
let mk_and ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_binop (Ibinop_and, Iop_reg r1, Iop_reg r2))

(** Bitwise NAND. *)
let mk_nand ib r1 r2 = mk_not ib (mk_and ib r1 r2)

(** Bitwise OR. *)
let mk_or ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_binop (Ibinop_or, Iop_reg r1, Iop_reg r2))

(** Bitwise NOR. *)
let mk_nor ib r1 r2 = mk_not ib (mk_or ib r1 r2)

(** Bitwise XOR. *)
let mk_xor ib r1 r2 =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_binop (Ibinop_xor, Iop_reg r1, Iop_reg r2))

(** Bitwise XNOR. *)
let mk_xnor ib r1 r2 = mk_not ib (mk_xor ib r1 r2)

(** A call to the function [fn] with the given [args]. *)
let mk_call ib fn args =
  assert (List.length args = List.length fn.fn_params);
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_call (fn, args))

(** A call to the C stdlib malloc() function with the given [size] in char unit
    (generally bytes).

    Note, all backends may not support the standard C library. *)
let mk_malloc ib size =
  let fn =
    get_or_insert_fn ib.ctx "malloc" Ityp_ptr [ Ityp_int ] ~is_external:true
  in
  mk_call ib fn [ Iop_imm size ]

(** A call to the C stdlib free() function that frees the given [ptr] (a register
    storing the pointer).

    Note, all backends may not support the standard C library. *)
let mk_free ib ptr =
  let fn =
    get_or_insert_fn ib.ctx "free" Ityp_void [ Ityp_ptr ] ~is_external:true
  in
  mk_call ib fn [ Iop_reg ptr ]

let set_terminator ib term_kind =
  let cur_bb = Option.get ib.cur_bb in
  Ir.set_term cur_bb term_kind;
  ib.cur_bb <- None

let set_bb ib bb = ib.cur_bb <- Some bb

let mk_bb ib =
  let fn = Option.get ib.cur_func_decl in
  Ir.mk_bb fn

let mk_fn ib name arity =
  let fn =
    get_or_insert_fn ib.ctx name Ityp_int (List.init arity (fun _ -> Ityp_int))
  in
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

let mk_if_expr ib cond then_gf else_gf =
  (* TODO: check mk_if_expr *)
  let then_bb = mk_bb ib in
  let else_bb = mk_bb ib in
  let exit_bb = mk_bb ib in
  set_terminator ib
    (Iinst_jmpc (Iop_reg cond, then_bb.b_label, else_bb.b_label));

  (* Then branch *)
  set_bb ib then_bb;
  let then_value = then_gf ib in
  set_terminator ib (Iinst_jmp exit_bb.b_label);

  (* Else branch *)
  set_bb ib else_bb;
  let else_value = else_gf ib in
  set_terminator ib (Iinst_jmp exit_bb.b_label);

  (* End *)
  set_bb ib exit_bb;
  let rout = Reg.fresh () in
  add_inst ib rout
    (Iinst_phi
       [
         (Iop_reg then_value, then_bb.b_label);
         (Iop_reg else_value, else_bb.b_label);
       ])

(** Lazy logical AND. *)
let mk_logical_and ib lhs rhs_gf =
  (* TODO: test mk_logical_and *)
  let not_lhs = mk_not ib lhs in
  mk_if_expr ib not_lhs (fun ib -> mk_false ib) rhs_gf

(** Lazy logical OR. *)
let mk_logical_or ib lhs rhs_gf =
  (* TODO: test mk_logical_or *)
  mk_if_expr ib lhs (fun ib -> mk_true ib) rhs_gf

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
