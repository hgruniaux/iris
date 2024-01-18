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

let finish ib =
  let fn = Option.get ib.cur_func_decl in
  Label.Map.iter (fun _ bb -> bb.b_insts <- List.rev bb.b_insts) fn.fn_blocks

(** Adds the instruction [inst] that writes to [reg] into the current
    basic block of the given IR Builder [ib]. *)
let add_inst ib reg inst_kind =
  let fn = Option.get ib.cur_func_decl in
  let bb = Option.get ib.cur_bb in
  let inst = Instruction.create fn reg inst_kind in
  bb.b_insts <- inst :: bb.b_insts;
  reg

let mk_const ib c =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_cst c)

(** An integer constant. *)
let mk_int ib imm =
  let rout = Reg.fresh () in
  add_inst ib rout (Iinst_loadi imm)

(** The true constant. *)
let mk_true ib = mk_int ib Z.one

(** The false constant. *)
let mk_false ib = mk_int ib Z.zero

(** A string constant. The final NUL byte is not added by this function,
    so if you want a NUL-terminated string either call mk_zstring or
    add it explicitly yourself. *)
let mk_string ib str = mk_const ib (Ir.get_string_constant ib.ctx str)

(** Same as [mk_string] but add an explicit final NUL character to generate
          NUL-terminated string. *)
let mk_zstring ib str = mk_string ib (str ^ "\x00")

let mk_inst ib kind =
  let rout = Reg.fresh () in
  add_inst ib rout kind

let mk_mov ib r1 = mk_inst ib (Iinst_mov r1)
let mk_load ib t r1 = mk_inst ib (Iinst_load (t, r1))
let mk_store ib r1 r2 = mk_inst ib (Iinst_store (r1, Iop_reg r2))
let mk_neg ib r1 = mk_inst ib (Iinst_unop (Iunop_neg, Iop_reg r1))
let mk_not ib r1 = mk_inst ib (Iinst_unop (Iunop_not, Iop_reg r1))

let mk_binop ib binop r1 r2 =
  mk_inst ib (Iinst_binop (binop, Iop_reg r1, Iop_reg r2))

let mk_add ib r1 r2 = mk_binop ib Ibinop_add r1 r2
let mk_sub ib r1 r2 = mk_binop ib Ibinop_sub r1 r2
let mk_mul ib r1 r2 = mk_binop ib Ibinop_mul r1 r2
let mk_udiv ib r1 r2 = mk_binop ib Ibinop_udiv r1 r2
let mk_sdiv ib r1 r2 = mk_binop ib Ibinop_sdiv r1 r2
let mk_urem ib r1 r2 = mk_binop ib Ibinop_urem r1 r2
let mk_srem ib r1 r2 = mk_binop ib Ibinop_srem r1 r2
let mk_asr ib r1 r2 = mk_binop ib Ibinop_asr r1 r2
let mk_lsr ib r1 r2 = mk_binop ib Ibinop_lsr r1 r2
let mk_lsl ib r1 r2 = mk_binop ib Ibinop_lsl r1 r2
let mk_and ib r1 r2 = mk_binop ib Ibinop_and r1 r2
let mk_or ib r1 r2 = mk_binop ib Ibinop_or r1 r2
let mk_xor ib r1 r2 = mk_binop ib Ibinop_xor r1 r2
let mk_nand ib r1 r2 = mk_not ib (mk_and ib r1 r2)
let mk_nor ib r1 r2 = mk_not ib (mk_or ib r1 r2)
let mk_xnor ib r1 r2 = mk_not ib (mk_xor ib r1 r2)
let mk_cmp ib cmp r1 r2 = mk_inst ib (Iinst_cmp (cmp, Iop_reg r1, Iop_reg r2))
let mk_eq ib r1 r2 = mk_cmp ib Icmp_eq r1 r2
let mk_ne ib r1 r2 = mk_cmp ib Icmp_ne r1 r2
let mk_slt ib r1 r2 = mk_cmp ib Icmp_slt r1 r2
let mk_sle ib r1 r2 = mk_cmp ib Icmp_sle r1 r2
let mk_sgt ib r1 r2 = mk_cmp ib Icmp_sgt r1 r2
let mk_sge ib r1 r2 = mk_cmp ib Icmp_sge r1 r2

(** A call to the function [fn] with the given [args]. *)
let mk_call ib fn args =
  assert (List.length args = List.length fn.fn_params);
  mk_inst ib (Iinst_call (fn, args))

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

let set_term ib term_kind =
  let cur_fn = Option.get ib.cur_func_decl in
  let cur_bb = Option.get ib.cur_bb in
  BasicBlock.set_term cur_fn cur_bb term_kind;
  ib.cur_bb <- None

let set_bb ib bb = ib.cur_bb <- Some bb

let mk_bb ib =
  let fn = Option.get ib.cur_func_decl in
  let bb = BasicBlock.create fn in
  BasicBlock.set_term fn bb Iterm_unreachable;
  bb

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
  set_term ib (Iterm_jmp (Stack.top ib.break_labels));
  set_bb ib next_bb

(** WARNING: This change the current basic block. *)
let mk_continue ib =
  let next_bb = mk_bb ib in
  set_term ib (Iterm_jmp (Stack.top ib.continue_labels));
  set_bb ib next_bb

let mk_if_expr ib cond then_gf else_gf =
  let fn = Option.get ib.cur_func_decl in
  (* TODO: check mk_if_expr *)
  let then_bb = mk_bb ib in
  let else_bb = mk_bb ib in
  let exit_bb = mk_bb ib in
  set_term ib (Iterm_jmpc (Iop_reg cond, then_bb.b_label, else_bb.b_label));

  (* Then branch *)
  set_bb ib then_bb;
  let then_value = then_gf ib in
  set_term ib (Iterm_jmp exit_bb.b_label);

  (* Else branch *)
  set_bb ib else_bb;
  let else_value = else_gf ib in
  set_term ib (Iterm_jmp exit_bb.b_label);

  (* End *)
  set_bb ib exit_bb;
  Instruction.insert_phi fn exit_bb
    [
      (Iop_reg then_value, then_bb.b_label);
      (Iop_reg else_value, else_bb.b_label);
    ]

(** Lazy logical AND. *)
let mk_logical_and ib lhs rhs_gf =
  mk_if_expr ib lhs rhs_gf (fun ib -> mk_false ib)

(** Lazy logical OR. *)
let mk_logical_or ib lhs rhs_gf =
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
  set_term ib (Iterm_jmp loop_bb.b_label);
  enter_loop ib loop_bb.b_label exit_bb.b_label;
  set_bb ib loop_bb;
  body_gf ib;
  set_term ib (Iterm_jmp loop_bb.b_label);
  exit_loop ib;
  set_bb ib exit_bb
