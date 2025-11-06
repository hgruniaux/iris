open Mr

let x86_cc_info =
  {
    cc_caller_saved = X86Regs.x86_caller_saved;
    cc_callee_saved = X86Regs.x86_callee_saved;
    cc_args_regs = X86Regs.x86_args_regs;
    cc_args_regs_count = X86Regs.x86_args_regs_count;
    cc_args_stack_ltr = false;
    (* Right to left *)
    cc_return_reg = Some X86Regs.return_reg;
    cc_caller_cleanup = true;
  }

let x64_cc_info =
  {
    cc_caller_saved = X86Regs.x64_caller_saved;
    cc_callee_saved = X86Regs.x64_callee_saved;
    cc_args_regs = X86Regs.x64_args_regs;
    cc_args_regs_count = X86Regs.x64_args_regs_count;
    cc_args_stack_ltr = false;
    (* Right to left *)
    cc_return_reg = Some X86Regs.return_reg;
    cc_caller_cleanup = true;
  }

let from_ir_operand op =
  match op with
  | Ir.Ival_reg r -> Mr.Oreg r
  | Ir.Ival_int (_, i) -> Mr.Oimm i
  | Ir.Ival_global g -> Mr.Oglobal g
  | _ -> failwith "from_ir_operand: unsupported operand type"

let uses_from_operands ops =
  let rec loop ops acc =
    match ops with
    | [] -> []
    | op :: remaining -> (
        match op with
        | Ir.Ival_reg r -> loop remaining (r :: acc)
        | _ -> loop remaining acc)
  in
  loop ops []

let use_from_operand op = match op with Ir.Ival_reg r -> [ r ] | _ -> []

let insert_mov_regs insts r1 r2 =
  insts :=
    Mr.mk_inst "mov" [ Mr.Oreg r1; Mr.Oreg r2 ] ~defs:[ r1 ] ~uses:[ r2 ]
      ~is_mov:true
    :: !insts

let insert_mov insts r1 r2 =
  insts :=
    Mr.mk_inst "mov"
      [ Mr.Oreg r1; from_ir_operand r2 ]
      ~defs:[ r1 ] ~uses:(use_from_operand r2) ~is_mov:true
    :: !insts

let insert_alloca insts r1 t =
  let size = Machine_info.size_of t in

  insts :=
    Mr.mk_inst "sub"
      [ Mr.Oreg X86Regs.esp; Mr.Oimm (Z.of_int size) ]
      ~defs:[ X86Regs.esp ] ~uses:[ X86Regs.esp ]
    :: !insts;
  insert_mov_regs insts r1 X86Regs.esp

let insert_load_mem insts r1 r_base shift offset =
  insts :=
    Mr.mk_inst "mov"
      [ Mr.Oreg r1; Mr.Omem (r_base, shift, offset) ]
      ~defs:[ r1 ] ~uses:[ r_base ]
    :: !insts

let insert_store_mem insts r_base shift offset value =
  insts :=
    Mr.mk_inst "mov"
      [ Mr.Omem (r_base, shift, offset); from_ir_operand value ]
      ~defs:[]
      ~uses:(r_base :: use_from_operand value)
    :: !insts

let insert_mov_global insts r1 global =
  insts :=
    Mr.mk_inst "mov" [ Mr.Oreg r1; Mr.Oglobal global ] ~defs:[ r1 ] ~uses:[]
    :: !insts

let from_ir_operand_as_reg insts op =
  match op with
  | Ir.Ival_reg r -> r
  | _ ->
      let tmp = Mr.Reg.fresh (Ir.Value.type_of op) in
      insert_mov insts tmp op;
      tmp

let insert_binop_util insts kind r1 r2 r3 =
  insert_mov insts r1 r2;
  insts :=
    Mr.mk_inst kind
      [ Mr.Oreg r1; from_ir_operand r3 ]
      ~defs:[ r1; X86Regs.eflags ]
      ~uses:(r1 :: use_from_operand r3)
    :: !insts

let insert_add insts r1 r2 r3 = insert_binop_util insts "add" r1 r2 r3
let insert_sub insts r1 r2 r3 = insert_binop_util insts "sub" r1 r2 r3
let insert_imul insts r1 r2 r3 = insert_binop_util insts "imul" r1 r2 r3
let insert_and insts r1 r2 r3 = insert_binop_util insts "and" r1 r2 r3
let insert_or insts r1 r2 r3 = insert_binop_util insts "or" r1 r2 r3
let insert_xor insts r1 r2 r3 = insert_binop_util insts "xor" r1 r2 r3

let insert_unop_util insts kind r1 r2 =
  insert_mov insts r1 r2;
  insts :=
    Mr.mk_inst kind [ Mr.Oreg r1 ] ~defs:[ r1; X86Regs.eflags ] ~uses:[ r1 ]
    :: !insts

let insert_not insts r1 r2 = insert_unop_util insts "not" r1 r2
let insert_neg insts r1 r2 = insert_unop_util insts "neg" r1 r2

let insert_div_util insts kind rout r1 r2 r3 ~is_x64 =
  insert_mov insts r1 r2;
  let r3 = from_ir_operand_as_reg insts r3 in
  insert_mov_regs insts X86Regs.eax r1;
  insts :=
    Mr.mk_inst
      (if is_x64 then "cqo" else "cdq")
      []
      ~defs:[ X86Regs.eax; X86Regs.edx ]
      ~uses:[ X86Regs.eax ]
    :: !insts;
  insts :=
    Mr.mk_inst kind [ Mr.Oreg r3 ]
      ~defs:[ X86Regs.eax; X86Regs.edx; X86Regs.eflags ]
      ~uses:[ X86Regs.eax; X86Regs.edx; r3 ]
    :: !insts;
  insert_mov_regs insts r1 rout

let insert_div insts r1 r2 r3 = insert_div_util insts "div" X86Regs.eax r1 r2 r3
let insert_rem insts r1 r2 r3 = insert_div_util insts "div" X86Regs.edx r1 r2 r3

let insert_idiv insts r1 r2 r3 =
  insert_div_util insts "idiv" X86Regs.eax r1 r2 r3

let insert_irem insts r1 r2 r3 =
  insert_div_util insts "idiv" X86Regs.edx r1 r2 r3

let insert_shift_util insts kind r1 r2 r3 =
  insert_mov insts r1 r2;
  let cl = X86Regs.ecx in
  (* We can not use from_ir_operand as shift instruction on x86 requires
     that the shift count is stored in the register CL (except if it is
     an immediate). *)
  let r3_op =
    match r3 with
    | Ir.Ival_reg r3 ->
        insert_mov_regs insts cl r3;
        Mr.Oreg cl
    | Ir.Ival_int (_, imm) -> Mr.Oimm imm
    | _ -> failwith "insert_shift_util: unsupported operand type"
  in
  insts :=
    Mr.mk_inst kind [ Mr.Oreg r1; r3_op ] ~uses:[ r1; cl ]
      ~defs:[ r1; X86Regs.eflags ]
    :: !insts

let insert_shl insts r1 r2 r3 = insert_shift_util insts "shl" r1 r2 r3
let insert_sar insts r1 r2 r3 = insert_shift_util insts "sar" r1 r2 r3
let insert_shr insts r1 r2 r3 = insert_shift_util insts "shr" r1 r2 r3

let insert_cmp_util insts cc r1 r2 r3 =
  let r2 = from_ir_operand_as_reg insts r2 in
  (* cmp r2 r3 *)
  insts :=
    Mr.mk_inst "cmp"
      [ Oreg r2; from_ir_operand r3 ]
      ~defs:[ X86Regs.eflags ]
      ~uses:(r2 :: use_from_operand r3)
    :: !insts;
  (* setcc r1 *)
  insts :=
    Mr.mk_inst ("set" ^ cc) [ Oreg r1 ] ~defs:[ r1 ] ~uses:[ X86Regs.eflags ]
    :: !insts;
  (* and r1, 0xff *)
  insts :=
    Mr.mk_inst "and"
      [ Oreg r1; Oimm (Z.of_int 0xff) ]
      ~defs:[ r1; X86Regs.eflags ] ~uses:[ r1 ]
    :: !insts

let insert_cast ~is_x64 insts r1 castop _ v =
  match castop with
  | Ir.Icast_bitcast ->
      (* Just a move *)
      insert_mov insts r1 v
  | Ir.Icast_extend_s ->
      (* Sign-extend *)
      insts :=
        Mr.mk_inst
          (if is_x64 then "movsx" else "movsx")
          [ Oreg r1; from_ir_operand v ]
          ~defs:[ r1 ] ~uses:(use_from_operand v)
        :: !insts
  | Ir.Icast_extend_u ->
      (* Zero-extend *)
      insts :=
        Mr.mk_inst
          (if is_x64 then "movzx" else "movzx")
          [ Oreg r1; from_ir_operand v ]
          ~defs:[ r1 ] ~uses:(use_from_operand v)
        :: !insts
  | Ir.Icast_trunc ->
      (* Truncate: just move the value, the upper bits are ignored *)
      insert_mov insts r1 v
  | Ir.Icast_promote | Ir.Icast_demote ->
      failwith "insert_cast: float cast operations not implemented yet"
  | Ir.Icast_fp2ui | Ir.Icast_fp2si | Ir.Icast_ui2fp | Ir.Icast_si2fp ->
      failwith "insert_cast: float-integer cast operations not implemented yet"
  | Ir.Icast_ptr2int | Ir.Icast_int2ptr ->
      (* Just a move *)
      insert_mov insts r1 v

let insert_push insts r1 =
  insts :=
    Mr.mk_inst "push"
      [ from_ir_operand r1 ]
      ~defs:[ X86Regs.esp ] ~uses:(use_from_operand r1)
    :: !insts

let insert_pop insts r1 =
  insts :=
    Mr.mk_inst "pop"
      [ from_ir_operand r1 ]
      ~defs:(X86Regs.esp :: use_from_operand r1)
      ~uses:[]
    :: !insts

let insert_jmp insts target_label target_args =
  (* FIXME: handle block arguments *)
  ignore target_args;
  insts :=
    Mr.mk_inst "jmp" [ Olabel target_label ] ~defs:[] ~uses:[ X86Regs.eflags ]
    :: !insts

let insert_jmp_conditional insts cond true_label true_args false_label
    false_args =
  (* FIXME: handle blocks arguments *)
  ignore true_args;
  ignore false_args;

  let cond = from_ir_operand_as_reg insts cond in
  insts :=
    Mr.mk_inst "cmp" [ Oreg cond; Oimm Z.zero ] ~defs:[ X86Regs.eflags ]
      ~uses:[ cond ]
    :: !insts;
  insts :=
    (* The true_args and else_label are inversed below because we check
       on zero (or false). *)
    Mr.mk_inst "jz"
      [ Olabel false_label; Olabel true_label ]
      ~defs:[] ~uses:[ X86Regs.eflags ]
    :: !insts

let insert_call insts cc_info r1 callee args =
  insts :=
    Mr.mk_inst "call"
      (Oreg r1 :: from_ir_operand callee :: args)
      ~defs:(Mr.RegSet.elements cc_info.cc_caller_saved)
      ~uses:[]
    :: !insts

let insert_ret insts cc_info =
  insts :=
    Mr.mk_inst "ret" [] ~defs:[]
      ~uses:(Mr.RegSet.elements cc_info.cc_callee_saved)
    :: !insts

(** Same as insert_ret but takes a value to return.

    This will insert either a mov or a push instruction (depending on the
    calling convention) to pass to caller the given [value]. Moreover, the uses
    field of the ret instruction is different from the one returning void. *)
let insert_ret_value insts cc_info value =
  let return_reg_use =
    match cc_info.cc_return_reg with
    | None ->
        (* Should probably never happen, as almost always the return value is
           stored in EAX under x86 or x86-64. But for sake of compatibility of
           the general custom calling convention interface, we support it. *)
        insert_push insts value;
        []
    | Some return_reg ->
        insert_mov insts return_reg value;
        [ return_reg ]
  in
  insts :=
    Mr.mk_inst "ret" [] ~defs:[]
      ~uses:(return_reg_use @ Mr.RegSet.elements cc_info.cc_callee_saved)
    :: !insts

(** Should we use the x86 enter instruction to allocate the function's frame or
    push and mov? By default, we use push and mov instructions instead (WAY
    faster on modern CPUs). You should never use the enter instruction instead
    of push and mov, see:
    - comment in insert_frame_alloc
    - https://stackoverflow.com/a/67424971 *)
let use_enter_inst = ref false

(** Should we use the x86 leave instruction to deallocate the function's frame
    or pop and mov? By default, we use mov and pop instructions instead (faster
    on modern CPUs). *)
let use_leave_inst = ref false

(* For the function prologue and epilogue (frame allocation and deallocation),
   see https://en.wikipedia.org/wiki/Function_prologue_and_epilogue. *)

let insert_frame_alloc insts fn =
  (*
    push ebp
    mov ebp, esp
    sub esp, N
  *)
  let frame = Option.get fn.Mr.mfn_frame in
  let n = Z.of_int (frame.frame_locals * 8) in
  if !use_enter_inst then
    (* In practice, no one should use the enter instruction instead of push then mov.
       The former is way slower on modern CPUs. Still, for sake of completeness
       and comparison, we support code generation with the enter instruction.
       Ref: https://stackoverflow.com/a/67424971 *)
    insts :=
      Mr.mk_inst "enter" [ Oimm n; Oimm Z.zero ]
        ~defs:[ X86Regs.esp; X86Regs.ebp ]
        ~uses:[ X86Regs.esp ]
      :: !insts
  else (
    insert_push insts (Ir.Ival_reg X86Regs.ebp);
    insert_mov_regs insts X86Regs.ebp X86Regs.esp;
    if n > Z.zero then (
      (* Free stack space of local variables *)
      insert_sub insts X86Regs.esp (Ir.Ival_reg X86Regs.esp)
        (Ir.Ival_int (Ityp_i64, n));
      (* Insert code to align the stack pointer to 16. *)
      (* TODO: The compiler should automatically align the stack. *)
      insert_and insts X86Regs.esp (Ir.Ival_reg X86Regs.esp)
        (Ir.Ival_int (Ityp_i64, Z.neg (Z.of_int 16)))))

let insert_frame_dealloc insts fn =
  (*
    mov esp, ebp
    pop ebp
  *)
  ignore fn;
  if !use_leave_inst then
    insts :=
      Mr.mk_inst "leave" []
        ~defs:[ X86Regs.esp; X86Regs.ebp ]
        ~uses:[ X86Regs.ebp ]
      :: !insts
  else (
    insert_mov_regs insts X86Regs.esp X86Regs.ebp;
    insert_pop insts (Ir.Ival_reg X86Regs.ebp))

let mk_mov r1 r2 =
  Mr.mk_inst "mov" [ Mr.Oreg r1; Mr.Oreg r2 ] ~defs:[ r1 ] ~uses:[ r2 ]
    ~is_mov:true

let mk_mov_operand r1 operand =
  match operand with
  | Mr.Oreg r2 -> mk_mov r1 r2
  | _ -> Mr.mk_inst "mov" [ Mr.Oreg r1; operand ] ~defs:[ r1 ] ~uses:[]

let mk_push r1 =
  Mr.mk_inst "push" [ Oreg r1 ] ~defs:[ X86Regs.esp ] ~uses:[ X86Regs.esp; r1 ]

let mk_pop r1 =
  Mr.mk_inst "pop" [ Oreg r1 ] ~defs:[ X86Regs.esp; r1 ] ~uses:[ X86Regs.esp ]

let mk_pop_many arch count =
  (* TODO: fix item size * count *)
  ignore arch;
  let insts = ref [] in
  insert_add insts X86Regs.esp (Ir.Ival_reg X86Regs.esp)
    (Ir.Ival_int (Ityp_i64, Z.of_int count));
  List.rev !insts
