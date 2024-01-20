open Mr
open MrPassUtils

module type MrBuilder = sig
  val cc_info_of : Ir.fn -> Mr.calling_convention_info
  val sizeof_operand : Mr.operand -> int
  val is_call : Mr.minst -> bool
  val mk_mov_operand : Mr.reg -> Mr.operand -> Mr.minst list
  val mk_mov_reg : Mr.reg -> Mr.reg -> Mr.minst list
  val mk_push_operand : Mr.operand -> Mr.minst list
  val mk_pop_bytes : int -> Mr.minst list
  val mk_pop_register : Mr.reg -> Mr.minst list
  val mk_call : Ir.fn -> Reg.set -> Mr.minst list
end

let extract_reg_from = function
  | Oreg r -> r
  | _ -> failwith "expected a register"

let extract_callee_from = function
  | Ofunc f -> f
  | _ -> failwith "expected a function"

module Make (Builder : MrBuilder) = struct
  (** This pass converts function calls (still high-level) to a lower-level
    * representation.
    *
    * In particular, it explains the calling convention and its implementation.
    * For example, it inserts move instructions to move the arguments of the
    * call to physical registers or push instructions. It also inserts either
    * a move instruction or a pop to retrieve the return value.
    *
    * After this pass, all function call instructions no longer take any arguments
    * except the function name, and no longer explicitly store the return value.
    * In other words, they have exactly the same semantics as the call instruction
    * on CPUs. *)
  let pass_fn _ fn =
    MrPassUtils.map_insts fn (fun inst ->
        if not (Builder.is_call inst) then [ inst ]
        else
          (* The (pseudo)-register where to store the return value. *)
          let return_reg = extract_reg_from (List.hd inst.mi_operands) in
          (* The callee function address. *)
          let callee =
            extract_callee_from (List.hd (List.tl inst.mi_operands))
          in
          (* The calling convention to use to call [callee]. *)
          let cc_info = Builder.cc_info_of callee in
          let args = List.tl (List.tl inst.mi_operands) in

          let args_in_regs, args_in_stack =
            firstk cc_info.cc_args_regs_count args
          in

          (* All instructions that must be inserted just BEFORE the call instruction. *)
          let precall_insts = ref [] in
          (* All instructions that must be inserted just AFTER the call instruction. *)
          let postcall_insts = ref [] in

          (* Inserts the move instructions for the parameters lying in registers. *)
          lax_iter2
            (fun arg_operand physical_reg ->
              precall_insts :=
                Builder.mk_mov_operand physical_reg arg_operand @ !precall_insts)
            args_in_regs cc_info.cc_args_regs;
          (* Move instructions are inserted in reverse, fix that. *)
          precall_insts := List.rev !precall_insts;

          (* Inserts the push instructions for the parameters lying in the stack. *)
          let push_insts =
            let insts =
              List.fold_left
                (fun insts operand -> Builder.mk_push_operand operand @ insts)
                [] args_in_stack
            in
            (* Reverse instructions if arguments are expected left to right. *)
            if cc_info.cc_args_stack_ltr then List.rev insts else insts
          in
          precall_insts := !precall_insts @ push_insts;

          let reg_defs = ref cc_info.cc_caller_saved in

          (* Retrieve the returned value either from a physical register
             or the stack depending on the calling convention. *)
          (if Ir.return_type_of callee <> Ir.Ityp_void then
             match cc_info.cc_return_reg with
             | Some r ->
                 reg_defs := Reg.Set.add return_reg !reg_defs;
                 postcall_insts := Builder.mk_mov_reg return_reg r
             | None -> postcall_insts := Builder.mk_pop_register return_reg);

          (* Pop arguments from stack if required. *)
          (if cc_info.cc_caller_cleanup then
             let byte_count =
               List.fold_left
                 (fun byte_count arg -> byte_count + Builder.sizeof_operand arg)
                 0 args_in_stack
             in
             postcall_insts := !postcall_insts @ Builder.mk_pop_bytes byte_count);

          !precall_insts @ Builder.mk_call callee !reg_defs @ !postcall_insts)
end
