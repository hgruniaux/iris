(** Minimal set of backend-specific functions needed by this pass. *)
module type MrBuilder = sig
  val cc_info_of : Ir.fn -> Mr.calling_convention_info
  val sizeof_operand : Mr.operand -> int
  val is_call : Mr.minst -> bool
  val mk_mov_operand : Mr.reg -> Mr.operand -> Mr.minst list
  val mk_mov_reg : Mr.reg -> Mr.reg -> Mr.minst list
  val mk_push_operand : Mr.operand -> Mr.minst list
  val mk_pop_bytes : int -> Mr.minst list
  val mk_pop_register : Mr.reg -> Mr.minst list
  val mk_call : Ir.fn -> Mr.Reg.set -> Mr.minst list
end

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
module Make (_ : MrBuilder) : MrPass.Pass
