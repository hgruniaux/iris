(** Minimal set of backend-specific functions needed by this pass. *)
module type MrBuilder = sig
  val mk_mov_reg : Mr.reg -> Mr.reg -> Mr.minst list
  val mk_frame_load : Mr.reg -> int -> Mr.minst list
end

(** This pass inserts move instructions at the start of the function to store
    the function parameters in the corresponding pseudo-registers. Indeed,
    the parameters can either be passed via physical registers or the stack. *)
module Make (_ : MrBuilder) : MrPass.Pass
