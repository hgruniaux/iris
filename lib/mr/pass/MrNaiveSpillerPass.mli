(** Minimal set of backend-specific functions needed by this pass. *)
module type MrBuilder = sig
  val spiller_registers : Mr.reg list
  val mk_frame_load : Mr.reg -> int -> Mr.minst list
  val mk_frame_store : int -> Mr.reg -> Mr.minst list
end

(** This pass inserts intermediate loads and stores to manage variable overflows.
    After this pass, no more pseudo registers that have been spilled are in the
    function.

    The current implementation of this pass is terribly naive (hence the name).
    We use a certain number of registers which are reserved (cannot be used
    anywhere other than in this pass). These reserved registers are used to
    temporarily store values after retrieving them from the stack or before
    storing them again.

    The number of reserved registers required must be greater than or equal
    to the largest number of registers that can be used by one instruction at
    a time. On x86, 2 are required. *)
module Make (_ : MrBuilder) : MrPass.Pass
