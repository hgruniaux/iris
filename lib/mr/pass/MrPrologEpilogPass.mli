(** Minimal set of backend-specific functions needed by this pass. *)
module type MrBuilder = sig
  val prolog : Mr.mfn -> Mr.minst list
  val epilog : Mr.mfn -> Mr.minst list
end

(** This pass inserts the prologue and epilogue of the function (specific to
    the target architecture) if required. It also updates the function's frame
    information.

    It is the prologue and epilogue that allocates the necessary space on the
    stack for local variables and spilled registers. *)
module Make (_ : MrBuilder) : MrPass.Pass
