module type MrBuilder = sig
  val prolog : Mr.mfn -> Mr.minst list
  val epilog : Mr.mfn -> Mr.minst list
end

module Make (Builder : MrBuilder) = struct
  (** This pass inserts the prolog and epilog code of a function. Generally,
      these two allocate and free the function's stack frame. *)
  let pass_fn _ fn =
    let prolog = Builder.prolog fn in
    let epilog = Builder.epilog fn in
    MrPassUtils.insert_prolog fn prolog;
    MrPassUtils.insert_epilog fn epilog
end
