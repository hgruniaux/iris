open Ir

(** This pass check if the given IR function is well-formed:
      - all basic blocks must have one terminator
      - PHI instructions or terminators must not be in the middle of a basic block
      - the function must be correctly typed
        - a function returning void can not have a ret with value instruction
        - adding void and an integer is not valid
        - etc.
      - etc.

    Obviously, this pass does  not modify the input IR function. Moreover,
    the checks are not intended to be exhaustive (there are many others asserts
    scattered all over the place). *)
let pass_fn _ fn =
  Label.Map.iter
    (fun _ bb ->
      (* The list b_phi_insts must only have PHI instructions. *)
      List.iter
        (fun inst ->
          let kind = inst.i_kind in
          assert (is_phi kind))
        bb.b_phi_insts;

      (* There must be no PHI instructions or terminators in b_insts. *)
      List.iter
        (fun inst ->
          let kind = inst.i_kind in
          assert (not (is_phi kind)))
        bb.b_insts;

      (* All basic blocks must have a terminator instruction. *)
      assert (Option.is_some bb.b_term))
    fn.fn_blocks;

  (* Does not change the code, only verify it. *)
  false
