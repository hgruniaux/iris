open Ir

let pass_fn fn =
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
          assert ((not (is_term kind)) && not (is_phi kind)))
        bb.b_insts;

      (* All basic blocks must have a terminator instruction. *)
      assert (Option.is_some bb.b_term))
    fn.fn_blocks;

  (* Do not change the code, only verify it. *)
  false
