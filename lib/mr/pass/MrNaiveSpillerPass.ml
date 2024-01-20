open Mr

module type MrBuilder = sig
  val spiller_registers : Mr.reg list
  val mk_frame_load : Mr.reg -> int -> Mr.minst list
  val mk_frame_store : int -> Mr.reg -> Mr.minst list
end

module type Pass = sig
  val pass_fn : AnalysisManager.t -> Mr.mfn -> unit
end

module Make (Builder : MrBuilder) = struct
  let handle_inst tmp_regs colors inst =
    let available_tmp_regs = ref tmp_regs in
    let load_insts = ref [] in
    let store_insts = ref [] in
    let mapping = Hashtbl.create 2 in

    let choose_tmp_reg () =
      match !available_tmp_regs with
      | [] ->
          failwith "not enough temp physical registers for register spilling"
      | x :: r ->
          available_tmp_regs := r;
          x
    in

    (* Generate load instructions for used spilled registers. *)
    inst.mi_uses <-
      Reg.Set.map
        (fun use ->
          match Reg.Map.find_opt use colors with
          | Some (RegAlloc.Spilled n) -> (
              match Hashtbl.find_opt mapping n with
              | Some r -> r
              | None ->
                  let tmp_reg = choose_tmp_reg () in
                  load_insts := Builder.mk_frame_load tmp_reg n @ !load_insts;
                  Hashtbl.add mapping n tmp_reg;
                  tmp_reg)
          | _ -> use)
        inst.mi_uses;

    (* Generate store instructions for defined spilled registers. *)
    inst.mi_defs <-
      Reg.Set.map
        (fun use ->
          match Reg.Map.find_opt use colors with
          | Some (RegAlloc.Spilled n) -> (
              match Hashtbl.find_opt mapping n with
              | Some r ->
                  store_insts := Builder.mk_frame_store n r @ !load_insts;
                  r
              | None ->
                  let tmp_reg = choose_tmp_reg () in
                  store_insts := Builder.mk_frame_store n tmp_reg @ !load_insts;
                  Hashtbl.add mapping n tmp_reg;
                  tmp_reg)
          | _ -> use)
        inst.mi_defs;

    (* Update operands to use the newly introduced temporary registers. *)
    inst.mi_operands <-
      List.map
        (function
          | Oreg r as o -> (
              match Reg.Map.find_opt r colors with
              | Some (RegAlloc.Spilled n) -> Oreg (Hashtbl.find mapping n)
              | _ -> o)
          | o -> o)
        inst.mi_operands;

    (!load_insts, !store_insts)

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
  let pass_fn am fn =
    let colors = AnalysisManager.regalloc am in

    let locals_count =
      Reg.Map.fold
        (fun _ color locals_count ->
          match color with
          | RegAlloc.Spilled n -> max locals_count (n + 1)
          | _ -> locals_count)
        colors 0
    in

    (* Update the function's frame. *)
    fn.mfn_frame <-
      Some
        {
          frame_params = List.length fn.mfn_params;
          frame_locals = locals_count;
        };

    let tmp_regs = Builder.spiller_registers in
    Label.Map.iter
      (fun _ bb ->
        bb.mbb_insts <-
          List.fold_right
            (fun inst acc ->
              let load_insts, store_insts = handle_inst tmp_regs colors inst in
              load_insts @ (inst :: (store_insts @ acc)))
            bb.mbb_insts [])
      fn.mfn_blocks
end
