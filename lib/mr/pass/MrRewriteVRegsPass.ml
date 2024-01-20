open Mr

(** This pass rewrites all occurrences of a pseudo-register with the associated
    physical register (after register allocation).

    It is assumed that the code does not contain spilled pseudo-registers (they
    must have been replaced by the spiller). *)
let pass_fn am fn =
  let colors = AnalysisManager.regalloc am in
  MrPassUtils.iter_insts fn (fun inst ->
      let rewrite_reg r =
        match Reg.Map.find_opt r colors with
        | None -> Some r
        | Some (RegAlloc.Spilled _) -> assert false
        | Some (RegAlloc.Reg r) -> Some r
      in

      let rewrite_operand o =
        match o with
        | Oreg r -> (
            match Reg.Map.find_opt r colors with
            | None -> o
            | Some (RegAlloc.Spilled _) -> assert false
            | Some (RegAlloc.Reg r) -> Oreg r)
        | _ -> o
      in

      inst.mi_operands <- List.map rewrite_operand inst.mi_operands;
      inst.mi_uses <- Reg.Set.filter_map rewrite_reg inst.mi_uses;
      inst.mi_defs <- Reg.Set.filter_map rewrite_reg inst.mi_defs)
