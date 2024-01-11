open Mir

(** This pass modify the MIR to replace each occurrence of a pseudo register
    to its selected physical register (result of register allocation). If the
    pseudo register is spilled, this pass also inserts load and store
    instructions to the stack. *)
let pass_fn colors fn =
  MirPassUtils.iter_insts fn (fun inst ->
      let rewrite_reg r =
        match Reg.Map.find_opt r colors with
        | None -> Some r
        | Some (RegAlloc.Spilled _) -> None
        | Some (RegAlloc.Reg r) -> Some r
      in

      let rewrite_operand o =
        match o with
        | Oreg r -> (
            match Reg.Map.find_opt r colors with
            | None -> o
            | Some (RegAlloc.Spilled n) -> Oframe n
            | Some (RegAlloc.Reg r) -> Oreg r)
        | _ -> o
      in

      inst.i_operands <- List.map rewrite_operand inst.i_operands;
      inst.i_uses <- Reg.Set.filter_map rewrite_reg inst.i_uses;
      inst.i_defs <- Reg.Set.filter_map rewrite_reg inst.i_defs)
