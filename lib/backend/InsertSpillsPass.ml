open Mir

let handle_inst colors inst =
  let load_insts = ref [] in
  let store_insts = ref [] in
  let tmp_regs = Hashtbl.create 3 in

  (* Generate load instructions for used spilled registers. *)
  inst.i_uses <-
    Reg.Set.map
      (fun use ->
        match Reg.Map.find_opt use colors with
        | Some (RegAlloc.Spilled n) -> (
            match Hashtbl.find_opt tmp_regs n with
            | Some r -> r
            | None ->
                let tmp_reg = Reg.fresh () in
                load_insts := Mir.mk_stack_load tmp_reg n :: !load_insts;
                Hashtbl.add tmp_regs n tmp_reg;
                tmp_reg)
        | _ -> use)
      inst.i_uses;

  (* Generate store instructions for defined spilled registers. *)
  inst.i_defs <-
    Reg.Set.map
      (fun use ->
        match Reg.Map.find_opt use colors with
        | Some (RegAlloc.Spilled n) -> (
            match Hashtbl.find_opt tmp_regs n with
            | Some r -> r
            | None ->
                let tmp_reg = Reg.fresh () in
                store_insts := Mir.mk_stack_store n tmp_reg :: !load_insts;
                Hashtbl.add tmp_regs n tmp_reg;
                tmp_reg)
        | _ -> use)
      inst.i_defs;

  (* Update operands to use the newly introduced temporary registers. *)
  inst.i_operands <-
    List.map
      (function
        | Oreg r as o -> (
            match Reg.Map.find_opt r colors with
            | Some (RegAlloc.Spilled n) -> Oreg (Hashtbl.find tmp_regs n)
            | _ -> o)
        | o -> o)
      inst.i_operands;

  (!load_insts, !store_insts)

let pass_fn colors fn =
  Label.Map.iter
    (fun _ bb ->
      bb.bb_insts <-
        List.fold_right
          (fun inst acc ->
            let load_insts, store_insts = handle_inst colors inst in
            load_insts @ (inst :: (store_insts @ acc)))
          bb.bb_insts [])
    fn.fn_blocks
