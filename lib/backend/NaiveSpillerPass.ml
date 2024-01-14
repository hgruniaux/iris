open Mir

let handle_inst tmp_regs colors inst =
  let available_tmp_regs = ref tmp_regs in
  let load_insts = ref [] in
  let store_insts = ref [] in
  let mapping = Hashtbl.create 2 in

  let choose_tmp_reg () =
    match !available_tmp_regs with
    | [] -> failwith "not enough temp physical registers for register spilling"
    | x :: r ->
        available_tmp_regs := r;
        x
  in

  (* Generate load instructions for used spilled registers. *)
  inst.i_uses <-
    Reg.Set.map
      (fun use ->
        match Reg.Map.find_opt use colors with
        | Some (RegAlloc.Spilled n) -> (
            match Hashtbl.find_opt mapping n with
            | Some r -> r
            | None ->
                let tmp_reg = choose_tmp_reg () in
                load_insts := Mir.mk_stack_load tmp_reg n :: !load_insts;
                Hashtbl.add mapping n tmp_reg;
                tmp_reg)
        | _ -> use)
      inst.i_uses;

  (* Generate store instructions for defined spilled registers. *)
  inst.i_defs <-
    Reg.Set.map
      (fun use ->
        match Reg.Map.find_opt use colors with
        | Some (RegAlloc.Spilled n) -> (
            match Hashtbl.find_opt mapping n with
            | Some r ->
                store_insts := Mir.mk_stack_store n r :: !load_insts;
                r
            | None ->
                let tmp_reg = choose_tmp_reg () in
                store_insts := Mir.mk_stack_store n tmp_reg :: !load_insts;
                Hashtbl.add mapping n tmp_reg;
                tmp_reg)
        | _ -> use)
      inst.i_defs;

  (* Update operands to use the newly introduced temporary registers. *)
  inst.i_operands <-
    List.map
      (function
        | Oreg r as o -> (
            match Reg.Map.find_opt r colors with
            | Some (RegAlloc.Spilled n) -> Oreg (Hashtbl.find mapping n)
            | _ -> o)
        | o -> o)
      inst.i_operands;

  (!load_insts, !store_insts)

(** A naive implementation of a register spiller. This pass inserts stack
    load and store instructions for spilled registers at each use and def.

    The idea behind the algorithm is very simple (after all it is naive).
    We use temporary registers (given in [tmp_regs], specific to each
    backend) to store the temporary results that are loaded from or
    stored to the stack. The size of [tmp_regs] must be enough for the
    instructions taking the most register operands (2 for x86 for example).

    The reg allocation result [colors] is used to determine what register
    is spilled and where it is stored in the function's frame. *)
let pass_fn tmp_regs colors fn =
  let locals_count =
    Reg.Map.fold
      (fun _ color locals_count ->
        match color with
        | RegAlloc.Spilled n -> max locals_count (n + 1)
        | _ -> locals_count)
      colors 0
  in

  (* Update the function's frame. *)
  fn.fn_frame <-
    Some
      { frame_params = List.length fn.fn_params; frame_locals = locals_count };

  Label.Map.iter
    (fun _ bb ->
      bb.bb_insts <-
        List.fold_right
          (fun inst acc ->
            let load_insts, store_insts = handle_inst tmp_regs colors inst in
            load_insts @ (inst :: (store_insts @ acc)))
          bb.bb_insts [])
    fn.fn_blocks
