open Ir

let map_operands f inst =
  Ir.update_uses inst ~remove:true;
  inst.i_kind <-
    (match inst.i_kind with
    | Iinst_binop (op, lhs, rhs) -> Iinst_binop (op, f lhs, f rhs)
    | Iinst_unop (op, value) -> Iinst_unop (op, f value)
    | Iinst_cmp (cmp, lhs, rhs) -> Iinst_cmp (cmp, f lhs, f rhs)
    | _ -> inst.i_kind);
  Ir.update_uses inst ~remove:false

(** This pass propagates moves and constants to later operands.
    So, for example:
        ...
        %2 = 42
        %3 = add %1 %2
        %4 = %3
        ret %4
    Will be simplified to:
        ...
        %2 = 42         ; no more used, will be removed by DCE
        %3 = add %1 42
        %4 = %3         ; no more used, will be removed by DCE
        ret %3

    A dead code elimination pass should be run after this.
*)
let pass_fn fn =
  let ht = Hashtbl.create 17 in

  let update_operand op =
    match op with
    | Iop_imm _ -> op
    | Iop_reg r -> (
        match Hashtbl.find_opt ht r with None -> op | Some new_op -> new_op)
  in

  Label.Map.iter
    (fun _ bb ->
      iter_insts
        (fun inst ->
          let r1 = inst.i_name in
          match inst.i_kind with
          | Iinst_cst cst -> (
              match Hashtbl.find_opt fn.fn_ctx.ctx_constants cst with
              | Some (Icst_int i) -> Hashtbl.add ht r1 (Iop_imm i)
              | Some (Icst_string _) | None -> ())
          | Iinst_mov r2 -> (
              match Hashtbl.find_opt ht r2 with
              | None -> Hashtbl.add ht r1 (Iop_reg r2)
              | Some op -> Hashtbl.add ht r1 op)
          | _ -> map_operands update_operand inst)
        bb)
    fn.fn_blocks
