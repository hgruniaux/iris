open Ir

let map_operands f inst =
  inst.i_kind <-
    (match inst.i_kind with
    | Iinst_binop (op, lhs, rhs) -> Iinst_binop (op, f lhs, f rhs)
    | Iinst_unop (op, value) -> Iinst_unop (op, f value)
    | Iinst_cmp (cmp, lhs, rhs) -> Iinst_cmp (cmp, f lhs, f rhs)
    | Iinst_call (fn, args) -> Iinst_call (fn, List.map f args)
    | Iinst_phi operands ->
        Iinst_phi (List.map (fun (o, label) -> (f o, label)) operands)
    | _ -> inst.i_kind)

let map_term_operands f term =
  match term with
  | Iterm_jmpc (cond, tl, te) -> Iterm_jmpc (f cond, tl, te)
  | Iterm_retv value -> Iterm_retv (f value)
  | Iterm_jmp _ | Iterm_ret | Iterm_unreachable -> term

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

    This also works for PHI instructions:
        %2 = phi [(%1, L1)]
        ret %2
    Will be simplified to:
        %2 = phi [(%1, L1)] ; no more used, will be removed by DCE
        ret %1

    A dead code elimination pass should be run after this.
*)
let pass_fn am fn =
  (* FIXME: should iterate over the dominator tree so we can propagate across basic blocks. *)
  Label.Map.iter
    (fun _ bb ->
      let ht = Hashtbl.create 17 in

      let update_operand op =
        match op with
        | Iop_imm _ -> op
        | Iop_reg r -> (
            match Hashtbl.find_opt ht r with
            | None -> op
            | Some new_op ->
                AnalysisManager.mark_as_dirty am;
                new_op)
      in

      let handle_inst inst =
        let r1 = inst.i_name in
        match inst.i_kind with
        | Iinst_cst cst -> (
            match Hashtbl.find_opt fn.fn_ctx.ctx_constants cst with
            | Some (Icst_int i) -> Hashtbl.add ht r1 (Iop_imm i)
            | Some (Icst_string _)
            | Some (Icst_function _)
            | Some (Icst_struct _)
            | None ->
                ())
        | Iinst_loadi i -> Hashtbl.add ht r1 (Iop_imm i)
        | Iinst_mov r2 -> (
            match Hashtbl.find_opt ht r2 with
            | None -> Hashtbl.add ht r1 (Iop_reg r2)
            | Some op -> Hashtbl.add ht r1 op)
        | Iinst_phi operands when List.compare_length_with operands 1 = 0 ->
            let o, _ = List.hd operands in
            Hashtbl.add ht r1 o;
            map_operands update_operand inst
        | _ -> map_operands update_operand inst
      in

      List.iter handle_inst bb.b_phi_insts;
      List.iter handle_inst bb.b_insts;
      bb.b_term <- map_term_operands update_operand bb.b_term)
    fn.fn_blocks;

  AnalysisManager.keep_cfg am;
  am.am_dirty
