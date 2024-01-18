open Ir

let is_power_of_2 n = Z.popcount n = 1

let instcombine_add o1 o2 =
  match (o1, o2) with
  (* Constant folding *)
  | Iop_imm a, Iop_imm b -> Some (Iinst_loadi (Z.add a b))
  (* x + 0 = 0 + x = x *)
  | (Iop_reg x, Iop_imm a | Iop_imm a, Iop_reg x) when a = Z.zero ->
      Some (Iinst_mov x)
  (* x + x = x << 1 *)
  | Iop_reg x, Iop_reg y when x = y ->
      Some (Iinst_binop (Ibinop_lsl, o1, Iop_imm Z.one))
  (* Reorder operands because addition is commutative *)
  | Iop_imm _, Iop_reg _ -> Some (Iinst_binop (Ibinop_add, o2, o1))
  (* No simplification found *)
  | _ -> None

let instcombine_sub o1 o2 =
  match (o1, o2) with
  (* Constant folding *)
  | Iop_imm a, Iop_imm b -> Some (Iinst_loadi (Z.sub a b))
  (* x - 0 = x *)
  | Iop_reg x, Iop_imm a when a = Z.zero -> Some (Iinst_mov x)
  (* 0 - x = -x *)
  | Iop_imm a, _ when a = Z.zero -> Some (Iinst_unop (Iunop_neg, o2))
  (* x - x = 0 *)
  | Iop_reg x, Iop_reg y when x = y -> Some (Iinst_loadi Z.zero)
  (* No simplification found *)
  | _ -> None

let instcombine_mul o1 o2 =
  match (o1, o2) with
  (* Constant folding *)
  | Iop_imm a, Iop_imm b -> Some (Iinst_loadi (Z.mul a b))
  (* x * 0 = 0 * x = 0 *)
  | (_, Iop_imm a | Iop_imm a, _) when a = Z.zero -> Some (Iinst_loadi Z.zero)
  (* x * 1 = 1 * x = x *)
  | (Iop_reg x, Iop_imm a | Iop_imm a, Iop_reg x) when a = Z.one ->
      Some (Iinst_mov x)
  (* x * 2^k = 2^k * x = x << k *)
  | (x, Iop_imm a | Iop_imm a, x) when is_power_of_2 a ->
      let k = Z.of_int (Z.log2 a) in
      Some (Iinst_binop (Ibinop_lsl, x, Iop_imm k))
  (* Reorder operands because multiplication is commutative *)
  | Iop_imm _, Iop_reg _ -> Some (Iinst_binop (Ibinop_mul, o2, o1))
  (* No simplification found *)
  | _ -> None

let instcombine_div ~is_signed o1 o2 =
  match (o1, o2) with
  (* x / 0 = undef *)
  | _, Iop_imm a when a = Z.zero -> None
  (* Constant folding *)
  | Iop_imm a, Iop_imm b -> Some (Iinst_loadi (Z.div a b))
  (* x / 1 = x *)
  | Iop_reg x, Iop_imm a when a = Z.one -> Some (Iinst_mov x)
  (* x / x = 1 *)
  | Iop_reg x, Iop_reg y when x = y -> Some (Iinst_loadi Z.one)
  (* x / 2^k = x >> k *)
  | x, Iop_imm a when is_power_of_2 a ->
      let k = Z.of_int (Z.log2 a) in
      let shift_kind = if is_signed then Ibinop_asr else Ibinop_lsr in
      Some (Iinst_binop (shift_kind, x, Iop_imm k))
  (* No simplification found *)
  | _ -> None

let instcombine_rem ~is_signed o1 o2 =
  match (o1, o2) with
  (* x mod 0 = undef *)
  | _, Iop_imm a when a = Z.zero -> None
  (* Constant folding *)
  | Iop_imm a, Iop_imm b -> Some (Iinst_loadi (Z.rem a b))
  (* x mod 1 = 0 *)
  | Iop_reg _, Iop_imm a when a = Z.one -> Some (Iinst_loadi Z.zero)
  (* x mod x = 0 *)
  | Iop_reg x, Iop_reg y when x = y -> Some (Iinst_loadi Z.zero)
  (* x mod 2^k = x & ((1 << k) - 1) (if the value is unsigned or positive) *)
  | x, Iop_imm a when is_power_of_2 a && not is_signed ->
      (* See https://graphics.stanford.edu/~seander/bithacks.html#ModulusDivisionEasy *)
      let mask = Z.(a - Z.one) in
      Some (Iinst_binop (Ibinop_and, x, Iop_imm mask))
  (* No simplification found *)
  | _ -> None

let instcombine_and o1 o2 =
  match (o1, o2) with
  (* Constant folding *)
  | Iop_imm a, Iop_imm b -> Some (Iinst_loadi (Z.logand a b))
  (* x & 0 = 0 *)
  | _, Iop_imm a when a = Z.zero -> Some (Iinst_loadi Z.zero)
  (* x & x = x *)
  | Iop_reg x, Iop_reg y when x = y -> Some (Iinst_mov x)
  (* No simplification found *)
  | _ -> None

let instcombine_or o1 o2 =
  match (o1, o2) with
  (* Constant folding *)
  | Iop_imm a, Iop_imm b -> Some (Iinst_loadi (Z.logor a b))
  (* x | 0 = x *)
  | Iop_reg x, Iop_imm a when a = Z.zero -> Some (Iinst_mov x)
  (* x | x = x *)
  | Iop_reg x, Iop_reg y when x = y -> Some (Iinst_mov x)
  (* No simplification found *)
  | _ -> None

let instcombine_xor o1 o2 =
  match (o1, o2) with
  (* Constant folding *)
  | Iop_imm a, Iop_imm b -> Some (Iinst_loadi (Z.logxor a b))
  (* x ^ 0 = x *)
  | Iop_reg x, Iop_imm a when a = Z.zero -> Some (Iinst_mov x)
  (* x ^ x = 0 *)
  | Iop_reg x, Iop_reg y when x = y -> Some (Iinst_loadi Z.zero)
  (* No simplification found *)
  | _ -> None

(* TODO: Instcombine for binops shifts *)

let instcombine_binop binop o1 o2 =
  match binop with
  | Ibinop_add -> instcombine_add o1 o2
  | Ibinop_sub -> instcombine_sub o1 o2
  | Ibinop_mul -> instcombine_mul o1 o2
  | Ibinop_sdiv -> instcombine_div ~is_signed:true o1 o2
  | Ibinop_udiv -> instcombine_div ~is_signed:false o1 o2
  | Ibinop_srem -> instcombine_rem ~is_signed:true o1 o2
  | Ibinop_urem -> instcombine_rem ~is_signed:false o1 o2
  | Ibinop_and -> instcombine_and o1 o2
  | Ibinop_or -> instcombine_or o1 o2
  | Ibinop_xor -> instcombine_xor o1 o2
  (* No simplification *)
  | _ -> None

let instcombine_not o =
  match o with
  (* Constant folding *)
  | Iop_imm a -> Some (Iinst_loadi (Z.lognot a))
  (* No simplification *)
  | _ -> None

let instcombine_neg o =
  match o with
  (* Constant folding *)
  | Iop_imm a -> Some (Iinst_loadi (Z.neg a))
  (* No simplification *)
  | _ -> None

let instcombine_unop unop o =
  match unop with
  | Iunop_not -> instcombine_not o
  | Iunop_neg -> instcombine_neg o

(** Instcombine for equal to. *)
let instcombine_eq o1 o2 =
  match (o1, o2) with
  (* Constant folding *)
  | Iop_imm a, Iop_imm b -> Some (Iinst_loadi (Z.of_int (Bool.to_int (a = b))))
  (* Reorder operands: imm == x <=> x == imm *)
  | Iop_imm _, Iop_reg _ -> Some (Iinst_cmp (Icmp_eq, o2, o1))
  (* No simplification found *)
  | _ -> None

(** Instcombine for not equal to. *)
let instcombine_ne o1 o2 =
  match (o1, o2) with
  (* Constant folding *)
  | Iop_imm a, Iop_imm b -> Some (Iinst_loadi (Z.of_int (Bool.to_int (a <> b))))
  (* Reorder operands: imm != x <=> x != imm *)
  | Iop_imm _, Iop_reg _ -> Some (Iinst_cmp (Icmp_ne, o2, o1))
  (* No simplification found *)
  | _ -> None

(** Instcombine for signed less than. *)
let instcombine_slt o1 o2 =
  match (o1, o2) with
  (* Constant folding *)
  | Iop_imm a, Iop_imm b -> Some (Iinst_loadi (Z.of_int (Bool.to_int (a < b))))
  (* Reorder operands: imm < x <=> x > imm *)
  | Iop_imm _, Iop_reg _ -> Some (Iinst_cmp (Icmp_sgt, o2, o1))
  (* No simplification found *)
  | _ -> None

(** Instcombine for signed less than or equal to. *)
let instcombine_sle o1 o2 =
  match (o1, o2) with
  (* Constant folding *)
  | Iop_imm a, Iop_imm b -> Some (Iinst_loadi (Z.of_int (Bool.to_int (a <= b))))
  (* Reorder operands: imm <= x <=> x >= imm *)
  | Iop_imm _, Iop_reg _ -> Some (Iinst_cmp (Icmp_sge, o2, o1))
  (* No simplification found *)
  | _ -> None

(** Instcombine for signed greater than. *)
let instcombine_sgt o1 o2 =
  match (o1, o2) with
  (* Constant folding *)
  | Iop_imm a, Iop_imm b -> Some (Iinst_loadi (Z.of_int (Bool.to_int (a > b))))
  (* Reorder operands: imm > x <=> x < imm *)
  | Iop_imm _, Iop_reg _ -> Some (Iinst_cmp (Icmp_slt, o2, o1))
  (* No simplification found *)
  | _ -> None

(** Instcombine for signed greather than or equal to. *)
let instcombine_sge o1 o2 =
  match (o1, o2) with
  (* Constant folding *)
  | Iop_imm a, Iop_imm b -> Some (Iinst_loadi (Z.of_int (Bool.to_int (a >= b))))
  (* Reorder operands: imm >= x <=> x <= imm *)
  | Iop_imm _, Iop_reg _ -> Some (Iinst_cmp (Icmp_sle, o2, o1))
  (* No simplification found *)
  | _ -> None

(* TODO: Instcombine for comparisons ult, ule, ugt, uge *)

let instcombine_cmp cmp o1 o2 =
  match cmp with
  | Icmp_eq -> instcombine_eq o1 o2
  | Icmp_ne -> instcombine_ne o1 o2
  | Icmp_slt -> instcombine_slt o1 o2
  | Icmp_sle -> instcombine_sle o1 o2
  | Icmp_sgt -> instcombine_sgt o1 o2
  | Icmp_sge -> instcombine_sge o1 o2
  | _ -> None

let instcombine_jmpc cond then_l else_l =
  match cond with
  (* False condition *)
  | Iop_imm a when a = Z.zero -> Some (Iterm_jmp else_l)
  (* True condition *)
  | Iop_imm _ -> Some (Iterm_jmp then_l)
  (* No simplification found *)
  | _ -> None

let update_cfg fn bb =
  (* Remove bb from its old successors *)
  Reg.Set.iter
    (fun succ_label ->
      let succ = Reg.Map.find succ_label fn.fn_blocks in
      succ.b_predecessors <- Reg.Set.remove bb.b_label succ.b_predecessors)
    bb.b_successors;

  (* Then re-add with the new successors. *)
  bb.b_successors <- Terminator.successors_of bb.b_term;
  Reg.Set.iter
    (fun succ_label ->
      let succ = Reg.Map.find succ_label fn.fn_blocks in
      succ.b_predecessors <- Reg.Set.add bb.b_label succ.b_predecessors)
    bb.b_successors

let pass_fn am fn =
  Label.Map.iter
    (fun _ bb ->
      let handle_inst inst =
        let result =
          match inst.i_kind with
          | Iinst_binop (binop, o1, o2) -> instcombine_binop binop o1 o2
          | Iinst_unop (unop, o) -> instcombine_unop unop o
          | Iinst_cmp (cmp, o1, o2) -> instcombine_cmp cmp o1 o2
          | _ -> None
        in

        inst.i_kind <-
          (match result with
          | Some kind ->
              AnalysisManager.mark_as_dirty am;
              kind
          | _ -> inst.i_kind)
      in

      List.iter handle_inst bb.b_phi_insts;
      List.iter handle_inst bb.b_insts;

      let result =
        match bb.b_term with
        | Iterm_jmpc (cond, tl, el) -> instcombine_jmpc cond tl el
        | _ -> None
      in

      (match result with
      | Some term ->
          AnalysisManager.mark_as_dirty am;
          bb.b_term <- term
      | _ -> ());

      update_cfg fn bb)
    fn.fn_blocks;

  AnalysisManager.keep_cfg am;
  am.am_dirty
