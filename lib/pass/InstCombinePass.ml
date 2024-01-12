open Ir

let pass_fn _ = ()

(*
let pass_fn fn =
  Label.Map.iter
    (fun _ bb ->
      iter_insts
        (fun inst ->
          let mov_or_cst o =
            match o with
            | Iop_imm i -> Iinst_unreachable
            | Iop_reg r -> Iinst_mov r
          in

          let new_kind =
            match inst.i_kind with
            | Iinst_binop (binop, o1, o2) -> (
                match (binop, o1, o2) with
                (* Addition *)
                | Ibinop_add, Iop_imm a, Iop_imm b -> Iinst_loadi (Z.add a b)
                | Ibinop_add, Iop_imm a, Iop_reg r when a = Z.zero ->
                    Iinst_mov r
                | Ibinop_add, Iop_reg r, Iop_imm b when b = Z.zero ->
                    Iinst_mov r
                (* Subtraction *)
                | Ibinop_sub, Iop_imm a, Iop_imm b -> Iinst_loadi (Z.sub a b)
                | Ibinop_sub, Iop_imm a, o2 when a = Z.zero ->
                    Iinst_unop (Iunop_neg, o2)
                | Ibinop_sub, Iop_reg r, Iop_imm b when b = Z.zero ->
                    Iinst_mov r
                (* Multiplication *)
                | Ibinop_mul, Iop_imm a, Iop_imm b -> Iinst_loadi (Z.mul a b)
                | Ibinop_mul, Iop_imm a, _ when a = Z.zero ->
                    Iinst_unop (Iunop_neg, o2)
                | Ibinop_mul, _, Iop_imm b when b = Z.zero -> mov_or_cst o2
                (* No simplification *)
                | _ -> None)
            | _ -> None
          in
          ())
        bb)
    fn.fn_blocks
*)
