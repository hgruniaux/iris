open Ir

type t = Ir.inst

(** Creates an instruction of the given [kind] and [name] that is part of [fn].
    Note that the instruction is not yet inserted into one of the function's
    basic blocks. *)
let create fn name kind =
  let inst =
    { i_name = name; i_kind = kind; i_typ = compute_inst_type fn kind }
  in
  Hashtbl.add fn.fn_symbol_table name inst;
  inst

(** Returns true if [inst_kind] may have observable side effects. *)
let may_have_side_effects inst_kind =
  match inst_kind with
  | Iinst_cst _ | Iinst_loadi _ -> false
  | Iinst_load _ | Iinst_loadfield _ ->
      false (* Reading from memory has no side effects. *)
  | Iinst_store _ -> true (* But writting to memory, yes. *)
  (* Moves and PHI instructions do not have any side effects. *)
  | Iinst_mov _ -> false
  | Iinst_phi _ -> false
  (* Arithmetic/logical/comparison instructions do not have any side effects. *)
  | Iinst_binop _ | Iinst_unop _ | Iinst_cmp _ -> false
  (* A call to a function may have side effects. However, in some cases, we can
     prove that the callee function is pure (has no side effets). *)
  | Iinst_call _ -> true (* TODO: support pure functions for side effects *)

(** Inserts a phi node with the given [operands] into the given [bb]. *)
let insert_phi fn bb operands =
  assert (operands <> []);
  assert (is_bb_from bb fn);
  let name = Reg.fresh () in
  let inst = create fn name (Iinst_phi operands) in
  (* Insert the PHI node in [bb]. *)
  bb.b_phi_insts <- inst :: bb.b_phi_insts;
  name
