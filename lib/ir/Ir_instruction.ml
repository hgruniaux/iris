open Ir_base
module Function = Ir_function
module Value = Ir_value
module ValueSet = Set.Make (Value)

type t = inst

let type_of = function
  | Iinst_value v -> Value.type_of v
  | Iinst_alloca _ -> Ityp_ptr
  | Iinst_load (typ, _) -> typ
  | Iinst_store _ -> Ityp_unit
  | Iinst_ibinop (_, v1, _) -> Value.type_of v1
  | Iinst_iunop (_, v) -> Value.type_of v
  | Iinst_icmp _ -> Ityp_i1
  | Iinst_cast (_, typ, _) -> typ
  | Iinst_call (fn, _) -> (
      match Value.type_of fn with
      | Ityp_func (_, ret_type, _) -> ret_type
      | _ -> assert false)
  | Iinst_phi operands ->
      let v, _ = List.hd operands in
      Value.type_of v

(** Creates an instruction of the given [kind] and [name] that is part of [fn].
    Note that the instruction is not yet inserted into one of the function's
    basic blocks. *)
let create fn name kind =
  let inst = { i_name = name; i_kind = kind } in
  Hashtbl.add fn.fn_symbol_table name inst;
  inst

(** Returns true if [inst_kind] may have observable side effects. *)
let may_have_side_effects inst_kind =
  match inst_kind with
  | Iinst_value _ | Iinst_phi _ | Iinst_ibinop _ | Iinst_iunop _ | Iinst_icmp _
  | Iinst_cast _ ->
      false
  | Iinst_alloca _ -> true
  | Iinst_load _ -> false (* Reading from memory has no side effects. *)
  | Iinst_store _ -> true (* But writting to memory, yes. *)
  (* A call to a function may have side effects. However, in some cases, we can
     prove that the callee function is pure (has no side effets). *)
  | Iinst_call _ -> true (* TODO: support pure functions for side effects *)

(** Inserts a phi node with the given [operands] into the given [bb]. *)
let insert_phi fn bb operands =
  assert (operands <> []);
  assert (is_bb_from bb fn);
  let name =
    Function.fresh_register fn (Value.type_of (fst (List.hd operands)))
  in
  let inst = create fn name (Iinst_phi operands) in
  (* Insert the PHI node in [bb]. *)
  bb.b_phi_insts <- inst :: bb.b_phi_insts;
  Ival_reg name

(** Returns the set of values used by the given instruction. *)
let uses inst =
  match inst.i_kind with
  | Iinst_value v -> ValueSet.singleton v
  | Iinst_alloca _ -> ValueSet.empty
  | Iinst_load (_, addr) -> ValueSet.singleton addr
  | Iinst_store (addr, value) -> ValueSet.of_list [ addr; value ]
  | Iinst_ibinop (_, v1, v2) -> ValueSet.of_list [ v1; v2 ]
  | Iinst_iunop (_, v) -> ValueSet.singleton v
  | Iinst_icmp (_, v1, v2) -> ValueSet.of_list [ v1; v2 ]
  | Iinst_cast (_, _, v) -> ValueSet.singleton v
  | Iinst_call (_, args) ->
      List.fold_left (fun acc v -> ValueSet.add v acc) ValueSet.empty args
  | Iinst_phi operands ->
      List.fold_left
        (fun acc (v, _) -> ValueSet.add v acc)
        ValueSet.empty operands

(** Returns the set of registers used by the given instruction. *)
let uses_reg inst =
  let used_values = uses inst in
  ValueSet.fold
    (fun v acc -> match v with Ival_reg r -> RegSet.add r acc | _ -> acc)
    used_values RegSet.empty

(** Returns the set of values defined by the given instruction. *)
let defs inst = ValueSet.singleton (Ival_reg inst.i_name)

(** Returns the set of registers defined by the given instruction. *)
let defs_reg inst = RegSet.singleton inst.i_name
