open Ir_base
module Function = Ir_function
module Value = Ir_value
module ValueSet = Set.Make (Value)

type t = inst

let type_of = function
  | Iexpr_value v -> Value.type_of v
  | Iexpr_alloca _ -> Ityp_ptr
  | Iexpr_load (typ, _) -> typ
  | Iexpr_ibinop (_, v1, _) -> Value.type_of v1
  | Iexpr_iunop (_, v) -> Value.type_of v
  | Iexpr_icmp _ -> Ityp_i1
  | Iexpr_cast (_, typ, _) -> typ
  | Iexpr_call (fn, _) -> (
      match Value.type_of fn with
      | Ityp_func (_, ret_type, _) -> ret_type
      | _ -> assert false)

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
  | Iexpr_value _ | Iexpr_ibinop _ | Iexpr_iunop _ | Iexpr_icmp _ | Iexpr_cast _
    ->
      false
  | Iexpr_alloca _ -> true
  | Iexpr_load _ -> false (* Reading from memory has no side effects. *)
  (* A call to a function may have side effects. However, in some cases, we can
     prove that the callee function is pure (has no side effets). *)
  | Iexpr_call _ -> true (* TODO: support pure functions for side effects *)

(** Returns the set of values used by the given instruction. *)
let uses inst =
  let uses_expr = function
    | Iexpr_value v -> ValueSet.singleton v
    | Iexpr_alloca _ -> ValueSet.empty
    | Iexpr_load (_, addr) -> ValueSet.singleton addr
    | Iexpr_ibinop (_, v1, v2) -> ValueSet.of_list [ v1; v2 ]
    | Iexpr_iunop (_, v) -> ValueSet.singleton v
    | Iexpr_icmp (_, v1, v2) -> ValueSet.of_list [ v1; v2 ]
    | Iexpr_cast (_, _, v) -> ValueSet.singleton v
    | Iexpr_call (_, args) ->
        List.fold_left (fun acc v -> ValueSet.add v acc) ValueSet.empty args
  in
  match inst with
  | Iinst_def (_, expr) -> uses_expr expr
  | Iinst_store (addr, value) -> ValueSet.of_list [ addr; value ]

(** Returns the set of registers used by the given instruction. *)
let uses_reg inst =
  let used_values = uses inst in
  ValueSet.fold
    (fun v acc -> match v with Ival_reg r -> RegSet.add r acc | _ -> acc)
    used_values RegSet.empty

(** Returns the set of values defined by the given instruction. *)
let defs inst =
  match inst with
  | Iinst_def (v, _) -> ValueSet.singleton (Ival_reg v)
  | Iinst_store _ -> ValueSet.empty

(** Returns the set of registers defined by the given instruction. *)
let defs_reg inst =
  match inst with
  | Iinst_def (v, _) -> RegSet.singleton v
  | Iinst_store _ -> RegSet.empty
