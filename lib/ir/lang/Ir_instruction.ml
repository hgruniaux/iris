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

(** Returns true if [inst] may have observable side effects. *)
let may_have_side_effects inst =
  match inst with
  | Iinst_def (_, expr) -> (
      match expr with
      | Iexpr_value _ -> false
      | Iexpr_alloca _ -> false
      | Iexpr_ibinop _ -> false
      | Iexpr_iunop _ -> false
      | Iexpr_icmp _ -> false
      | Iexpr_cast _ -> false
      | Iexpr_call _ -> true
      | Iexpr_load _ -> true)
  | Iinst_store _ -> true

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

let map_expr_values f expr =
  match expr with
  | Iexpr_value v -> Iexpr_value (f v)
  | Iexpr_alloca _ -> expr
  | Iexpr_load (typ, addr) -> Iexpr_load (typ, f addr)
  | Iexpr_ibinop (op, v1, v2) -> Iexpr_ibinop (op, f v1, f v2)
  | Iexpr_iunop (op, v) -> Iexpr_iunop (op, f v)
  | Iexpr_icmp (cmp, v1, v2) -> Iexpr_icmp (cmp, f v1, f v2)
  | Iexpr_cast (cast_kind, typ, v) -> Iexpr_cast (cast_kind, typ, f v)
  | Iexpr_call (fn, args) -> Iexpr_call (f fn, List.map f args)

(** Applies [f] to all values used in [inst], returning a new instruction with
    the transformed values. The defined name for [Iinst_def] is not changed. *)
let map_values f inst =
  match inst with
  | Iinst_def (name, expr) -> Iinst_def (name, map_expr_values f expr)
  | Iinst_store (addr, value) -> Iinst_store (f addr, f value)

(** Same as [map_values f_val inst] but also applies [f_reg] to the instruction
    name of [Iinst_def]. *)
let map_values_and_def f_val f_reg inst =
  match inst with
  | Iinst_def (name, expr) -> Iinst_def (f_reg name, map_expr_values f_val expr)
  | Iinst_store (addr, value) -> Iinst_store (f_val addr, f_val value)

(** Applies [f] to all registers used in [inst], returning a new instruction
    with the transformed registers. *)
let map_regs f inst =
  let map_value v = match v with Ival_reg r -> Ival_reg (f r) | _ -> v in
  match inst with
  | Iinst_def (name, expr) ->
      let new_expr = map_expr_values map_value expr in
      Iinst_def (f name, new_expr)
  | _ -> map_values map_value inst
