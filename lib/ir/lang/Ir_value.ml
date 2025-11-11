open Ir_base

type t = value

let equal v1 v2 =
  match (v1, v2) with
  | Ival_reg r1, Ival_reg r2 -> Reg.equal r1 r2
  | Ival_int (t1, i1), Ival_int (t2, i2) -> t1 = t2 && Z.equal i1 i2
  | Ival_float (t1, f1), Ival_float (t2, f2) -> t1 = t2 && f1 = f2
  | Ival_global g1, Ival_global g2 -> g1 = g2
  | _ -> false

let compare v1 v2 =
  match (v1, v2) with
  | Ival_reg r1, Ival_reg r2 -> Reg.compare r1 r2
  | Ival_reg _, _ -> -1
  | _, Ival_reg _ -> 1
  | Ival_global g1, Ival_global g2 -> Stdlib.compare g1 g2
  | Ival_global _, _ -> -1
  | _, Ival_global _ -> 1
  | Ival_int (t1, i1), Ival_int (t2, i2) ->
      let c = Stdlib.compare t1 t2 in
      if c <> 0 then c else Z.compare i1 i2
  | Ival_int _, _ -> -1
  | _, Ival_int _ -> 1
  | Ival_float (t1, f1), Ival_float (t2, f2) ->
      let c = Stdlib.compare t1 t2 in
      if c <> 0 then c else Stdlib.compare f1 f2

(** Sets the debug name of a value (if it is a register). *)
let set_name v name = match v with Ival_reg r -> Reg.set_name r name | _ -> ()

(** Gets the type of a value. *)
let type_of = function
  | Ival_reg r -> Reg.type_of r
  | Ival_int (typ, _) -> typ
  | Ival_float (typ, _) -> typ
  | Ival_global g -> (
      match g.global_kind with
      | Iglobal_variable _ -> Ityp_ptr
      | Iglobal_function f -> f.fn_type)

(** Maps the register inside a value using [f]. *)
let map_reg f = function Ival_reg r -> Ival_reg (f r) | v -> v

(** If the value is a virtual register, returns it. Otherwise, returns None. *)
let extract_reg v = match v with Ival_reg r -> Some r | _ -> None

(** If the value is a virtual register, returns a singleton set containing it.
    Otherwise, returns an empty set. *)
let as_regset v =
  match v with Ival_reg r -> RegSet.singleton r | _ -> RegSet.empty
