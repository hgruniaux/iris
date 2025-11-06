open Tast
open LibIris

type value = VUnknown | VInt of Z.t | VFloat of float | VBool of bool

let bool_as_int b = if b then VInt Z.one else VInt Z.zero

let rec eval_constant_expr (expr : texpr) : value =
  match expr.texpr_kind with
  | Texpr_int v -> VInt v
  | Texpr_float v -> VFloat v
  | Texpr_bool v -> VBool v
  | Texpr_paren e -> eval_constant_expr e
  | Texpr_unary (unop, e) -> eval_constant_unary_expr unop e
  | Texpr_binary (binop, e1, e2) -> (
      let v1 = eval_constant_expr e1 in
      let v2 = eval_constant_expr e2 in
      match (binop.pbinop_kind, v1, v2) with
      | Pbinop_comma, v1, v2 when v1 <> VUnknown -> v2
      | _, VInt n1, VInt n2 -> eval_binary_expr_int binop n1 n2
      | _, VFloat f1, VFloat f2 -> eval_binary_expr_float binop f1 f2
      | _ -> VUnknown)
  | Texpr_sizeof ttype -> eval_sizeof_expr ttype
  | Texpr_alignof ttype -> eval_alignof_expr ttype
  | Texpr_cast (_, e, _) | Texpr_implicit_cast (_, e, _) ->
      (* TODO: Implement constant evaluation for casts *)
      eval_constant_expr e
  | _ -> VUnknown

and eval_constant_unary_expr (unop : unop) (expr : texpr) : value =
  match unop.punop_kind with
  | Punop_sizeof -> eval_sizeof_expr expr.texpr_type
  | _ -> (
      let v = eval_constant_expr expr in
      match (unop.punop_kind, v) with
      | Punop_neg, VInt n -> VInt (Z.neg n)
      | Punop_neg, VFloat f -> VFloat (-.f)
      | Punop_bitnot, VInt f -> VInt (Z.lognot f)
      | Punop_lognot, VBool b -> VBool (not b)
      | Punop_lognot, VInt v when v = Z.zero -> VInt Z.one
      | Punop_lognot, VInt _ -> VInt Z.zero
      | Punop_sizeof, _ -> assert false (* already handled *)
      | _ -> VUnknown)

and eval_sizeof_expr (ttype : ttype) : value =
  if Type.is_object ttype && Type.is_complete ttype then
    let ir_type = Codegen_type.codegen_type ttype in
    let size = Machine_info.size_of ir_type in
    VInt (Z.of_int size)
  else VUnknown

and eval_alignof_expr (ttype : ttype) : value =
  if Type.is_object ttype && Type.is_complete ttype then
    let ir_type = Codegen_type.codegen_type ttype in
    let align = Machine_info.align_of ir_type in
    VInt (Z.of_int align)
  else VUnknown

and eval_binary_expr_int op v1 v2 =
  match op.pbinop_kind with
  | Pbinop_add -> VInt (Z.add v1 v2)
  | Pbinop_sub -> VInt (Z.sub v1 v2)
  | Pbinop_mul -> VInt (Z.mul v1 v2)
  | Pbinop_div -> if Z.equal v2 Z.zero then VUnknown else VInt (Z.div v1 v2)
  | Pbinop_mod -> if Z.equal v2 Z.zero then VUnknown else VInt (Z.rem v1 v2)
  | Pbinop_shl -> VInt (Z.shift_left v1 (Z.to_int v2))
  | Pbinop_shr -> VInt (Z.shift_right v1 (Z.to_int v2))
  | Pbinop_or -> VInt (Z.logor v1 v2)
  | Pbinop_and -> VInt (Z.logand v1 v2)
  | Pbinop_xor -> VInt (Z.logxor v1 v2)
  | Pbinop_eq -> bool_as_int (v1 = v2)
  | Pbinop_ne -> bool_as_int (v1 <> v2)
  | Pbinop_lt -> bool_as_int (v1 < v2)
  | Pbinop_le -> bool_as_int (v1 <= v2)
  | Pbinop_gt -> bool_as_int (v1 > v2)
  | Pbinop_ge -> bool_as_int (v1 >= v2)
  | _ -> VUnknown

and eval_binary_expr_float op v1 v2 =
  match op.pbinop_kind with
  | Pbinop_add -> VFloat (v1 +. v2)
  | Pbinop_sub -> VFloat (v1 -. v2)
  | Pbinop_mul -> VFloat (v1 *. v2)
  | Pbinop_div -> if v2 = 0.0 then VUnknown else VFloat (v1 /. v2)
  | Pbinop_eq -> bool_as_int (v1 = v2)
  | Pbinop_ne -> bool_as_int (v1 <> v2)
  | Pbinop_lt -> bool_as_int (v1 < v2)
  | Pbinop_le -> bool_as_int (v1 <= v2)
  | Pbinop_gt -> bool_as_int (v1 > v2)
  | Pbinop_ge -> bool_as_int (v1 >= v2)
  | _ -> VUnknown
