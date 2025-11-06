(* Copyright (C) 2025 Hubert Gruniaux
 * This file is part of Iris. *)

open Tast
open Type
open Typing_common

(** Check if two types are compatible as defined in C23 6.2.7. *)
let rec are_compatible t1 t2 =
  (* C23 6.2.7 Compatible type and composite type *)
  let t1 = head t1 in
  let t2 = head t2 in

  match (t1, t2) with
  (* FIXME: handle qualifiers *)
  (* C23 6.7.7.2 §2
     For two pointer types to be compatible, both shall be identically
     qualified and both shall be pointers to compatible types. *)
  | Ttyp_ptr t1, Ttyp_ptr t2 ->
      are_compatible t1 t2 (* FIXME: handle qualifiers *)
  (* C23 6.7.7.3 §6
     For two array types to be compatible, both shall have compatible element
     types, and if both size specifiers are present, and are integer constant
     expressions, then both size specifiers shall have the same constant value. *)
  | Ttyp_array (t1, Some sz1), Ttyp_array (t2, Some sz2) ->
      are_compatible t1 t2 && sz1 = sz2
  | Ttyp_array (t1, _), Ttyp_array (t2, _) -> are_compatible t1 t2
  (* C23 6.7.3.3 §16
     The enumerated type is compatible with the underlying type of the enumeration. *)
  | Ttyp_enum e1, t2 when t2 = e1.tenum_underlying_type -> true
  | t1, Ttyp_enum e2 when t1 = e2.tenum_underlying_type -> true
  (* C23 6.7.7.4 §14
     For two function types to be compatible, both shall specify compatible
     return types. Moreover, the parameter type lists shall agree in the
     number of parameters and in use of the final ellipsis; corresponding
     parameters shall have compatible types. In the determination of type
     compatibility and of a composite type, each parameter declared with
     function or array type is taken as having the adjusted type and each
     parameter declared with qualified type is taken as having the unqualified
     version of its declared type. *)
  | ( Ttyp_function (ret1, params1, vararg1),
      Ttyp_function (ret2, params2, vararg2) ) ->
      (* FIXME: handle qualifiers *)
      List.length params1 = List.length params2
      && List.for_all2 are_compatible params1 params2
      && are_compatible ret1 ret2 && vararg1 = vararg2
  (* C23 6.2.7 §1
     Two types are compatible types if they are the same. *)
  | _ when t1 = t2 -> true
  (* Otherwise, *)
  | _ -> false

(** Create a composite type from two compatible types as specified in C23 6.2.7.
    It is assumed that [t1] and [t2] are compatible. *)
let rec composite_type_from t1 t2 =
  (* C23 6.2.7 Compatible type and composite type
     We follow the algorithm described in §3.
     Most comments below are direct citations from this paragraph. *)
  let t1 = head t1 in
  let t2 = head t2 in

  assert (are_compatible t1 t2);

  match (t1, t2) with
  | Ttyp_struct _, Ttyp_struct _ ->
      failwith "struct composite type not implemented"
  (* If one type is an array of known constant size,
     the composite type is an array of that size. *)
  | Ttyp_array (t1, Some sz1), Ttyp_array (t2, _) ->
      Ttyp_array (composite_type_from t1 t2, Some sz1)
  | Ttyp_array (t1, _), Ttyp_array (t2, Some sz2) ->
      Ttyp_array (composite_type_from t1 t2, Some sz2)
  (* If both types are arrays of unknown size,
     the composite type is an array of unknown size. *)
  | Ttyp_array (t1, _), Ttyp_array (t2, _) ->
      Ttyp_array (composite_type_from t1 t2, None)
  (* If both types are function types, the type of each parameter in the composite
     parameter type list is the composite type of the corresponding parameters. *)
  | ( Ttyp_function (ret1, params1, vararg1),
      Ttyp_function (ret2, params2, vararg2) ) ->
      (* The following assert is guaranteed because both types must be compatible. *)
      assert (vararg1 = vararg2);
      let ret_type = composite_type_from ret1 ret2 in
      let param_types = List.map2 composite_type_from params1 params2 in
      Ttyp_function (ret_type, param_types, vararg1)
  (* If both types are the same type, the composite type is this type. *)
  | _ when t1 = t2 -> t1
  | _ -> failwith "failed to create composite type"

(** Check if a value of type [from_type] can be implicitly cast to [to_type] as
    if by assignment. It returns the cast kind or [Tcast_invalid] if the cast is
    not allowed. *)
let can_implicitly_cast ~from_type ~to_type =
  let from_type = head from_type in
  let to_type = head to_type in

  if are_compatible from_type to_type then Tcast_noop
  else if is_integer from_type then (* also handle from_type=bool *)
    (* C23 6.3.1.2 Boolean type
       C23 6.3.1.3 Signed and unsigned integers
       C23 6.3.1.4 Real floating and integer *)
    if is_bool to_type then Tcast_int2bool
    else if is_integer to_type then Tcast_int2int
    else if is_floating_point to_type then Tcast_int2float
    else Tcast_invalid
  else if is_floating_point from_type then
    (* C23 6.3.1.2 Boolean type
       C23 6.3.1.4 Real floating and integer
       C23 6.3.1.5 Real floating types *)
    if is_bool to_type then Tcast_float2bool
    else if is_floating_point to_type then Tcast_float2float
    else if is_integer to_type then Tcast_float2int
    else Tcast_invalid
  else if is_pointer from_type then
    (* C23 6.3.1.2 Boolean type
       C23 6.3.2.3 §1
       A pointer to void may be converted to or from a pointer to any object type.
       A pointer to any object type may be converted to a pointer to void and back again. *)
    if is_bool to_type then Tcast_pointer2bool
    else if is_pointer_to_void from_type && is_pointer_to_object to_type then
      Tcast_pointer2pointer
    else if is_pointer_to_object from_type && is_pointer_to_void to_type then
      Tcast_pointer2pointer
    else Tcast_invalid
  else Tcast_invalid

(** Check if [expr] can be implicitly cast to [to_type]. This is not just a
    shorthand for [can_implicitly_cast], as it also handle the case where [expr]
    is the 0 integer constant (and therefore can be casted to any pointer). *)
let can_implicitly_cast_expr expr to_type =
  let expr_without_parens = ignore_parens expr in
  match expr_without_parens.texpr_kind with
  | Texpr_int z when Z.equal z Z.zero && is_pointer_to_function to_type ->
      Tcast_null2function
  | Texpr_int z when Z.equal z Z.zero && is_pointer to_type ->
      Tcast_null2pointer
  | _ -> can_implicitly_cast ~from_type:expr.texpr_type ~to_type

(** Check if a value of type [from_type] can be cast to [to_type] using an
    explicit cast operator. It returns the cast kind or [Tcast_invalid] if the
    cast is not allowed. *)
let can_explicitly_cast ~from_type ~to_type =
  let from_type = head from_type in
  let to_type = head to_type in

  match can_implicitly_cast ~from_type ~to_type with
  | Tcast_invalid ->
      if is_void to_type then Tcast_void
      else if is_pointer from_type && is_integer to_type then Tcast_pointer2int
      else if is_integer from_type && is_pointer to_type then Tcast_int2pointer
      else if is_pointer_to_object from_type && is_pointer_to_object to_type
      then Tcast_pointer2pointer
      else if is_pointer_to_function from_type && is_pointer_to_function to_type
      then Tcast_pointer2pointer
      else Tcast_invalid
  | kind ->
      (* If we can implicitly cast, we can also explicitly cast. *)
      kind

(** Check if [expr] can be cast to [to_type] using an explicit cast operator.
    This is not just a shorthand for [can_explicitly_cast], as it also handle
    the case where [expr] is the 0 integer constant (and therefore can be casted
    to any pointer). *)
let can_explicitly_cast_expr expr to_type =
  let expr_without_parens = ignore_parens expr in
  match expr_without_parens.texpr_kind with
  | Texpr_int z when Z.equal z Z.zero && is_pointer_to_function to_type ->
      Tcast_null2function
  | Texpr_int z when Z.equal z Z.zero && is_pointer to_type ->
      Tcast_null2pointer
  | _ -> can_explicitly_cast ~from_type:expr.texpr_type ~to_type

(** Creates a new implicit cast expression if needed. If [expr] is already of
    [target_type], it is returned as is. This function do not check if the cast
    is legal. *)
let implicit_cast_to typ (expr : texpr) (kind : cast_kind) =
  if expr.texpr_type = typ then expr
  else mk_texpr (Texpr_implicit_cast (typ, expr, kind)) typ expr.texpr_loc

(** Tries to implicitly cast [expr] to a [target_type]. If [expr] is already of
    [target_type], it is returned as is. Otherwise, a new implicit cast
    expression is returned. If the cast is not allowed, an error is reported. *)
let try_implicit_cast_to target_type expr =
  let from_type = Type.head expr.texpr_type in
  let to_type = Type.head target_type in
  if from_type = to_type then expr
  else
    match can_implicitly_cast_expr expr to_type with
    | Tcast_invalid ->
        let msg =
          Format.asprintf
            "Cannot implicitly cast expression of type %a to type %a."
            Type.pp_print expr.texpr_type Type.pp_print target_type
        in
        error expr.texpr_loc msg
    | cast_kind ->
        mk_texpr
          (Texpr_implicit_cast (target_type, expr, cast_kind))
          target_type expr.texpr_loc

(** Performs integer promotion as specified in C23 6.3.1.1. It returns the new
    type, and the kind of cast used. *)
let rec integer_promotion t =
  match head t with
  | Ttyp_bool | Ttyp_char | Ttyp_signed_char | Ttyp_unsigned_char
  | Ttyp_signed_short | Ttyp_unsigned_short ->
      (Ttyp_signed_int, Tcast_int2int)
  | Ttyp_enum enum_decl -> integer_promotion enum_decl.tenum_underlying_type
  | _ -> (t, Tcast_noop)

(** Performs default argument promotion as specified in C23 6.5.2. It returns
    the new type, and the kind of cast used. *)
let default_argument_promotion t =
  match head t with
  | Ttyp_float -> (Ttyp_double, Tcast_float2float)
  | t -> integer_promotion t

(** Performs integer promotion on [expr]. *)
let perform_integer_promotion expr =
  let target_type, cast_kind = integer_promotion expr.texpr_type in
  implicit_cast_to target_type expr cast_kind

(** Performs default argument promotion on [expr]. *)
let perform_default_argument_promotion expr =
  let target_type, cast_kind = default_argument_promotion expr.texpr_type in
  implicit_cast_to target_type expr cast_kind

(** Implements the usual arithmetic conversions as specified in C23 6.3.1.8. The
    provided types must be arithmetic types. *)
let common_real_type t1 t2 =
  match (head t1, head t2) with
  (* If the corresponding real type of either operand is long double, the
     other operand is converted, without change of type domain, to a type
     whose corresponding real type is long double. *)
  | Ttyp_long_double, _ | _, Ttyp_long_double -> Ttyp_long_double
  (* Otherwise, if the corresponding real type of either operand is double,
     the other operand is converted, without change of type domain, to a type
     whose corresponding real type is double. *)
  | Ttyp_double, _ | _, Ttyp_double -> Ttyp_double
  (* Otherwise, if the corresponding real type of either operand is float,
     the other operand is converted, without change of type domain, to a type
     whose corresponding real type is float. *)
  | Ttyp_float, _ | _, Ttyp_float -> Ttyp_float
  (* Otherwise, if any of the two types is an enumeration, it is converted to
     its underlying type. Then, the integer promotions are performed on both
     operands. Next, the following rules are applied to the promoted operands: *)
  | t1, t2 ->
      let t1, _ = integer_promotion (convert_enumeration_type t1) in
      let t2, _ = integer_promotion (convert_enumeration_type t2) in
      let t1_is_signed = is_signed t1 in
      let t2_is_signed = is_signed t2 in
      let t1_rank = integer_conversion_rank t1 in
      let t2_rank = integer_conversion_rank t2 in

      (* If both operands have the same type, then no further conversion is needed. *)
      if t1 = t2 then t1
      else if t1_is_signed = t2_is_signed then
        (* Otherwise, if both operands are signed or both are unsigned, the operand
           with the type of lesser integer conversion rank is converted to the type
           of the operand with greater rank. *)
        if t1_rank < t2_rank then t2 else t1
      else if
        (* Otherwise, one operand is signed and the other is unsigned. *)
        (not t1_is_signed) && t1_rank >= t2_rank
      then
        (* If the type of the unsigned operand has rank greater than or equal
           to that of the signed operand, then the signed operand is converted
           to the type of the unsigned operand. *)
        t1
      else if (not t2_is_signed) && t2_rank >= t1_rank then
        (* If the type of the unsigned operand has rank greater than or equal
           to that of the signed operand, then the signed operand is converted
           to the type of the unsigned operand. *)
        t2
      else if t1_is_signed then
        (* FIXME: can represent all values of the type of the unsigned operand? *)
        (* Otherwise, if the type of the signed operand can represent all values
           of the type of the unsigned operand, then the unsigned operand is
           converted to the type of the signed operand. *)
        t1
      else if t2_is_signed then
        (* FIXME: can represent all values of the type of the unsigned operand? *)
        (* Otherwise, if the type of the signed operand can represent all values
           of the type of the unsigned operand, then the unsigned operand is
           converted to the type of the signed operand. *)
        t2
      else if
        (* Otherwise, both operands are converted to the unsigned type corresponding
           to the type of the signed operand. *)
        t1_is_signed
      then to_unsigned t1
      else to_unsigned t2

(** Performs usual arithmetic conversion on [expr1] and [expr2] as specified in
    C23 6.5.8. It returns the either the original expressions or the new
    expressions after insertion of implicit casts. *)
let perform_usual_arithmetic_conversion expr1 expr2 =
  let common_type = common_real_type expr1.texpr_type expr2.texpr_type in
  let expr1_cast_kind = can_explicitly_cast_expr expr1 common_type in
  let expr2_cast_kind = can_explicitly_cast_expr expr2 common_type in
  assert (expr1_cast_kind <> Tcast_invalid);
  assert (expr2_cast_kind <> Tcast_invalid);
  let expr1_casted = implicit_cast_to common_type expr1 expr1_cast_kind in
  let expr2_casted = implicit_cast_to common_type expr2 expr2_cast_kind in
  (expr1_casted, expr2_casted)
