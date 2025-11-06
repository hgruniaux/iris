(*
 * Copyright (c) 2025 Hubert Gruniaux
 * This file is part of the Iris project.
 *)

open Ir_base
module Value = Ir_value

let bitwidth_of_integer_type = function
  | Ityp_i1 -> 1
  | Ityp_i8 -> 8
  | Ityp_i16 -> 16
  | Ityp_i32 -> 32
  | Ityp_i64 -> 64
  | _ -> failwith "Ir_eval.bitwidth_of_integer_type: not an integer type"

(** Wraps an arbitrary precision signed integer value into the target type
    integer. *)
let wrap_sint target_type v =
  let bw = bitwidth_of_integer_type target_type in
  Z.signed_extract v 0 bw

let wrap_uint target_type v =
  let bw = bitwidth_of_integer_type target_type in
  Z.extract v 0 bw

(** Normalizes the given value [v]. *)
let normalize v =
  match v with
  | Ival_int (typ, value) ->
      let wrapped_value = wrap_sint typ value in
      Ival_int (typ, wrapped_value)
  | _ -> v

let eval_ibinary_impl f v1 v2 =
  match (v1, v2) with
  | Ival_int (typ1, v1), Ival_int (typ2, v2) when typ1 = typ2 ->
      let result = f v1 v2 in
      Some (Ival_int (typ1, wrap_sint typ1 result))
  | _ -> None

let eval_iadd v1 v2 =
  match (v1, v2) with
  | Ival_int (_, v1), _ when Z.equal v1 Z.zero -> Some (normalize v2)
  | _, Ival_int (_, v2) when Z.equal v2 Z.zero -> Some (normalize v1)
  | _ -> eval_ibinary_impl Z.add v1 v2

let eval_isub v1 v2 =
  match (v1, v2) with
  | _, Ival_int (_, v2) when Z.equal v2 Z.zero -> Some (normalize v1)
  | _ -> eval_ibinary_impl Z.sub v1 v2

let eval_imul v1 v2 =
  match (v1, v2) with
  | Ival_int (t, v1), _ when Z.equal v1 Z.zero -> Some (Ival_int (t, Z.zero))
  | _, Ival_int (t, v2) when Z.equal v2 Z.zero -> Some (Ival_int (t, Z.zero))
  | Ival_int (_, v1), _ when Z.equal v1 Z.one -> Some (normalize v2)
  | _, Ival_int (_, v2) when Z.equal v2 Z.one -> Some (normalize v1)
  | _ -> eval_ibinary_impl Z.mul v1 v2

let eval_idiv_s _v1 _v2 = None
let eval_idiv_u _v1 _v2 = None
let eval_irem_s _v1 _v2 = None
let eval_irem_u _v1 _v2 = None

let eval_bitshift f v1 v2 =
  match (v1, v2) with
  | _, Ival_int (_, v2) when Z.equal v2 Z.zero -> Some v1
  | Ival_int (t1, v1), Ival_int (_, v2) ->
      let t1_bw = bitwidth_of_integer_type t1 in
      if Z.lt v2 Z.zero || Z.geq v2 (Z.of_int t1_bw) then
        (* If [v2] is negative or larger than or equal to the number of bits of [t1],
           the result is undefined. *)
        None
      else
        let shift = Z.to_int v2 in
        let result = f (Z.extract v1 0 t1_bw) shift in
        Some (Ival_int (t1, result))
  | _ -> None

let eval_lsl v1 v2 = eval_bitshift (fun v shift -> Z.shift_left v shift) v1 v2

let eval_lsr v1 v2 =
  eval_bitshift (fun v shift -> Z.shift_right_trunc v shift) v1 v2

let eval_asr v1 v2 = eval_bitshift (fun v shift -> Z.shift_right v shift) v1 v2

let eval_ibinary op v1 v2 =
  match op with
  | Ibinop_add -> eval_iadd v1 v2
  | Ibinop_sub -> eval_isub v1 v2
  | Ibinop_mul -> eval_imul v1 v2
  | Ibinop_div_s -> eval_idiv_s v1 v2
  | Ibinop_div_u -> eval_idiv_u v1 v2
  | Ibinop_rem_s -> eval_irem_s v1 v2
  | Ibinop_rem_u -> eval_irem_u v1 v2
  | Ibinop_and -> eval_ibinary_impl Z.logand v1 v2
  | Ibinop_or -> eval_ibinary_impl Z.logor v1 v2
  | Ibinop_xor -> eval_ibinary_impl Z.logxor v1 v2
  | Ibinop_lsl -> eval_lsl v1 v2
  | Ibinop_lsr -> eval_lsr v1 v2
  | Ibinop_asr -> eval_asr v1 v2

module TestIBinary = struct
  let i32_unknown = Ival_reg (Reg.fresh Ityp_i32)
  let i8 x = Ival_int (Ityp_i8, Z.of_string x)
  let i16 x = Ival_int (Ityp_i16, Z.of_string x)
  let i32 x = Ival_int (Ityp_i32, Z.of_string x)
  let i64 x = Ival_int (Ityp_i64, Z.of_string x)

  let check_ibinary op v1 v2 expected =
    match eval_ibinary op v1 v2 with
    | Some result ->
        if result <> expected then
          let msg =
            Format.asprintf "expected %a but got %a for (%a %a %a)"
              Ir_printer.Printer.pp_value expected Ir_printer.Printer.pp_value
              result Ir_printer.Printer.pp_ibinop op Ir_printer.Printer.pp_value
              v1 Ir_printer.Printer.pp_value v2
          in
          failwith msg
    | None -> failwith "eval_ibinary: expected Some result but got None"

  let check_ibinary_error op v1 v2 =
    match eval_ibinary op v1 v2 with
    | Some result ->
        let msg =
          Format.asprintf "expected error but got %a for (%a %a %a)"
            Ir_printer.Printer.pp_value result Ir_printer.Printer.pp_ibinop op
            Ir_printer.Printer.pp_value v1 Ir_printer.Printer.pp_value v2
        in
        failwith msg
    | None -> ()

  let%test_unit "eval_ibinary: addition" =
    (* Symbolic simplification *)
    check_ibinary Ibinop_add i32_unknown (i32 "0") i32_unknown;
    check_ibinary Ibinop_add (i32 "0") i32_unknown i32_unknown;

    (* Concrete addition: i8 *)
    check_ibinary Ibinop_add (i8 "10") (i8 "5") (i8 "15");
    check_ibinary Ibinop_add (i8 "127") (i8 "1") (i8 "-128");
    check_ibinary Ibinop_add (i8 "127") (i8 "127") (i8 "-2");
    check_ibinary Ibinop_add (i8 "-128") (i8 "-1") (i8 "127");
    check_ibinary Ibinop_add (i8 "-128") (i8 "127") (i8 "-1");
    check_ibinary Ibinop_add (i8 "-5") (i8 "5") (i8 "0");

    (* Concrete addition: i32 *)
    check_ibinary Ibinop_add (i32 "1000") (i32 "2000") (i32 "3000");
    check_ibinary Ibinop_add (i32 "2147483647") (i32 "1") (i32 "-2147483648");
    check_ibinary Ibinop_add (i32 "-2147483648") (i32 "-1") (i32 "2147483647");
    check_ibinary Ibinop_add (i32 "2147483647") (i32 "2147483647") (i32 "-2");
    check_ibinary Ibinop_add (i32 "-1000") (i32 "-1") (i32 "-1001");
    check_ibinary Ibinop_add (i32 "0") (i32 "4294967295") (i32 "-1");
    check_ibinary Ibinop_add (i32 "4294967295") (i32 "4294967295") (i32 "-2");

    (* Concrete addition: i64 *)
    check_ibinary Ibinop_add (i64 "1000000") (i64 "2000000") (i64 "3000000");
    check_ibinary Ibinop_add
      (i64 "9223372036854775807")
      (i64 "1")
      (i64 "-9223372036854775808");
    check_ibinary Ibinop_add
      (i64 "-9223372036854775808")
      (i64 "-1")
      (i64 "9223372036854775807");
    check_ibinary Ibinop_add (i64 "-1000") (i64 "1000") (i64 "0")

  let%test_unit "eval_ibinary: subtraction" =
    (* Symbolic simplification *)
    check_ibinary Ibinop_sub i32_unknown (i32 "0") i32_unknown;

    (* Concrete subtraction: i8 *)
    check_ibinary Ibinop_sub (i8 "10") (i8 "5") (i8 "5");
    check_ibinary Ibinop_sub (i8 "-128") (i8 "1") (i8 "127");
    check_ibinary Ibinop_sub (i8 "127") (i8 "-1") (i8 "-128");
    check_ibinary Ibinop_sub (i8 "0") (i8 "127") (i8 "-127");
    check_ibinary Ibinop_sub (i8 "-5") (i8 "-5") (i8 "0");

    (* Concrete subtraction: i32 *)
    check_ibinary Ibinop_sub (i32 "5000") (i32 "2000") (i32 "3000");
    check_ibinary Ibinop_sub (i32 "-2147483648") (i32 "1") (i32 "2147483647");
    check_ibinary Ibinop_sub (i32 "2147483647") (i32 "-1") (i32 "-2147483648");
    check_ibinary Ibinop_sub (i32 "0") (i32 "2147483647") (i32 "-2147483647");
    check_ibinary Ibinop_sub (i32 "-10") (i32 "2147483647") (i32 "2147483639");

    (* Concrete subtraction: i64 *)
    check_ibinary Ibinop_sub (i64 "3000000") (i64 "1000000") (i64 "2000000");
    check_ibinary Ibinop_sub
      (i64 "-9223372036854775808")
      (i64 "1")
      (i64 "9223372036854775807");
    check_ibinary Ibinop_sub
      (i64 "9223372036854775807")
      (i64 "-1")
      (i64 "-9223372036854775808");
    check_ibinary Ibinop_sub (i64 "-1000") (i64 "1") (i64 "-1001")

  let%test_unit "eval_ibinary: multiplication" =
    (* Symbolic simplification *)
    check_ibinary Ibinop_mul i32_unknown (i32 "0") (i32 "0");
    check_ibinary Ibinop_mul (i32 "0") i32_unknown (i32 "0");
    check_ibinary Ibinop_mul i32_unknown (i32 "1") i32_unknown;
    check_ibinary Ibinop_mul (i32 "1") i32_unknown i32_unknown;
    check_ibinary Ibinop_mul (i32 "4294967295") (i32 "0") (i32 "0");

    (* Concrete multiplication: i8 *)
    check_ibinary Ibinop_mul (i8 "2") (i8 "3") (i8 "6");
    check_ibinary Ibinop_mul (i8 "1") (i8 "127") (i8 "127");
    check_ibinary Ibinop_mul (i8 "127") (i8 "0") (i8 "0");
    check_ibinary Ibinop_mul (i8 "-128") (i8 "1") (i8 "-128");
    check_ibinary Ibinop_mul (i8 "-1") (i8 "-1") (i8 "1");

    (* Concrete multiplication: i32 *)
    check_ibinary Ibinop_mul (i32 "100") (i32 "200") (i32 "20000");
    check_ibinary Ibinop_mul (i32 "-2147483648") (i32 "1") (i32 "-2147483648");
    check_ibinary Ibinop_mul (i32 "2147483647") (i32 "1") (i32 "2147483647");
    check_ibinary Ibinop_mul (i32 "-10") (i32 "5") (i32 "-50");
    check_ibinary Ibinop_mul (i32 "-100") (i32 "-200") (i32 "20000");
    check_ibinary Ibinop_mul (i32 "4294967295") (i32 "4294967295") (i32 "1")

  let%test_unit "eval_ibinary: signed division" =
    (* Symbolic simplification *)
    check_ibinary Ibinop_div_s i32_unknown (i32 "1") i32_unknown;
    check_ibinary Ibinop_div_s (i32 "1") i32_unknown i32_unknown;

    check_ibinary_error Ibinop_div_s i32_unknown (i32 "0");
    check_ibinary_error Ibinop_div_s (i32 "1") (i32 "0");

    (* Concrete division: i8 *)
    check_ibinary Ibinop_div_s (i8 "127") (i8 "127") (i8 "1");
    check_ibinary Ibinop_div_s (i8 "-128") (i8 "2") (i8 "-64");
    check_ibinary Ibinop_div_s (i8 "-127") (i8 "4") (i8 "-31");
    check_ibinary_error Ibinop_div_s (i8 "-128") (i8 "-1");

    (* Concrete division: i32 *)
    check_ibinary Ibinop_div_s (i32 "100") (i32 "10") (i32 "10");
    check_ibinary Ibinop_div_s (i32 "-100") (i32 "10") (i32 "-10");
    check_ibinary Ibinop_div_s (i32 "100") (i32 "-10") (i32 "-10");
    check_ibinary Ibinop_div_s (i32 "-100") (i32 "-10") (i32 "10");
    check_ibinary Ibinop_div_s (i32 "10") (i32 "3") (i32 "3");
    check_ibinary Ibinop_div_s (i32 "-10") (i32 "3") (i32 "-3");
    check_ibinary_error Ibinop_div_s (i32 "2147483647") (i32 "-1")

  let%test_unit "eval_ibinary: signed remainder" =
    (* Symbolic simplification *)
    check_ibinary_error Ibinop_rem_s i32_unknown (i32 "0");
    check_ibinary_error Ibinop_rem_s (i32 "1") (i32 "0");

    (* Concrete division: i8 *)
    check_ibinary Ibinop_rem_s (i8 "127") (i8 "5") (i8 "2");
    check_ibinary Ibinop_rem_s (i8 "-127") (i8 "5") (i8 "-2");
    check_ibinary Ibinop_rem_s (i8 "-127") (i8 "-5") (i8 "-2");
    check_ibinary Ibinop_rem_s (i8 "-128") (i8 "3") (i8 "-2");
    check_ibinary Ibinop_rem_s (i8 "-128") (i8 "-1") (i8 "0");

    (* Concrete division: i32 *)
    check_ibinary Ibinop_rem_s (i32 "10") (i32 "3") (i32 "1");
    check_ibinary Ibinop_rem_s (i32 "-10") (i32 "3") (i32 "-1");
    check_ibinary Ibinop_rem_s (i32 "10") (i32 "-3") (i32 "1");
    check_ibinary Ibinop_rem_s (i32 "-10") (i32 "-3") (i32 "-1");
    check_ibinary Ibinop_rem_s (i32 "2147483647") (i32 "2") (i32 "1");
    check_ibinary Ibinop_rem_s (i32 "-2147483648") (i32 "-1") (i32 "0")

  let%test_unit "eval_ibinary: unsigned division" =
    (* Symbolic simplification *)
    check_ibinary Ibinop_div_u i32_unknown (i32 "1") i32_unknown;
    check_ibinary Ibinop_div_u (i32 "1") i32_unknown i32_unknown;

    check_ibinary_error Ibinop_div_u i32_unknown (i32 "0");
    check_ibinary_error Ibinop_div_u (i32 "1") (i32 "0");

    (* Concrete division: i8 *)
    check_ibinary Ibinop_div_u (i8 "255") (i8 "1") (i8 "-1");
    check_ibinary Ibinop_div_u (i8 "255") (i8 "2") (i8 "127");
    check_ibinary Ibinop_div_u (i8 "100") (i8 "3") (i8 "33");
    check_ibinary Ibinop_div_u (i8 "255") (i8 "255") (i8 "1");

    (* Concrete division: i32 *)
    check_ibinary Ibinop_div_u (i32 "100") (i32 "10") (i32 "10");
    check_ibinary Ibinop_div_u (i32 "4294967295") (i32 "1") (i32 "-1");
    check_ibinary Ibinop_div_u (i32 "4294967295") (i32 "2") (i32 "2147483647");
    check_ibinary Ibinop_div_u (i32 "4294967295") (i32 "4294967295") (i32 "1")

  let%test_unit "eval_ibinary: unsigned remainder" =
    (* Symbolic simplification *)
    check_ibinary_error Ibinop_rem_u i32_unknown (i32 "0");
    check_ibinary_error Ibinop_rem_u (i32 "1") (i32 "0");

    (* Concrete division: i8 *)
    check_ibinary Ibinop_rem_u (i8 "255") (i8 "2") (i8 "1");
    check_ibinary Ibinop_rem_u (i8 "250") (i8 "3") (i8 "1");
    check_ibinary Ibinop_rem_u (i8 "255") (i8 "128") (i8 "127");
    check_ibinary Ibinop_rem_u (i8 "100") (i8 "10") (i8 "0");

    (* Concrete division: i32 *)
    check_ibinary Ibinop_rem_u (i32 "10") (i32 "3") (i32 "1");
    check_ibinary Ibinop_rem_u (i32 "4294967295") (i32 "2") (i32 "1");
    check_ibinary Ibinop_rem_u (i32 "4294967295") (i32 "4294967294") (i32 "1");
    check_ibinary Ibinop_rem_u (i32 "1000") (i32 "100") (i32 "0")

  let%test_unit "eval_ibinary: bitwise and" =
    (* Concrete and: i8 *)
    check_ibinary Ibinop_and (i8 "-1") (i8 "1") (i8 "1");
    check_ibinary Ibinop_and (i8 "-128") (i8 "127") (i8 "0");
    check_ibinary Ibinop_and (i8 "-5") (i8 "-10") (i8 "-14");
    check_ibinary Ibinop_and (i8 "255") (i8 "1") (i8 "1");
    check_ibinary Ibinop_and (i8 "100") (i8 "200") (i8 "64");

    (* Concrete and: i32 *)
    check_ibinary Ibinop_and (i32 "5") (i32 "3") (i32 "1");
    check_ibinary Ibinop_and (i32 "-1") (i32 "1") (i32 "1");
    check_ibinary Ibinop_and (i32 "-1") (i32 "-2147483648") (i32 "-2147483648");
    check_ibinary Ibinop_and (i32 "-10") (i32 "-20") (i32 "-28");
    check_ibinary Ibinop_and (i32 "-1") (i32 "1") (i32 "1");
    check_ibinary Ibinop_and (i32 "100000") (i32 "200000") (i32 "66560");
    check_ibinary Ibinop_and (i32 "-2147483648") (i32 "2147483647") (i32 "0")

  let%test_unit "eval_ibinary: bitwise or" =
    (* Concrete or: i8 *)
    check_ibinary Ibinop_or (i8 "-1") (i8 "1") (i8 "-1");
    check_ibinary Ibinop_or (i8 "-128") (i8 "127") (i8 "-1");
    check_ibinary Ibinop_or (i8 "-5") (i8 "-10") (i8 "-1");
    check_ibinary Ibinop_or (i8 "255") (i8 "1") (i8 "-1");
    check_ibinary Ibinop_or (i8 "100") (i8 "200") (i8 "-20");

    (* Concrete or: i32 *)
    check_ibinary Ibinop_or (i32 "5") (i32 "3") (i32 "7");
    check_ibinary Ibinop_or (i32 "-1") (i32 "1") (i32 "-1");
    check_ibinary Ibinop_or (i32 "-1") (i32 "-2147483648") (i32 "-1");
    check_ibinary Ibinop_or (i32 "-10") (i32 "-20") (i32 "-2");
    check_ibinary Ibinop_or (i32 "-1") (i32 "1") (i32 "-1");
    check_ibinary Ibinop_or (i32 "100000") (i32 "200000") (i32 "233440");
    check_ibinary Ibinop_or (i32 "-2147483648") (i32 "2147483647") (i32 "-1")

  let%test_unit "eval_ibinary: bitwise xor" =
    (* Concrete xor: i8 *)
    check_ibinary Ibinop_xor (i8 "-1") (i8 "1") (i8 "-2");
    check_ibinary Ibinop_xor (i8 "-128") (i8 "127") (i8 "-1");
    check_ibinary Ibinop_xor (i8 "-5") (i8 "-10") (i8 "13");
    check_ibinary Ibinop_xor (i8 "255") (i8 "1") (i8 "-2");
    check_ibinary Ibinop_xor (i8 "100") (i8 "200") (i8 "-84");

    (* Concrete xor: i32 *)
    check_ibinary Ibinop_xor (i32 "5") (i32 "3") (i32 "6");
    check_ibinary Ibinop_xor (i32 "-1") (i32 "1") (i32 "-2");
    check_ibinary Ibinop_xor (i32 "-1") (i32 "-2147483648") (i32 "2147483647");
    check_ibinary Ibinop_xor (i32 "-10") (i32 "-20") (i32 "26");
    check_ibinary Ibinop_xor (i32 "-1") (i32 "1") (i32 "-2");
    check_ibinary Ibinop_xor (i32 "100000") (i32 "200000") (i32 "166880");
    check_ibinary Ibinop_xor (i32 "-2147483648") (i32 "2147483647") (i32 "-1")
end

let eval_iunary op _v = match op with _ -> None

let eval_icmp_impl v1 v2 f =
  let v1 = normalize v1 in
  let v2 = normalize v2 in
  match (v1, v2) with
  | Ival_int (_, v1), Ival_int (_, v2) ->
      let result = if f v1 v2 then Z.one else Z.zero in
      Some (Ival_int (Ityp_i1, result))
  | _ -> None

let eval_icmp_eq v1 v2 =
  if Value.equal v1 v2 then Some (Ival_int (Ityp_i1, Z.one))
  else eval_icmp_impl v1 v2 Z.equal

let eval_icmp_ne v1 v2 =
  if Value.equal v1 v2 then Some (Ival_int (Ityp_i1, Z.zero))
  else eval_icmp_impl v1 v2 (fun a b -> not (Z.equal a b))

let eval_icmp op v1 v2 =
  match op with
  | Icmp_eq -> eval_icmp_eq v1 v2
  | Icmp_ne -> eval_icmp_ne v1 v2
  | _ -> None

module TestICmp = struct
  let i32_unknown_1 = Ival_reg (Reg.fresh Ityp_i32)
  let i32_unknown_2 = Ival_reg (Reg.fresh Ityp_i32)
  let i8 x = Ival_int (Ityp_i8, Z.of_string x)
  let i16 x = Ival_int (Ityp_i16, Z.of_string x)
  let i32 x = Ival_int (Ityp_i32, Z.of_string x)
  let i64 x = Ival_int (Ityp_i64, Z.of_string x)

  let check_icmp cmp v1 v2 expected =
    match eval_icmp cmp v1 v2 with
    | Some (Ival_int (Ityp_i1, result)) ->
        let bool_result = Z.equal result Z.one in
        if bool_result <> expected then
          let msg =
            Format.asprintf "expected %b for (%a %a %a)" expected
              Ir_printer.Printer.pp_icmp cmp Ir_printer.Printer.pp_value v1
              Ir_printer.Printer.pp_value v2
          in
          failwith msg
    | Some result ->
        let msg =
          Format.asprintf "expected i1 result but got %a for (%a %a %a)"
            Ir_printer.Printer.pp_value result Ir_printer.Printer.pp_icmp cmp
            Ir_printer.Printer.pp_value v1 Ir_printer.Printer.pp_value v2
        in
        failwith msg
    | None -> failwith "eval_icmp: expected Some result but got None"

  let%test_unit "eval_icmp: equality" =
    (* Symbolic simplification *)
    check_icmp Icmp_eq i32_unknown_1 i32_unknown_1 true;

    (* Concrete equality *)
    check_icmp Icmp_eq (i32 "10") (i32 "10") true;
    check_icmp Icmp_eq (i32 "10") (i32 "20") false;
    check_icmp Icmp_eq (i8 "-1") (i8 "255") true;
    check_icmp Icmp_eq (i16 "32767") (i16 "-32769") true

  let%test_unit "eval_icmp: inequality" =
    (* Symbolic simplification *)
    check_icmp Icmp_ne i32_unknown_1 i32_unknown_1 false;

    (* Concrete inequality *)
    check_icmp Icmp_ne (i32 "10") (i32 "10") false;
    check_icmp Icmp_ne (i32 "10") (i32 "20") true;
    check_icmp Icmp_ne (i8 "-1") (i8 "255") false;
    check_icmp Icmp_ne (i16 "32767") (i16 "-32769") false
end

let eval_cast_extend_s target_type v =
  match v with
  | Ival_int (source_type, value) ->
      let bw_target_typ = Type.bitwidth target_type in
      let bw_source_typ = Type.bitwidth source_type in
      assert (bw_target_typ > bw_source_typ);
      let extended_c = Z.signed_extract value 0 bw_source_typ in
      Some (Ival_int (target_type, extended_c))
  | _ -> None

let eval_cast_extend_u target_type v =
  match v with
  | Ival_int (source_type, value) ->
      let bw_target_typ = Type.bitwidth target_type in
      let bw_source_typ = Type.bitwidth source_type in
      assert (bw_target_typ > bw_source_typ);
      let extended_c = Z.extract value 0 bw_source_typ in
      Some (Ival_int (target_type, extended_c))
  | _ -> None

let eval_cast op target_type v =
  if target_type = Value.type_of v then Some (normalize v)
  else
    match op with
    | Icast_extend_s -> eval_cast_extend_s target_type v
    | Icast_extend_u -> eval_cast_extend_u target_type v
    | _ -> None

module TestCast = struct
  let i8 x = Ival_int (Ityp_i8, Z.of_string x)
  let i16 x = Ival_int (Ityp_i16, Z.of_string x)
  let i32 x = Ival_int (Ityp_i32, Z.of_string x)
  let i64 x = Ival_int (Ityp_i64, Z.of_string x)
  let f32 x = Ival_float (Ityp_f32, Float.of_string x)
  let f64 x = Ival_float (Ityp_f64, Float.of_string x)

  let check_icast op v expected =
    match eval_cast op (Value.type_of expected) v with
    | Some result ->
        if result <> expected then
          let msg =
            Format.asprintf "expected %a but got %a for cast %a of %a"
              Ir_printer.Printer.pp_value expected Ir_printer.Printer.pp_value
              result Ir_printer.Printer.pp_castop op Ir_printer.Printer.pp_value
              v
          in
          failwith msg
    | None -> failwith "eval_cast: expected Some result but got None"

  let%test_unit "eval_cast: extend signed" =
    check_icast Icast_extend_s (i8 "10") (i32 "10");
    check_icast Icast_extend_s (i8 "127") (i32 "127");
    check_icast Icast_extend_s (i8 "-1") (i32 "-1");
    check_icast Icast_extend_s (i8 "-128") (i32 "-128");
    check_icast Icast_extend_s (i16 "32767") (i64 "32767");
    check_icast Icast_extend_s (i16 "-1") (i64 "-1");
    check_icast Icast_extend_s (i16 "-32768") (i64 "-32768");
    check_icast Icast_extend_s (i8 "65535") (i32 "-1")

  let%test_unit "eval_cast: extend unsigned" =
    check_icast Icast_extend_u (i8 "10") (i32 "10");
    check_icast Icast_extend_u (i8 "255") (i32 "255");
    check_icast Icast_extend_u (i8 "0") (i32 "0");
    check_icast Icast_extend_u (i8 "65535") (i64 "255");
    check_icast Icast_extend_u (i8 "32768") (i64 "0");
    check_icast Icast_extend_u (i8 "1") (i64 "1")
end
