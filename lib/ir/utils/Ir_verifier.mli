(*
 * Copyright (c) 2025 Hubert Gruniaux
 * This file is part of the Iris project.
 *
 * This file contains the IR verifier. It checks that the IR is well-typed
 * and that all invariants are respected.
 *)

open Ir_base

exception Error of string

val check_ibinary : ibinop -> value -> value -> unit
(** [check_ibinary op v1 v2] checks that the binary operation [op] is well-typed
    for the operands [v1] and [v2]. Raises [Error e] in case of error. *)

val check_iunary : iunop -> value -> unit
(** [check_iunary op v] checks that the unary operation [op] is well-typed for
    the operand [v]. Raises [Error e] in case of error. *)

val check_icmp : icmpop -> value -> value -> unit
(** [check_icmp op v1 v2] checks that the comparison operation [op] is
    well-typed for the operands [v1] and [v2]. Raises [Error e] in case of
    error. *)

val check_cast : castop -> typ -> value -> unit
(** [check_cast op t v] checks that the cast operation [op] is valid from [v] to
    the target type [t]. Raises [Error e] in case of error. *)

val check_load : typ -> value -> unit
(** [check_load t v] checks that the load operation is well-typed for the target
    type [t] and the address [v]. Raises [Error e] in case of error. *)

val check_store : value -> value -> unit
(** [check_store addr v] checks that the store operation is well-typed for the
    address [addr] and the value [v]. Raises [Error e] in case of error. *)

val check_call : value -> value list -> unit
(** [check_call fn args] checks that the call operation is well-typed for the
    function [fn] and its [args]. Raises [Error e] in case of error. *)

val check_ret : fn -> value option -> unit
(** [check_ret fn v_opt] checks that the return operation is well-typed for the
    function [fn] and the optional return value [v_opt]. Raises [Error e] in
    case of error. *)

val check_br : fn -> label -> value list -> unit
(** [check_br fn target_label args] checks that the branch operation to
    [target_label] with [args] is valid within the function [fn]. Raises
    [Error e] in case of error. *)

val check_br_if :
  fn -> value -> label -> value list -> label -> value list -> unit
(** [check_br_if fn cond true_label true_args false_label false_args] checks
    that the conditional branch operation is valid within the function [fn],
    ensuring that [cond] is of boolean type and that both [true_label] and
    [false_label] exist and take [true_args] and [false_args] respectively.
    Raises [Error e] in case of error. *)

val check_uses : fn -> unit
(** [check_uses fn] checks that all uses of registers in the function [fn] are
    valid, i.e., that they are defined and dominated by their definitions.
    Raises [Error e] if any use is invalid. *)
