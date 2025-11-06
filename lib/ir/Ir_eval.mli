(*
 * Copyright (c) 2025 Hubert Gruniaux
 * This file is part of the Iris project.
 *
 * This file contains the implementation of the IR constant evaluator.
 *)

open Ir_base

val eval_ibinary : ibinop -> value -> value -> value option
(** Evaluates a binary integer operation between two values. If it unable to
    evaluate the operation (error occurred, non constant operands, undefined
    behavior, etc.), it returns [None]. *)

val eval_iunary : iunop -> value -> value option
(** Evaluates a unary integer operation on a value. If it unable to evaluate the
    operation (error occurred, non constant operands, undefined behavior, etc.),
    it returns [None]. *)

val eval_icmp : icmpop -> value -> value -> value option
(** Evaluates a comparison operation between two values. If it unable to
    evaluate the operation (error occurred, non constant operands, undefined
    behavior, etc.), it returns [None]. *)
