(*
 * Copyright (c) 2025 Hubert Gruniaux
 * This file is part of the Iris project.
 *
 * This file provides an interface for encoding an Iris IR function into
 * a Z3 SMT formula. It translates IR instructions and control flow into
 * corresponding SMT constructs, enabling formal verification and analysis
 * of the IR code.
 *)

type context
type encoded_function
type equivalence_fail_info

type equivalence_result =
  | Equivalent  (** The two functions are equivalent. *)
  | Unknown of string
      (** The equivalence check could not be performed, the result is unknown
          (e.g. timeout). A small explanation message is provided. *)
  | Not_equivalent of equivalence_fail_info
      (** The two functions are not equivalent. Additional information about the
          failure is provided. *)

val create_context : Z3.context -> context
(** Creates a new SMT encoding context using the given Z3 context. *)

val encode_function : context -> Ir.Function.t -> encoded_function
(** [encode_function ctx fn] encodes the given IR function [fn] into multiple
    SMT functions modeling its behavior. See {!encoded_function} for details. *)

val check_equivalence :
  context -> encoded_function -> encoded_function -> equivalence_result
(** [check_equivalence ctx src_fn tgt_fn] checks the equivalence of the two
    encoded functions [src_fn] and [tgt_fn] within the given context [ctx]. *)

val check_module : Ir.Module.t -> unit
(** Encode the [source] and [target] functions in the given module, and check
    their equivalence using SMT. *)
