open Ir_base

type t

val create : ctx -> t
(** Creates a new IR builder for the given IR module. *)

val emit_bool_constant : t -> bool -> value
(** Emits a boolean constant as an integer of type i1. *)

val emit_int_constant : t -> typ -> Z.t -> value
(** Emits an integer constant. The constant is wrapped-around if it exceeds the
    bit-width of the type. *)

val emit_float_constant : t -> typ -> float -> value
(** Emits a floating-point constant. *)

val emit_constant_zero : t -> typ -> value
(** Emits a zero constant for the given type (integer or float). *)

val emit_constant_one : t -> typ -> value
(** Emits a one constant for the given type (integer or float). *)

val emit_string_constant :
  ?unique:bool -> ?nul_terminated:bool -> t -> string -> value
(** Emits a string constant.

    If [unique] is [true], the string constant is reused if it already exists.

    If [nul_terminated] is [true], the string will be emitted as a
    null-terminated string. *)

val emit_alloca : t -> typ -> value
(** [emit_alloca b typ]

    Emits an allocation instruction for a local variable of type [typ]. The
    instruction is inserted at the beginning of the current function's entry
    basic block, and not at the current insertion point. *)

val emit_load : t -> typ -> value -> value
(** [emit_load b typ ptr]

    Emits a load instruction from the given pointer value. The loaded value is
    of the specified type [typ]. *)

val emit_store : t -> value -> value -> unit
(** [emit_store b ptr value]

    Emits a store instruction to the given pointer value. *)

val emit_array_addr : t -> typ -> value -> value -> value
(** [emit_array_addr b elem_typ base index]

    Emits instructions to compute the address of the element at the given index
    in the array. The [elem_typ] is the type of each element in the array. The
    [base] is a pointer to the first element of the array. The [index] is the
    index of the desired element. *)

val emit_struct_field_addr : t -> typ -> value -> int -> value
(** [emit_struct_field_addr b struct_typ base field_index]

    Emits instructions to compute the address of the specified field within a
    struct. The [base] is a pointer to the struct, and [field_index] is the
    index of the field within the struct. *)

val emit_ibinary : t -> ibinop -> value -> value -> value
val emit_iunary : t -> iunop -> value -> value
val emit_icmp : t -> icmpop -> value -> value -> value

val emit_cast : t -> castop -> typ -> value -> value
(** Emits a cast instruction from one type to another using the specified cast
    operation. Note, not all casts are allowed. *)

val emit_cast_int : signed:bool -> t -> typ -> value -> value
(** Emits a cast instruction from a value of one integer type to another. If
    [signed] is true, the target integer type is treated as signed.

    In particular, this function selects between no cast, sign extension, zero
    extension, or truncation as appropriate. *)

val emit_cast_float : t -> typ -> value -> value
(** Emits a cast instruction from a value of one floating-point type to another.

    In particular, this function selects between no cast, floating-point
    extension, or floating-point truncation as appropriate. *)

val emit_call : t -> value -> value list -> value
(** Emits a function call instruction. *)

val emit_noreturn_call : t -> value -> value list -> unit
(** Emits a a call to a function that does not return. It is equivalent to
    emitting a call instruction then an unreachable terminator. *)

val emit_unreachable : t -> unit
val emit_ret : t -> value option -> unit
val emit_br : t -> bb -> unit
val emit_br_if : t -> value -> bb -> bb -> unit
val emit_extern_function : t -> string -> typ -> global

val begin_function : t -> string -> typ -> global * reg list
(** Begins the definition of a new function. This creates a new function global
    in the module with the provided signature, creates its entry basic block,
    and sets the insertion point to that basic block. Returns the created
    function global and its parameters. *)

val end_function : t -> unit
(** Ends the definition of the current function. *)

val current_module : t -> ctx
(** Returns the current IR module being built. *)

val current_function : t -> fn option
(** Returns the current function being used as insertion point, or [None] if not
    in a function. *)

val current_block : t -> bb option
(** Returns the current basic block being used as insertion point, or [None] if
    not in a basic block. *)

val set_current_block : t -> bb -> unit
(** Sets the current basic block being used as insertion point. *)

val fresh_block : ?name:string option -> t -> bb
(** Creates and returns a new basic block in the current function. The new basic
    block is not set as the current insertion point. *)
