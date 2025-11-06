(* Copyright (C) 2025 Hubert Gruniaux
 * This file is part of Iris. *)

val pointer_integer_type : Ir_base.Type.t
(** The IR type representing a pointer-sized integer. Generally [Ityp_i32] or
    [Ityp_i64]. *)

val size_of : Ir_base.Type.t -> int
(** [size_of ir_ty]

    Returns the size in bytes of the given IR type. *)

val align_of : Ir_base.Type.t -> int
(** [align_of ir_ty]

    Returns the preferred minimum alignment in bytes of the given IR type. The
    alignment is always a power of two and positive. *)

val compute_struct_layout :
  is_packed:bool -> Ir_base.Type.t list -> (int * int) list
(** [compute_struct_layout ~is_packed field_types]

    Given a list of IR types representing the fields of a struct, computes the
    layout of the struct as a list of (offset, size) pairs for each field.

    The offset is the byte offset of the field within the struct, and the size
    is the size in bytes of the field.

    The function takes into account alignment requirements of each field type
    and adds padding as necessary, unless [is_packed] is true. In that case, no
    padding is added between fields. *)
