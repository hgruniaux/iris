open Tast

type t = ttype
type quals = { qual_const : bool; qual_volatile : bool; qual_restrict : bool }

let rec head t =
  match t with Ttyp_typedef td -> head td.ttypedef_type | _ -> t

(** Check if [t] is an incomplete type as defined in C23 6.2.5. *)
let is_incomplete t =
  match head t with
  (* C23 6.2.5 §24
     The void type comprises an empty set of values; it is an incomplete
     object type that cannot be completed. *)
  | Ttyp_void -> true
  (* C23 6.2.5 §27
     An array type of unknown size is an incomplete type. *)
  | Ttyp_array (_, None) -> true
  (* C23 6.2.5 §27
     A structure or union type of unknown content (...) is an incomplete type. *)
  | Ttyp_struct decl when decl.tstruct_fields = None -> true
  (* An enumerated type can never be incomplete in C, but we support it
     as an extension. *)
  | Ttyp_enum decl when decl.tenum_enumerators = None -> true
  (* Otherwise, it is complete: *)
  | _ -> false

(** Check if [t] is a complete type, by opposition to incomplete. This is the
    same as [not (is_incomplete t)]. *)
let is_complete t = not (is_incomplete t)

(** Returns the integer conversion rank as specified in C23 6.3.1.1. Fails if
    the type is not an integer. *)
let rec integer_conversion_rank t =
  match head t with
  | Ttyp_bool -> 0
  | Ttyp_char | Ttyp_signed_char | Ttyp_unsigned_char -> 1
  | Ttyp_signed_short | Ttyp_unsigned_short -> 2
  | Ttyp_signed_int | Ttyp_unsigned_int -> 3
  | Ttyp_signed_long | Ttyp_unsigned_long -> 4
  | Ttyp_signed_long_long | Ttyp_unsigned_long_long -> 5
  | Ttyp_enum enum_decl ->
      integer_conversion_rank enum_decl.tenum_underlying_type
  | _ -> failwith "Not an integer type"

(** Performs integer promotion as specified in C23 6.3.1.1. *)
let rec integer_promotion t =
  match head t with
  | Ttyp_bool | Ttyp_char | Ttyp_signed_char | Ttyp_unsigned_char
  | Ttyp_signed_short | Ttyp_unsigned_short ->
      Ttyp_signed_int
  | Ttyp_enum enum_decl -> integer_promotion enum_decl.tenum_underlying_type
  | _ -> t

(** Performs default argument promotion as specified in C23 6.5.2. *)
let default_argument_promotion t =
  match head t with Ttyp_float -> Ttyp_double | t -> integer_promotion t

(** If the type is unsigned, convert it to signed. For all other types
    (including non-integer types), return the type unchanged. *)
let to_signed t =
  match head t with
  | Ttyp_unsigned_char -> Ttyp_signed_char
  | Ttyp_unsigned_short -> Ttyp_signed_short
  | Ttyp_unsigned_int -> Ttyp_signed_int
  | Ttyp_unsigned_long -> Ttyp_signed_long
  | Ttyp_unsigned_long_long -> Ttyp_signed_long_long
  | t -> t

(** If the type is signed, convert it to unsigned. For all other types
    (including non-integer types), return the type unchanged. *)
let to_unsigned t =
  match head t with
  | Ttyp_signed_char -> Ttyp_unsigned_char
  | Ttyp_signed_short -> Ttyp_unsigned_short
  | Ttyp_signed_int -> Ttyp_unsigned_int
  | Ttyp_signed_long -> Ttyp_unsigned_long
  | Ttyp_signed_long_long -> Ttyp_unsigned_long_long
  | t -> t

(** Check if a type is an integer type. *)
let is_integer t =
  (* C23 6.2.5 Types
   22> The type char, the signed and unsigned integer types, and the enumerated
     > types are collectively called integer types. *)
  match head t with
  | Ttyp_bool | Ttyp_char | Ttyp_signed_char | Ttyp_unsigned_char
  | Ttyp_signed_short | Ttyp_unsigned_short | Ttyp_signed_int
  | Ttyp_unsigned_int | Ttyp_signed_long | Ttyp_unsigned_long
  | Ttyp_signed_long_long | Ttyp_unsigned_long_long | Ttyp_enum _ ->
      true
  | _ -> false

(** Check if [t] is a signed integer type. *)
let is_signed t = is_integer t && head t = to_signed t

(** Check if [t] is an unsigned integer type. *)
let is_unsigned t = is_integer t && head t = to_unsigned t

(** Check if [t] is a floating-point type. *)
let is_floating_point t =
  match head t with
  | Ttyp_float | Ttyp_double | Ttyp_long_double -> true
  | _ -> false

(** Check if [t] is a pointer type. *)
let is_pointer t = match head t with Ttyp_ptr _ -> true | _ -> false

(** Check if [t] is a pointer to a complete type. *)
let is_pointer_to_complete_type t =
  match head t with Ttyp_ptr ty -> not (is_incomplete ty) | _ -> false

(** Check if [t] is an arithmetic type. *)
let is_arithmetic t =
  (* C23 6.2.5 Types
   23> Integer and floating types are collectively called arithmetic types. *)
  is_integer t || is_floating_point t

(** Check if [t] is a real type. *)
let is_real t =
  (* C23 6.2.5 Types
   22> The integer and real floating types are collectively called real types. *)
  (* We don't support complex types, so we can ignore them. *)
  is_arithmetic t

(** Check if [t] is a scalar type. *)
let is_scalar t =
  (* C23 6.2.5 §26
     Arithmetic types, pointer types, and the nullptr_t type are collectively
     called scalar types. *)
  is_arithmetic t || is_pointer t

(** Check if [t] is an aggregate type. *)
let is_aggregate t =
  (* C23 6.2.5 §26
     Array and structure types are collectively called aggregate types. *)
  match head t with
  | Ttyp_struct struct_decl -> not struct_decl.tstruct_is_union
  | Ttyp_array _ -> true
  | _ -> false

(** Check if [t] is an array type. *)
let is_array t = match head t with Ttyp_array _ -> true | _ -> false

(** Check if [t] is a function type. *)
let is_function t = match head t with Ttyp_function _ -> true | _ -> false

(** Check if [t] is an object type. *)
let is_object t =
  (* C23 6.2.5 §1
     Types are partitioned into object types (types that describe objects) and
     function types (types that describe functions). *)
  not (is_function t)

(** Check if [t] is the void type. *)
let is_void t = head t = Ttyp_void

(** Check if [t] is a void*. *)
let is_pointer_to_void t =
  match head t with Ttyp_ptr ty -> is_void ty | _ -> false

(** Check if [t] is a pointer to an object type. *)
let is_pointer_to_object t =
  match head t with Ttyp_ptr ty -> is_object ty | _ -> false

(** Check if [t] is a pointer to a function type. *)
let is_pointer_to_function t =
  match head t with Ttyp_ptr ty -> is_function ty | _ -> false

(** Check if [t] is the bool type. *)
let is_bool t = head t = Ttyp_bool

(** Check if [t] is a function type. *)
let is_function t = match head t with Ttyp_function _ -> true | _ -> false

(** Get the type pointed to by a pointer type. *)
let pointee t =
  match head t with Ttyp_ptr ty -> ty | _ -> failwith "Not a pointer type"

(** Get the element type of an array type. *)
let element_type t =
  match head t with
  | Ttyp_array (ty, _) -> ty
  | _ -> failwith "Not an array type"

(** If [t] is a enumeration type, convert it to its underlying type. Otherwise,
    returns the type unchanged. *)
let convert_enumeration_type t =
  match head t with Ttyp_enum e -> e.tenum_underlying_type | _ -> t

let remove_qualifiers t = t

(** Get the return type, parameter types, and variadic status of a function
    type. *)
let destruct_function t =
  match head t with
  | Ttyp_function (ret_ty, param_tys, is_variadic) ->
      (ret_ty, param_tys, is_variadic)
  | _ -> failwith "Not a function type"

(** Get the declaration of a struct type. *)
let destruct_struct t =
  match head t with
  | Ttyp_struct struct_decl -> struct_decl
  | _ -> failwith "Not a struct type"

let rec pp_print fmt = function
  | Ttyp_void -> Format.fprintf fmt "void"
  | Ttyp_bool -> Format.fprintf fmt "bool"
  | Ttyp_char -> Format.fprintf fmt "char"
  | Ttyp_signed_char -> Format.fprintf fmt "signed char"
  | Ttyp_unsigned_char -> Format.fprintf fmt "unsigned char"
  | Ttyp_signed_short -> Format.fprintf fmt "short"
  | Ttyp_unsigned_short -> Format.fprintf fmt "unsigned short"
  | Ttyp_signed_int -> Format.fprintf fmt "int"
  | Ttyp_unsigned_int -> Format.fprintf fmt "unsigned int"
  | Ttyp_signed_long -> Format.fprintf fmt "long"
  | Ttyp_unsigned_long -> Format.fprintf fmt "unsigned long"
  | Ttyp_signed_long_long -> Format.fprintf fmt "long long"
  | Ttyp_unsigned_long_long -> Format.fprintf fmt "unsigned long long"
  | Ttyp_float -> Format.fprintf fmt "float"
  | Ttyp_double -> Format.fprintf fmt "double"
  | Ttyp_long_double -> Format.fprintf fmt "long double"
  | Ttyp_ptr ty -> Format.fprintf fmt "%a*" pp_print ty
  | Ttyp_array (ty, Some size) -> Format.fprintf fmt "%a[%d]" pp_print ty size
  | Ttyp_array (ty, None) -> Format.fprintf fmt "%a[]" pp_print ty
  | Ttyp_enum enum_decl ->
      Format.fprintf fmt "enum %s"
        (match enum_decl.tenum_name with
        | Some name -> name.id
        | None -> "<anonymous>")
  | Ttyp_struct struct_decl when struct_decl.tstruct_is_union -> (
      match struct_decl.tstruct_name with
      | Some name -> Format.fprintf fmt "union %s" name.id
      | None -> Format.fprintf fmt "union <anonymous>")
  | Ttyp_struct struct_decl -> (
      match struct_decl.tstruct_name with
      | Some name -> Format.fprintf fmt "struct %s" name.id
      | None -> Format.fprintf fmt "struct <anonymous>")
  | Ttyp_function (ret_ty, param_tys, false) ->
      Format.fprintf fmt "%a(*)(%a)" pp_print ret_ty
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           pp_print)
        param_tys
  | Ttyp_function (ret_ty, param_tys, true) ->
      Format.fprintf fmt "%a(*)(%a, ...)" pp_print ret_ty
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           pp_print)
        param_tys
  | Ttyp_typedef typedef_decl ->
      Format.fprintf fmt "%s (aka %a)" typedef_decl.ttypedef_name.id pp_print
        (head typedef_decl.ttypedef_type)
