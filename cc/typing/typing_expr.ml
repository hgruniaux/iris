open Ast
open Tast
open Typing_common
open Typing_cast

(** Check if an expression is an lvalue. *)
let is_lvalue expr =
  (* C23 6.3.2.1 §1
     An lvalue is an expression (with an object type other than void)
     that potentially designates an object; *)
  let expr_type = Type.head expr.texpr_type in
  expr.texpr_is_lvalue
  && (not (Type.is_void expr_type))
  && Type.is_object expr_type

(** Check if an expression is a modifiable lvalue. *)
let is_modifiable_lvalue expr =
  (* C23 6.3.2.1 §1
     A modifiable lvalue is an lvalue that does not have array type,
     does not have an incomplete type, does not have a const-qualified type,
     and if it is a structure or union, does not have any member (including,
     recursively, any member or element of all contained aggregates or unions)
     with a const-qualified type. *)
  let expr_type = Type.head expr.texpr_type in
  (* FIXME: check const qualifier, and members of struct/union *)
  is_lvalue expr
  && (not (Type.is_array expr_type))
  && Type.is_complete expr_type

(** Report an error of [expr] is not an lvalue. *)
let expect_lvalue expr =
  if not (is_lvalue expr) then
    let msg = "Expected an lvalue expression." in
    error expr.texpr_loc msg

(** Report an error of [expr] is not a modifiable lvalue. *)
let expect_modifiable_lvalue expr =
  if not (is_modifiable_lvalue expr) then
    let msg = "Expected a modifiable lvalue expression." in
    error expr.texpr_loc msg

(** Converts a lvalue to a rvalue as specified in C23 6.3.2.1. *)
let lvalue_conversion (expr : texpr) =
  match expr.texpr_type with
  | Ttyp_array (element_type, _) ->
      let ptr_type = Ttyp_ptr element_type in
      mk_texpr
        (Texpr_implicit_cast (ptr_type, expr, Tcast_array2pointer))
        ptr_type expr.texpr_loc
  | Ttyp_function _ ->
      let ptr_type = Ttyp_ptr expr.texpr_type in
      mk_texpr
        (Texpr_implicit_cast (ptr_type, expr, Tcast_function2pointer))
        ptr_type expr.texpr_loc
  | _ ->
      if is_lvalue expr then
        mk_texpr
          (Texpr_implicit_cast (expr.texpr_type, expr, Tcast_lvalue2rvalue))
          expr.texpr_type expr.texpr_loc
      else expr

(** Determine the type of an integer constant from its suffix and value. *)
let type_from_int_suffix_and_value value suffix =
  match suffix with
  | Pint_suffix_unsigned -> Ttyp_unsigned_int
  | Pint_suffix_long -> Ttyp_signed_long
  | Pint_suffix_unsigned_long -> Ttyp_unsigned_long
  | Pint_suffix_long_long -> Ttyp_signed_long_long
  | Pint_suffix_unsigned_long_long -> Ttyp_unsigned_long_long
  | Pint_suffix_none ->
      (* FIXME: determine the type from the value *)
      ignore value;
      Ttyp_signed_int

(** Determine the type of a floating-point constant from its suffix. *)
let type_from_float_suffix suffix =
  match suffix with
  | Pfloat_suffix_float -> Ttyp_float
  | Pfloat_suffix_long_double -> Ttyp_long_double
  | Pfloat_suffix_none -> Ttyp_double

(** Check the arguments in a function call, and cast them if necessary to their
    respective parameter types. Perform also default argument promotion if
    needed. *)
let check_call_arguments param_types is_vararg args callee_loc =
  let num_args = List.length args in
  let num_params = List.length param_types in

  (* Check if the number of arguments is valid. *)
  if num_args < num_params then
    let msg =
      Format.asprintf
        "Too few arguments in function call; expected at least %d but got %d."
        num_params num_args
    in
    error callee_loc msg
  else if (not is_vararg) && num_args > num_params then
    (* If the function is variadic, we allow extra arguments. *)
    let msg =
      Format.asprintf
        "Too many arguments in function call; expected at most %d but got %d."
        num_params num_args
    in
    error callee_loc msg
  else
    (* Cast arguments to their respective parameter types or perform
       default argument promotion if needed. *)
    List.mapi
      (fun i targ ->
        let targ = lvalue_conversion targ in
        if i < num_params then
          try_implicit_cast_to (List.nth param_types i) targ
        else perform_default_argument_promotion targ)
      args

(** Check the return type of a function call and report any error at [loc]. *)
let check_call_return_type return_type loc =
  (* C23 6.5.3.3 §1
     The expression that denotes the called function shall have type pointer
     to function returning void or returning a complete object type other than
     an array type. *)
  if Type.is_void return_type then ()
  else if Type.is_incomplete return_type then
    let msg =
      Format.asprintf "Call to function with incomplete return type %a."
        Type.pp_print return_type
    in
    error loc msg
  else if Type.is_array return_type then
    let msg =
      Format.asprintf "Call to function with array return type %a."
        Type.pp_print return_type
    in
    error loc msg

(** Check the callee expression in a function call, and return the callee's
    return type, parameter types, and whether it is variadic. *)
let check_callee callee_expr =
  let report_invalid_callee () =
    let msg =
      Format.asprintf "Callee is not a function or function pointer, got %a."
        Type.pp_print callee_expr.texpr_type
    in
    error callee_expr.texpr_loc msg
  in

  (* C23 6.5.3.3 §1
     The expression that denotes the called function shall have type pointer
     to function returning void or returning a complete object type other than
     an array type. *)
  let callee_type = Type.head callee_expr.texpr_type in
  if not (Type.is_pointer callee_type) then report_invalid_callee ();
  let pointee = Type.pointee callee_type in
  match Type.head pointee with
  | Ttyp_function (return_type, param_types, is_vararg) ->
      check_call_return_type return_type callee_expr.texpr_loc;
      (return_type, param_types, is_vararg)
  | _ -> report_invalid_callee ()

let check_sizeof_operand loc typ =
  (* C23 6.5.4.4 §1
     The sizeof operator shall not be applied to an expression that has
     function type or an incomplete type, to the parenthesized name of
     such a type, or to an expression that designates a bit-field member. *)
  (* TODO: support bit-fields *)
  if Type.is_incomplete typ || Type.is_function typ then
    let msg =
      Format.asprintf "Cannot apply sizeof to incomplete or function type %a."
        Type.pp_print typ
    in
    error loc msg

let check_alignof_operand loc typ =
  (* C23 6.5.4.4 §1
     The alignof operator shall not be applied to a function type
     or an incomplete type. *)
  if Type.is_incomplete typ || Type.is_function typ then
    let msg =
      Format.asprintf "Cannot apply alignof to incomplete or function type %a."
        Type.pp_print typ
    in
    error loc msg
