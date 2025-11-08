open Ast
open Tast
open Constant_eval
open Typing_cast
open Typing_expr
open Typing_decl
include Typing_common

let ptrdiff_t = Ttyp_signed_long_long
let size_t = Ttyp_unsigned_long_long

let expect_arithmetic_type expr =
  if not (Type.is_arithmetic expr.texpr_type) then
    let msg =
      Format.asprintf "Expected arithmetic type, but got %a." Type.pp_print
        expr.texpr_type
    in
    error expr.texpr_loc msg

let expect_integer_type expr =
  if not (Type.is_integer expr.texpr_type) then
    let msg =
      Format.asprintf "Expected integer type, but got %a." Type.pp_print
        expr.texpr_type
    in
    error expr.texpr_loc msg

let expect_scalar_type expr =
  if not (Type.is_scalar expr.texpr_type) then
    let msg =
      Format.asprintf "Expected scalar type, but got %a." Type.pp_print
        expr.texpr_type
    in
    error expr.texpr_loc msg

let expect_constant_integer_expr expr =
  expect_integer_type expr;
  match eval_constant_expr expr with
  | VInt n -> n
  | _ ->
      let msg = Format.asprintf "This expression is not constant." in
      error expr.texpr_loc msg

type type_info = {
  mutable current_type : ttype option;
  mutable is_const : bool;
  mutable is_volatile : bool;
  mutable is_restrict : bool;
  mutable alignas : ttype option;
  mutable is_inline : bool;
  mutable is_noreturn : bool;
  mutable storage_class : pstorage_class_specifier option;
  mutable is_signed : bool option;
  mutable is_long : bool;
  mutable is_long_long : bool;
  mutable is_short : bool;
}

(** Parse a list of declaration specifiers and produce a type with its
    qualifiers. *)
let rec parse_type ctx specifiers : ttype =
  let info =
    {
      current_type = None;
      is_const = false;
      is_volatile = false;
      is_restrict = false;
      alignas = None;
      is_inline = false;
      is_noreturn = false;
      storage_class = None;
      is_signed = None;
      is_long = false;
      is_long_long = false;
      is_short = false;
    }
  in

  let handle_type_specifier loc = function
    | Ptype_specifier_void -> info.current_type <- Some Ttyp_void
    | Ptype_specifier_bool -> info.current_type <- Some Ttyp_bool
    | Ptype_specifier_char -> info.current_type <- Some Ttyp_char
    | Ptype_specifier_int -> info.current_type <- Some Ttyp_signed_int
    | Ptype_specifier_double -> info.current_type <- Some Ttyp_double
    | Ptype_specifier_float -> info.current_type <- Some Ttyp_float
    | Ptype_specifier_short -> info.is_short <- true
    | Ptype_specifier_long ->
        if info.is_long_long then () (* TODO: error when more than two longs *)
        else if info.is_long then info.is_long_long <- true
        else info.is_long <- true
    (* TODO: Check for signed/unsigned *)
    | Ptype_specifier_signed -> info.is_signed <- Some true
    | Ptype_specifier_unsigned -> info.is_signed <- Some false
    | Ptype_specifier_typedef_name id ->
        let typedef_decl = lookup_typedef ctx id in
        info.current_type <- Some (Ttyp_typedef typedef_decl)
    | Ptype_specifier_struct struct_specifier ->
        let typ =
          parse_struct_specifier ~is_union:false ctx loc struct_specifier
        in
        info.current_type <- Some typ
    | Ptype_specifier_union struct_specifier ->
        let typ =
          parse_struct_specifier ~is_union:true ctx loc struct_specifier
        in
        info.current_type <- Some typ
    | Ptype_specifier_enum enum_specifier ->
        let typ = parse_enum_specifier ctx loc enum_specifier in
        info.current_type <- Some typ
    | Ptype_specifier_typeof_expr (expr, is_typeof_unqual) ->
        let typ = parse_typeof_expr ctx expr is_typeof_unqual in
        info.current_type <- Some typ
    | Ptype_specifier_typeof_type (type_name, is_typeof_unqual) ->
        let typ = parse_typeof_type ctx type_name is_typeof_unqual in
        info.current_type <- Some typ
  in

  let handle_type_qualifier _loc = function
    | Ptype_qualifier_const -> info.is_const <- true
    | Ptype_qualifier_volatile -> info.is_volatile <- true
    | Ptype_qualifier_restrict -> info.is_restrict <- true
    | Ptype_qualifier_alignas_expr expr ->
        (* TODO: Support alignas specifier *)
        let t = type_expr ctx expr in
        let c = eval_constant_expr t in
        ignore c
    | Ptype_qualifier_alignas_type type_name ->
        (* TODO: Support alignas specifier *)
        let parsed_type = parse_type_name ctx type_name in
        ignore parsed_type
  in

  let handle_function_specifier _loc = function
    | Pfunction_specifier_inline -> info.is_inline <- true
    | Pfunction_specifier_noreturn -> info.is_noreturn <- true
  in

  let handle_storage_class_specifier _loc c =
    (* TODO *)
    ignore c
  in

  List.iter
    (fun specifier ->
      let loc = specifier.loc in
      match specifier.value with
      | Ptype_specifier t -> handle_type_specifier loc t
      | Ptype_qualifier q -> handle_type_qualifier loc q
      | Pfunction_specifier s -> handle_function_specifier loc s
      | Pstorage_class_specifier s -> handle_storage_class_specifier loc s)
    specifiers;

  (* Handle signedness qualifiers *)
  (match info.is_signed with
  | None -> ()
  | Some is_signed -> (
      match info.current_type with
      | None ->
          if is_signed then info.current_type <- Some Ttyp_signed_int
          else info.current_type <- Some Ttyp_unsigned_int
      | Some t when Type.is_integer t ->
          info.current_type <-
            Some (if is_signed then Type.to_signed t else Type.to_unsigned t)
      | _ -> ()));

  (if info.is_short then
     match info.current_type with
     | None -> info.current_type <- Some Ttyp_signed_short
     | Some Ttyp_signed_int -> info.current_type <- Some Ttyp_signed_short
     | Some Ttyp_unsigned_int -> info.current_type <- Some Ttyp_unsigned_short
     | Some _ -> failwith "parse_type: 'short' cannot combine with this type");

  (if info.is_long_long then
     match info.current_type with
     | None -> info.current_type <- Some Ttyp_signed_long_long
     | Some Ttyp_signed_int -> info.current_type <- Some Ttyp_signed_long_long
     | Some Ttyp_unsigned_int ->
         info.current_type <- Some Ttyp_unsigned_long_long
     | Some _ ->
         failwith "parse_type: 'long long' cannot combine with this type");

  (if info.is_long && not info.is_long_long then
     match info.current_type with
     | None -> info.current_type <- Some Ttyp_signed_long
     | Some Ttyp_signed_int -> info.current_type <- Some Ttyp_signed_long
     | Some Ttyp_unsigned_int -> info.current_type <- Some Ttyp_unsigned_long
     | Some Ttyp_double -> info.current_type <- Some Ttyp_long_double
     | Some _ -> failwith "parse_type: 'long' cannot combine with this type");

  match info.current_type with
  | Some t -> t
  | None -> failwith "parse_type: No type specified"

and parse_abstract_declarator_with_type ctx abstract_declarator base_type =
  match abstract_declarator with
  | Pabstract_declarator_pointer (d, quals) -> (
      ignore quals;
      match d with
      | None -> Ttyp_ptr base_type
      | Some d -> parse_abstract_declarator_with_type ctx d (Ttyp_ptr base_type)
      )
  | Pabstract_declarator_paren d ->
      parse_abstract_declarator_with_type ctx d base_type
  | Pabstract_declarator_array (d, quals, size_expr) ->
      ignore quals;
      let size =
        Option.map (type_and_eval_constant_expr_as_int ctx) size_expr
      in
      let array_type = Ttyp_array (base_type, size) in
      parse_abstract_declarator_with_type ctx d array_type
  | Pabstract_declarator_array_static _ ->
      failwith
        "parse_abstract_declarator_with_type: array static not implemented"
  | Pabstract_declarator_array_star _ ->
      failwith "parse_abstract_declarator_with_type: array * not implemented"
  | Pabstract_declarator_function (d, params, is_vararg) -> (
      let params = parse_function_declarator_params ctx params in
      let param_tys = List.map (fun v -> v.tvalue_decl_type) params in
      let function_type = Ttyp_function (base_type, param_tys, is_vararg) in
      match d with
      | None -> function_type
      | Some d -> parse_abstract_declarator_with_type ctx d function_type)

and parse_type_name ctx (specifiers, abstract_declarator) : ttype =
  let base_type = parse_type ctx specifiers in
  match abstract_declarator with
  | None -> base_type
  | Some abstract_declarator ->
      parse_abstract_declarator_with_type ctx abstract_declarator base_type

and parse_struct_declaration ~is_union ctx loc name =
  let name =
    match name with
    | Some name -> name
    | None ->
        let msg =
          Format.asprintf "Declaration of anonymous %s must be a definition."
            (if is_union then "union" else "struct")
        in
        error loc msg
  in

  let symbol_table =
    if is_union then ctx.ctx_union_declarations else ctx.ctx_struct_declarations
  in
  match Hashtbl.find_opt symbol_table name.id with
  | Some struct_decl ->
      (* The struct was previously declared, we need to check if both declarations
          are compatible. *)
      (* FIXME: Check if compatible declarations (same fields etc.), see standard. *)
      Ttyp_struct struct_decl
  | None ->
      let struct_decl =
        {
          tstruct_name = Some name;
          tstruct_fields = None;
          tstruct_is_union = is_union;
        }
      in
      Hashtbl.add ctx.ctx_struct_declarations name.id struct_decl;
      Ttyp_struct struct_decl

and parse_struct_definition ~is_union ctx name members =
  let symbol_table =
    if is_union then ctx.ctx_union_declarations else ctx.ctx_struct_declarations
  in

  let tstruct_decl =
    (* Check if the struct was previously declared, in that case complete the definition.
       Otherwise, declare a new struct. *)
    match
      Option.map (fun name -> Hashtbl.find_opt symbol_table name.id) name
    with
    | Some (Some struct_decl) ->
        (* The struct was previously declared, we need to check if both declarations
         are compatible. *)
        (* FIXME: Check if compatible declarations (same fields etc.), see standard. *)
        struct_decl
    | _ ->
        let struct_decl =
          {
            tstruct_name = name;
            tstruct_fields = None;
            tstruct_is_union = is_union;
          }
        in

        (* Add the declaration to the symbol table. *)
        (match name with
        | None -> ()
        | Some name -> Hashtbl.replace symbol_table name.id struct_decl);
        struct_decl
  in

  (* Type check members. *)
  let members_symbol_table = Hashtbl.create 7 in
  let struct_fields = ref [] in

  let type_check_member tmember_type declarator =
    let name, tmember_type, _ =
      parse_declarator_with_type ctx declarator tmember_type
    in
    if Hashtbl.mem members_symbol_table name.id then
      let msg = Format.asprintf "Redefinition of member '%s'." name.id in
      error name.loc msg
    else
      let tfield_decl =
        { tstruct_field_name = name; tstruct_field_type = tmember_type }
      in
      struct_fields := tfield_decl :: !struct_fields;
      Hashtbl.add members_symbol_table name.id tfield_decl
  in

  List.iter
    (fun member ->
      match member.value with
      | Pstruct_member_static_assert _ ->
          failwith "Static assert not implemented"
      | Pstruct_member_field (typ, declarators) ->
          let tfield_type = parse_type ctx typ in
          List.iter
            (fun declarator -> type_check_member tfield_type declarator)
            declarators)
    members;

  tstruct_decl.tstruct_fields <- Some (List.rev !struct_fields);
  Ttyp_struct tstruct_decl

and parse_struct_specifier ~is_union ctx loc (name, members_opt) =
  match members_opt with
  | None -> parse_struct_declaration ~is_union ctx loc name
  | Some members -> parse_struct_definition ~is_union ctx name members

and parse_enum_declaration ctx loc name tunderlying_type_opt =
  let name =
    match name with
    | Some name -> name
    | None ->
        let msg = "Declaration of anonymous union must be a definition." in
        error loc msg
  in

  match Hashtbl.find_opt ctx.ctx_enum_declarations name.id with
  | Some enum_decl ->
      (* The enum was previously declared, we need to check if both declarations
         are compatible. *)
      (* FIXME: Check if compatible declarations (same underlying type etc.), see standard. *)
      Ttyp_enum enum_decl
  | None ->
      let enum_decl =
        {
          tenum_name = Some name;
          tenum_underlying_type =
            Option.value tunderlying_type_opt ~default:Ttyp_signed_int;
          tenum_enumerators = None;
        }
      in
      Hashtbl.add ctx.ctx_enum_declarations name.id enum_decl;
      Ttyp_enum enum_decl

and parse_enum_definition ctx loc name tunderlying_type_opt enumerators =
  let enum_decl =
    (* Check if the enum was previously declared, in that case complete the definition.
       Otherwise, declare a new enum. *)
    match
      Option.map
        (fun name -> Hashtbl.find_opt ctx.ctx_enum_declarations name.id)
        name
    with
    | Some (Some enum_decl) ->
        (* The enum was previously declared, we need to check if both declarations
           are compatible. *)
        (* FIXME: Check if compatible declarations (same underlying type etc.), see standard. *)
        enum_decl
    | _ ->
        let enum_decl =
          {
            tenum_name = name;
            tenum_underlying_type =
              Option.value tunderlying_type_opt ~default:Ttyp_signed_int;
            tenum_enumerators = None;
          }
        in

        (* Add the declaration to the symbol table. *)
        (match name with
        | Some name -> Hashtbl.add ctx.ctx_enum_declarations name.id enum_decl
        | None -> ());

        enum_decl
  in

  (* TODO: parse enumerators *)
  ignore enumerators;
  ignore loc;

  Ttyp_enum enum_decl

and parse_enum_specifier ctx loc (name, underlying_type_opt, enumerators_opt) =
  let tunderlying_type_opt = Option.map (parse_type ctx) underlying_type_opt in
  match enumerators_opt with
  | None -> parse_enum_declaration ctx loc name tunderlying_type_opt
  | Some enumerators ->
      parse_enum_definition ctx loc name tunderlying_type_opt enumerators

and parse_typeof_expr ctx expr is_typeof_unqual =
  let texpr = type_expr ctx expr in
  if is_typeof_unqual then Type.remove_qualifiers texpr.texpr_type
  else texpr.texpr_type

and parse_typeof_type ctx type_name is_typeof_unqual =
  let ttype = parse_type_name ctx type_name in
  if is_typeof_unqual then Type.remove_qualifiers ttype else ttype

and adjust_parameter_type ttype =
  match ttype with
  | Ttyp_array (element_type, _) ->
      (* C23 6.7.7.4 §6
         A declaration of a parameter as "array of type" shall be
         adjusted to "qualified pointer to type". *)
      (* TODO: handle qualifiers *)
      Ttyp_ptr element_type
  | Ttyp_function _ ->
      (* C23 6.7.7.4 §7
         A declaration of a parameter as "function returning type" shall be
         adjusted to "pointer to function returning type", as in 6.3.2.1. *)
      Ttyp_ptr ttype
  | _ -> ttype

and parse_function_parameter ctx loc (t, d) =
  let ttyp = parse_type ctx t in
  let name, ttyp, _ = parse_declarator_with_type ctx d ttyp in
  let adjusted_type = adjust_parameter_type ttyp in
  let param_decl = create_param_decl adjusted_type (Some name) loc in
  param_decl

and parse_function_parameter_unnamed ctx loc type_name =
  let ttyp = parse_type_name ctx type_name in
  let adjusted_type = adjust_parameter_type ttyp in
  let param_decl = create_param_decl adjusted_type None loc in
  param_decl

and parse_function_declarator_params ctx params : tvalue_decl list =
  let parsed_params =
    List.map
      (fun param ->
        match param.value with
        | Pparameter_named (t, d) ->
            parse_function_parameter ctx param.loc (t, d)
        | Pparameter_unnamed type_name ->
            parse_function_parameter_unnamed ctx param.loc type_name)
      params
  in

  (* C23 6.7.7.4 §9
     The special case of an unnamed parameter of type void as the only item in
     the list specifies that the function has no parameters. *)
  match parsed_params with
  | param_decl :: []
    when Type.is_void param_decl.tvalue_decl_type
         && param_decl.tvalue_decl_name = None ->
      []
  | _ -> parsed_params

and parse_declarator_with_type ctx declarator base_type =
  let rec aux ctx declarator base_type params =
    match declarator.value with
    | Pdeclarator_name id -> (id, base_type, params)
    | Pdeclarator_paren d -> aux ctx d base_type params
    | Pdeclarator_pointer (d, quals) ->
        ignore quals;
        (* TODO: quals *)
        aux ctx d (Ttyp_ptr base_type) params
    | Pdeclarator_array (d, quals, size_expr) ->
        ignore quals;
        (* TODO: quals *)
        let size =
          Option.map (type_and_eval_constant_expr_as_int ctx) size_expr
        in
        aux ctx d (Ttyp_array (base_type, size)) params
    | Pdeclarator_array_static _ ->
        failwith "parse_declarator_with_type: array static not implemented"
    | Pdeclarator_array_star _ ->
        failwith "parse_declarator_with_type: array * not implemented"
    | Pdeclarator_function (d, params, is_vararg, _) ->
        let params = parse_function_declarator_params ctx params in
        let param_tys = List.map (fun v -> v.tvalue_decl_type) params in
        let function_type = Ttyp_function (base_type, param_tys, is_vararg) in
        aux ctx d function_type params
  in
  aux ctx declarator base_type []

(* C23 6.5 Expressions *)
and type_expr ctx (expr : pexpr) =
  match expr.pexpr_kind with
  (* C23 6.5.2 Primary expressions *)
  | Pexpr_nullptr -> type_nullptr_expr ctx expr.pexpr_loc
  | Pexpr_bool b -> type_bool_expr ctx expr.pexpr_loc b
  | Pexpr_int (n, s) -> type_int_expr ctx expr.pexpr_loc n s
  | Pexpr_float (f, s) -> type_float_expr ctx expr.pexpr_loc f s
  | Pexpr_char c -> type_char_expr ctx expr.pexpr_loc c
  | Pexpr_string s -> type_string_expr ctx expr.pexpr_loc s
  | Pexpr_ident id -> type_ident_expr ctx expr.pexpr_loc id
  | Pexpr_paren e -> type_paren_expr ctx expr.pexpr_loc e
  (* C23 6.5.3 Postfix expressions *)
  | Pexpr_array (array_expr, index_expr) ->
      type_array_expr ctx expr.pexpr_loc array_expr index_expr
  | Pexpr_call (callee, args) -> type_call_expr ctx expr.pexpr_loc callee args
  | Pexpr_member (base, member_name) ->
      type_member_expr ctx expr.pexpr_loc base member_name
  | Pexpr_member_deref (base, member_name) ->
      type_member_deref_expr ctx expr.pexpr_loc base member_name
  (* C23 6.5.4 Unary operators *)
  | Pexpr_unary (op, e) -> type_unary_expr ctx expr.pexpr_loc op e
  | Pexpr_sizeof ty -> type_sizeof_expr ctx expr.pexpr_loc ty
  | Pexpr_alignof ty -> type_alignof_expr ctx expr.pexpr_loc ty
  (* C23 6.5.6 - 6.5.15 and 6.5.17 - 6.5.18 *)
  | Pexpr_binary (op, lhs, rhs) ->
      type_binary_expr ctx expr.pexpr_loc op lhs rhs
  (* C23 6.5.5 Cast operators *)
  | Pexpr_cast (ty, e) ->
      type_cast_expr ctx expr.pexpr_loc ty e
      (* C23 6.5.16 Conditional operator *)
  | Pexpr_conditional (cond, then_expr, else_expr) ->
      type_conditional_expr ctx expr.pexpr_loc cond then_expr else_expr

(* C23 6.5.2 Primary expressions *)
and type_nullptr_expr _ loc =
  (* C23 6.4.4.6 §4
     The keyword nullptr represents a null pointer constant. *)
  ignore loc;
  failwith "type_nullptr_expr: Not implemented"

(* C23 6.5.2 Primary expressions *)
and type_bool_expr _ loc b =
  (* C23 6.4.4.6 §3
     The keywords false and true are constants of type bool with
     a value of 0 for false and 1 for true. *)
  mk_texpr (Texpr_bool b) Ttyp_bool loc

(* C23 6.5.2 Primary expressions
   C23 6.4.4.2 Integer constants *)
and type_int_expr _ loc value suffix =
  let typ = type_from_int_suffix_and_value value suffix in
  mk_texpr (Texpr_int value) typ loc

(* C23 6.5.2 Primary expressions
   C23 6.4.4.3 Floating constants *)
and type_float_expr _ loc value suffix =
  let typ = type_from_float_suffix suffix in
  mk_texpr (Texpr_float value) typ loc

(* C23 6.5.2 Primary expressions
   C23 6.4.4.5 Character constants *)
and type_char_expr _ loc c =
  ignore loc;
  ignore c;
  failwith "type_char_expr: Not implemented"

(* C23 6.5.2 Primary expressions
   C23 6.4.5 String literals *)
and type_string_expr _ loc s =
  let full_string = String.concat "" s in
  (* C23 6.5.2 §5
     A string literal is a primary expression.
     It is an lvalue with type as detailed in 6.4.5.

     C23 6.4.5 §6
     The multibyte character sequence (...) array of static storage duration
     and length just sufficient to contain the sequence. For character string
     literals, the array elements have type char (...). *)
  mk_texpr ~is_lvalue:true (Texpr_string full_string)
    (Ttyp_array (Ttyp_char, Some (String.length full_string + 1)))
    loc

(* C23 6.5.2 Primary expressions *)
and type_ident_expr ctx loc id =
  let decl = lookup_value ctx id in
  mk_texpr ~is_lvalue:true (Texpr_decl decl) decl.tvalue_decl_type loc

(* C23 6.5.2 Primary expressions *)
and type_paren_expr ctx loc expr =
  (* C23 6.5.2 §6
     A parenthesized expression is a primary expression. Its type, value,
     and semantics are identical to those of the unparenthesized expression. *)
  let typed_expr = type_expr ctx expr in
  let expr_type = typed_expr.texpr_type in
  let is_lvalue = typed_expr.texpr_is_lvalue in
  mk_texpr ~is_lvalue (Texpr_paren typed_expr) expr_type loc

(* C23 6.5.3.2 Array subscripting *)
and type_array_expr ctx loc array_expr index_expr =
  (* [tarray] is the expression before the subscript operator.
     [tindex] is the expression inside the brackets.
     So `tarray[tindex]`. *)
  let tarray = lvalue_conversion (type_expr ctx array_expr) in
  let tindex = lvalue_conversion (type_expr ctx index_expr) in

  (* [tbase_expr] is the base address of the array element, it has pointer type.
     [toffset_expr] is the index of the element being accessed, it has integer type.
     They are not the same as [tarray] and [tindex] because `5[ptr]` is valid in C
     and equivalent to `ptr[5]`. *)
  let tbase_expr, toffset_expr =
    if Type.is_pointer tarray.texpr_type then (
      expect_integer_type tindex;
      (tarray, tindex))
    else if Type.is_pointer tindex.texpr_type then (
      expect_integer_type tarray;
      (tindex, tarray))
    else
      let msg =
        Format.asprintf
          "One of the operands of array subscript must be a pointer."
      in
      error loc msg
  in

  (if not (Type.is_pointer_to_complete_type tbase_expr.texpr_type) then
     let msg =
       Format.asprintf "Cannot subscript pointer to incomplete type %a."
         Type.pp_print tbase_expr.texpr_type
     in
     error tbase_expr.texpr_loc msg);

  let element_type = Type.pointee tbase_expr.texpr_type in
  mk_texpr ~is_lvalue:true
    (Texpr_array (tbase_expr, toffset_expr))
    element_type loc

(* C23 6.5.3.3 Function calls *)
and type_call_expr ctx loc callee args =
  let tcallee = lvalue_conversion (type_expr ctx callee) in
  let targs = List.map (type_expr ctx) args in
  let return_type, param_types, is_vararg = check_callee tcallee in
  let casted_args =
    check_call_arguments param_types is_vararg targs tcallee.texpr_loc
  in
  mk_texpr (Texpr_call (tcallee, casted_args)) return_type loc

and check_member_name struct_decl member_name =
  match struct_decl.tstruct_fields with
  | None ->
      let msg =
        Format.asprintf "Cannot access member '%s' of incomplete struct/union."
          member_name.id
      in
      error member_name.loc msg
  | Some fields -> (
      match
        List.find_opt (fun f -> f.tstruct_field_name.id = member_name.id) fields
      with
      | Some field -> field
      | None ->
          let msg =
            Format.asprintf "Struct/union has no member named '%s'."
              member_name.id
          in
          error member_name.loc msg)

and check_base_type_for_member_access base_loc base_type =
  match Type.head base_type with
  | Ttyp_struct struct_decl -> struct_decl
  | _ ->
      (* C23 6.5.3.4 Structure and union members
       > The first operand of the . operator shall have type "structure" or "union".
       > If the first operand is not of structure or union type, the program is ill-formed. *)
      let msg =
        Format.asprintf "Cannot access member of non-struct/union type %a."
          Type.pp_print base_type
      in
      error base_loc msg

(* C23 6.5.3.4 Structure and union members *)
and type_member_expr ctx loc base member_name =
  let tbase = type_expr ctx base in
  let struct_decl =
    check_base_type_for_member_access tbase.texpr_loc tbase.texpr_type
  in
  let field = check_member_name struct_decl member_name in
  (* 3> The value is that of the named member, and is an lvalue if
      > the first expression is an lvalue.  *)
  let is_lvalue = tbase.texpr_is_lvalue in
  mk_texpr ~is_lvalue (Texpr_member (tbase, field)) field.tstruct_field_type loc

(* C23 6.5.3.4 Structure and union members *)
and type_member_deref_expr ctx loc base member_name =
  let tbase = type_expr ctx base in

  let pointee_type =
    match tbase.texpr_type with
    | Ttyp_ptr pointed_type -> pointed_type
    | _ ->
        (* 2> The first operand of the -> operator shall have type "pointer to atomic,
            > qualified, or unqualified structure" or "pointer to atomic, qualified,
            > or unqualified union". *)
        let msg =
          Format.asprintf "Cannot dereference non-pointer type %a."
            Type.pp_print tbase.texpr_type
        in
        error tbase.texpr_loc msg
  in

  let struct_decl =
    check_base_type_for_member_access tbase.texpr_loc pointee_type
  in
  let field = check_member_name struct_decl member_name in

  (* 4> The value is that of the named member of the object to which
      > the first expression points, and is an lvalue. *)
  let is_lvalue = true in
  mk_texpr ~is_lvalue
    (Texpr_member_deref (tbase, field))
    field.tstruct_field_type loc

(* C23 6.5.3 Postfix operators *)
(* C23 6.5.4 Unary operators *)
and type_unary_expr ctx loc op e =
  let te = type_expr ctx e in
  match op.punop_kind with
  | Punop_plus | Punop_neg ->
      (* C23 6.5.4.3 Unary arithmetic operators *)
      let te = lvalue_conversion te in
      expect_arithmetic_type te;
      let te_casted = perform_integer_promotion te in
      mk_texpr (Texpr_unary (op, te_casted)) te_casted.texpr_type loc
  | Punop_bitnot ->
      (* C23 6.5.4.3 Unary arithmetic operators *)
      let te = lvalue_conversion te in
      expect_integer_type te;
      let te_casted = perform_integer_promotion te in
      mk_texpr (Texpr_unary (op, te_casted)) te_casted.texpr_type loc
  | Punop_lognot ->
      (* C23 6.5.4.3 Unary arithmetic operators *)
      let te = lvalue_conversion te in
      expect_scalar_type te;
      mk_texpr (Texpr_unary (op, te)) Ttyp_signed_int loc
  | Punop_pre_inc | Punop_pre_dec | Punop_post_inc | Punop_post_dec ->
      expect_arithmetic_type te;
      let result_type = te.texpr_type in
      mk_texpr (Texpr_unary (op, te)) result_type loc
  | Punop_addrof ->
      (* C23 6.5.4.2 Address and indirection operators *)
      expect_lvalue te;
      let ptr_type = Ttyp_ptr te.texpr_type in
      mk_texpr (Texpr_unary (op, te)) ptr_type loc
  | Punop_deref -> (
      (* C23 6.5.4.2 Address and indirection operators *)
      match te.texpr_type with
      | Ttyp_ptr pointed_type ->
          mk_texpr ~is_lvalue:true (Texpr_unary (op, te)) pointed_type loc
      | _ ->
          let msg =
            Format.asprintf "Cannot dereference non-pointer type %a."
              Type.pp_print te.texpr_type
          in
          error te.texpr_loc msg)
  | Punop_sizeof ->
      (* C23 6.5.4.4 The sizeof and alignof operators *)
      check_sizeof_operand te.texpr_loc te.texpr_type;
      (* C23 6.5.4.4 §5
       The value of the result of both operators is implementation-defined,
       and its type (an unsigned integer type) is size_t *)
      let result_type = size_t in
      mk_texpr (Texpr_unary (op, te)) result_type loc

(* C23 6.5.4.4 The sizeof and alignof operators *)
and type_sizeof_expr ctx loc ty =
  let ttype = parse_type_name ctx ty in
  check_sizeof_operand loc ttype;

  (* C23 6.5.4.4 §5
     The value of the result of both operators is implementation-defined,
     and its type (an unsigned integer type) is size_t *)
  let result_type = size_t in
  mk_texpr (Texpr_sizeof ttype) result_type loc

(* C23 6.5.4.4 The sizeof and alignof operators *)
and type_alignof_expr ctx loc ty =
  let ttype = parse_type_name ctx ty in
  check_alignof_operand loc ttype;

  (* C23 6.5.4.4 §5
     The value of the result of both operators is implementation-defined,
     and its type (an unsigned integer type) is size_t *)
  let result_type = size_t in
  mk_texpr (Texpr_alignof ttype) result_type loc

(* C23 6.5.5 Cast operators *)
and type_cast_expr ctx loc target_type expr =
  let parsed_target_type = parse_type_name ctx target_type in
  let typed_expr = lvalue_conversion (type_expr ctx expr) in
  let cast_kind = can_explicitly_cast_expr typed_expr parsed_target_type in
  match cast_kind with
  | Tcast_invalid ->
      let msg =
        Format.asprintf "Cannot cast expression of type %a to type %a."
          Type.pp_print typed_expr.texpr_type Type.pp_print parsed_target_type
      in
      error loc msg
  | _ ->
      mk_texpr
        (Texpr_cast (parsed_target_type, typed_expr, cast_kind))
        parsed_target_type loc

(* C23 6.5.6 Multiplicative operators
   Except the % operator. *)
and type_binary_multiplicative_expr _ loc op tlhs trhs =
  let tlhs = lvalue_conversion tlhs in
  let trhs = lvalue_conversion trhs in
  (* 2§ Each of the operands shall have arithmetic type. *)
  expect_arithmetic_type tlhs;
  expect_arithmetic_type trhs;
  (* 4§ The usual arithmetic conversions are performed on the operands. *)
  let tlhs_casted, trhs_casted =
    perform_usual_arithmetic_conversion tlhs trhs
  in
  mk_texpr
    (Texpr_binary (op, tlhs_casted, trhs_casted))
    tlhs_casted.texpr_type loc

(* C23 6.5.7 Additive operators *)
and type_binary_add_expr _ loc op tlhs trhs =
  let tlhs = lvalue_conversion tlhs in
  let trhs = lvalue_conversion trhs in
  let tlhs_typ = tlhs.texpr_type in
  let trhs_typ = trhs.texpr_type in

  let check_pointer_type ptr_expr =
    if not (Type.is_pointer_to_complete_type ptr_expr.texpr_type) then
      let msg =
        Format.asprintf
          "Pointer arithmetic on pointer to incomplete type %a is not allowed."
          Type.pp_print ptr_expr.texpr_type
      in
      error ptr_expr.texpr_loc msg
    else ptr_expr.texpr_type
  in

  if Type.is_arithmetic tlhs_typ && Type.is_arithmetic trhs_typ then
    (* 2§ either both operands shall have arithmetic type, *)
    let tlhs_casted, trhs_casted =
      perform_usual_arithmetic_conversion tlhs trhs
    in
    mk_texpr
      (Texpr_binary (op, tlhs_casted, trhs_casted))
      tlhs_casted.texpr_type loc
  else if Type.is_pointer tlhs_typ && Type.is_integer trhs_typ then
    (* 2> or one operand shall be a pointer to a complete object type and
        > the other shall have integer type. *)
    let result_type = check_pointer_type tlhs in
    mk_texpr (Texpr_binary (op, tlhs, trhs)) result_type loc
  else if Type.is_integer tlhs_typ && Type.is_pointer trhs_typ then
    let result_type = check_pointer_type trhs in
    mk_texpr (Texpr_binary (op, tlhs, trhs)) result_type loc
  else
    let msg =
      Format.asprintf
        "Invalid operands to binary operator '+'; left operand has type %a, \
         right operand has type %a."
        Type.pp_print tlhs_typ Type.pp_print trhs_typ
    in
    error loc msg

(* C23 6.5.7 Additive operators *)
and type_binary_sub_expr _ loc op tlhs trhs =
  let tlhs = lvalue_conversion tlhs in
  let trhs = lvalue_conversion trhs in
  let tlhs_typ = tlhs.texpr_type in
  let trhs_typ = trhs.texpr_type in

  let check_pointer_type ptr_expr =
    if not (Type.is_pointer_to_complete_type ptr_expr.texpr_type) then
      let msg =
        Format.asprintf
          "Pointer arithmetic on pointer to incomplete type %a is not allowed."
          Type.pp_print ptr_expr.texpr_type
      in
      error ptr_expr.texpr_loc msg
  in

  if Type.is_arithmetic tlhs_typ && Type.is_arithmetic trhs_typ then
    (* 3> both operands have arithmetic type *)
    let tlhs_casted, trhs_casted =
      perform_usual_arithmetic_conversion tlhs trhs
    in
    mk_texpr
      (Texpr_binary (op, tlhs_casted, trhs_casted))
      tlhs_casted.texpr_type loc
  else if Type.is_pointer tlhs_typ && Type.is_pointer trhs_typ then (
    (* 3> both operands are pointers to qualified or unqualified versions of
        > compatible complete object types *)
    check_pointer_type tlhs;
    check_pointer_type trhs;
    (* FIXME: check if they are compatible types *)
    (* 10> The size of the result is implementation-defined, and its type
         > (a signed integer type) is ptrdiff_t *)
    mk_texpr (Texpr_binary (op, tlhs, trhs)) ptrdiff_t loc)
  else if Type.is_pointer tlhs_typ && Type.is_integer trhs_typ then (
    (* 3> the left operand is a pointer to a complete object type and the right
        > operand has integer type. *)
    check_pointer_type tlhs;
    mk_texpr (Texpr_binary (op, tlhs, trhs)) tlhs.texpr_type loc)
  else
    let msg =
      Format.asprintf
        "Invalid operands to binary operator '-'; left operand has type %a, \
         right operand has type %a."
        Type.pp_print tlhs_typ Type.pp_print trhs_typ
    in
    error loc msg

(* C23 6.5.8 Bitwise shift operators *)
and type_binary_shift_expr _ loc op tlhs trhs =
  let tlhs = lvalue_conversion tlhs in
  let trhs = lvalue_conversion trhs in
  (* 2> Each of the operands shall have integer type. *)
  expect_integer_type tlhs;
  expect_integer_type trhs;
  (* 3> The integer promotions are performed on each of the operands.
      > The type of the result is that of the promoted left operand. *)
  let tlhs_casted = perform_integer_promotion tlhs in
  let trhs_casted = perform_integer_promotion trhs in
  let result_type = tlhs_casted.texpr_type in
  mk_texpr (Texpr_binary (op, tlhs_casted, trhs_casted)) result_type loc

(* C23 6.5.6 Multiplicative operators (the % operator)
   C23 6.5.11 Bitwise AND operator
   C23 6.5.12 Bitwise exclusive OR operator
   C23 6.5.13 Bitwise inclusive OR operator *)
and type_binary_integer_expr _ loc op tlhs trhs =
  let tlhs = lvalue_conversion tlhs in
  let trhs = lvalue_conversion trhs in
  expect_integer_type tlhs;
  expect_integer_type trhs;
  let tlhs_casted, trhs_casted =
    perform_usual_arithmetic_conversion tlhs trhs
  in
  mk_texpr
    (Texpr_binary (op, tlhs_casted, trhs_casted))
    tlhs_casted.texpr_type loc

(* C23 6.5.9 Relational operators *)
and type_binary_relational_expr _ loc op tlhs trhs =
  let tlhs = lvalue_conversion tlhs in
  let trhs = lvalue_conversion trhs in
  let tlhs_typ = tlhs.texpr_type in
  let trhs_typ = trhs.texpr_type in

  (* 7> The result has type int. *)
  let result_type = Ttyp_signed_int in
  if Type.is_real tlhs_typ && Type.is_real trhs_typ then
    (* 2> both operands have real type *)
    (* 4> If both of the operands have arithmetic type, the usual
        > arithmetic conversions are performed. *)
    let tlhs_casted, trhs_casted =
      perform_usual_arithmetic_conversion tlhs trhs
    in
    mk_texpr (Texpr_binary (op, tlhs_casted, trhs_casted)) result_type loc
  else if Type.is_pointer tlhs_typ && Type.is_pointer trhs_typ then
    (* 2> both operands are pointers to qualified or unqualified versions
        > of compatible object types *)
    (* FIXME: check if compatible types *)
    mk_texpr (Texpr_binary (op, tlhs, trhs)) result_type loc
  else
    let msg =
      Format.asprintf
        "Invalid operands to binary relational operator; left operand has type \
         %a, right operand has type %a."
        Type.pp_print tlhs_typ Type.pp_print trhs_typ
    in
    error loc msg

(* C23 6.5.10 Equality operators *)
and type_binary_equality_expr _ loc op tlhs trhs =
  let tlhs = lvalue_conversion tlhs in
  let trhs = lvalue_conversion trhs in
  let tlhs_typ = tlhs.texpr_type in
  let trhs_typ = trhs.texpr_type in
  (* 4> The result has type int. *)
  let result_type = Ttyp_signed_int in
  if Type.is_arithmetic tlhs_typ && Type.is_arithmetic trhs_typ then
    (* 2> both operands have arithmetic type *)
    (* 5> If both of the operands have arithmetic type, the usual arithmetic
        > conversions are performed. *)
    let tlhs_casted, trhs_casted =
      perform_usual_arithmetic_conversion tlhs trhs
    in
    mk_texpr (Texpr_binary (op, tlhs_casted, trhs_casted)) result_type loc
  else
    (* FIXME: Support pointers and other types *)
    let msg =
      Format.asprintf
        "Invalid operands to binary equality operator; left operand has type \
         %a, right operand has type %a."
        Type.pp_print tlhs_typ Type.pp_print trhs_typ
    in
    error loc msg

(* C23 6.5.14 Logical AND operator
   C23 6.5.15 Logical OR operator *)
and type_binary_logical_expr _ loc op tlhs trhs =
  ignore loc;
  ignore op;
  ignore tlhs;
  ignore trhs;
  failwith "type_binary_logical_expr: Not implemented"

(* C23 6.5.17 Assignment operators *)
and type_binary_assignment_expr ctx loc op lhs rhs =
  let tlhs = type_expr ctx lhs in
  let trhs = type_expr ctx rhs in

  (if not tlhs.texpr_is_lvalue then
     let msg = "Left operand of assignment operator must be an lvalue." in
     error tlhs.texpr_loc msg);

  match op.pbinop_kind with
  | Pbinop_assign ->
      expect_modifiable_lvalue tlhs;
      let trhs_converted =
        try_implicit_cast_to tlhs.texpr_type (lvalue_conversion trhs)
      in
      mk_texpr (Texpr_binary (op, tlhs, trhs_converted)) tlhs.texpr_type loc
  | Pbinop_assign_add | Pbinop_assign_sub | Pbinop_assign_mul
  | Pbinop_assign_div | Pbinop_assign_mod | Pbinop_assign_and | Pbinop_assign_or
  | Pbinop_assign_xor | Pbinop_assign_shl | Pbinop_assign_shr ->
      failwith "type_assignment_expr: Not implemented for compound assignments"
  | _ -> assert false

(* C23 6.5.18 Comma operator *)
and type_binary_comma_expr _ loc op tlhs trhs =
  (* > A comma operator does not yield an lvalue. *)
  let trhs = lvalue_conversion trhs in
  mk_texpr (Texpr_binary (op, tlhs, trhs)) trhs.texpr_type loc

and type_binary_expr ctx loc op lhs rhs =
  let tlhs = type_expr ctx lhs in
  let trhs = type_expr ctx rhs in
  match op.pbinop_kind with
  | Pbinop_mul | Pbinop_div ->
      type_binary_multiplicative_expr ctx loc op tlhs trhs
  | Pbinop_add -> type_binary_add_expr ctx loc op tlhs trhs
  | Pbinop_sub -> type_binary_sub_expr ctx loc op tlhs trhs
  | Pbinop_mod | Pbinop_and | Pbinop_or | Pbinop_xor ->
      type_binary_integer_expr ctx loc op tlhs trhs
  | Pbinop_shl | Pbinop_shr -> type_binary_shift_expr ctx loc op tlhs trhs
  | Pbinop_lt | Pbinop_gt | Pbinop_le | Pbinop_ge ->
      type_binary_relational_expr ctx loc op tlhs trhs
  | Pbinop_eq | Pbinop_ne -> type_binary_equality_expr ctx loc op tlhs trhs
  | Pbinop_logand | Pbinop_logor ->
      type_binary_logical_expr ctx loc op tlhs trhs
  | Pbinop_assign | Pbinop_assign_add | Pbinop_assign_sub | Pbinop_assign_mul
  | Pbinop_assign_div | Pbinop_assign_mod | Pbinop_assign_and | Pbinop_assign_or
  | Pbinop_assign_xor | Pbinop_assign_shl | Pbinop_assign_shr ->
      type_binary_assignment_expr ctx loc op lhs rhs
  | Pbinop_comma -> type_binary_comma_expr ctx loc op tlhs trhs

(* C23 6.5.16 Conditional operator *)
and type_conditional_expr ctx loc cond then_expr else_expr =
  let tcond = lvalue_conversion (type_expr ctx cond) in
  let tthen_expr = lvalue_conversion (type_expr ctx then_expr) in
  let telse_expr = lvalue_conversion (type_expr ctx else_expr) in
  ignore tcond;
  ignore tthen_expr;
  ignore telse_expr;
  ignore loc;
  failwith "type_conditional_expr: Not implemented"

and type_and_eval_constant_expr_as_int ctx expr =
  let texpr = lvalue_conversion (type_expr ctx expr) in
  let value = expect_constant_integer_expr texpr in
  try Z.to_int value
  with Z.Overflow ->
    let msg = "Constant integer expression is too big." in
    error texpr.texpr_loc msg

(** Check if [expr] has scalar type and convert it to bool. *)
let type_boolean_expr ctx expr =
  let texpr = lvalue_conversion (type_expr ctx expr) in
  expect_scalar_type texpr;
  try_implicit_cast_to Ttyp_bool texpr

(* C23 6.8 Statements and blocks *)
let rec type_stmt ctx (stmt : pstmt) =
  match stmt.pstmt_kind with
  | Pstmt_decl decl -> type_decl_stmt ctx decl
  (* C23 6.8.2 Labeled statements *)
  | Pstmt_label (label, stmt) -> type_label_stmt ctx stmt.pstmt_loc label stmt
  | Pstmt_case (expr, stmt) -> type_case_stmt ctx stmt.pstmt_loc expr stmt
  | Pstmt_default stmt -> type_default_stmt ctx stmt.pstmt_loc stmt
  (* C23 6.8.3 Compound statement *)
  | Pstmt_compound stmts -> type_compound_stmt ctx stmt.pstmt_loc stmts
  (* C23 6.8.4 Expression and null statements *)
  | Pstmt_null -> mk_tstmt Tstmt_null stmt.pstmt_loc
  | Pstmt_expr e ->
      let te = type_expr ctx e in
      mk_tstmt (Tstmt_expr te) stmt.pstmt_loc
  (* C23 6.8.5 Selection statements *)
  | Pstmt_if (cond, true_stmt, false_stmt) ->
      type_if_stmt ctx stmt.pstmt_loc cond true_stmt false_stmt
  | Pstmt_switch (cond, body) -> type_switch_stmt ctx stmt.pstmt_loc cond body
  (* C23 6.8.6 Iteration statements *)
  | Pstmt_while (cond, body) -> type_while_stmt ctx stmt.pstmt_loc cond body
  | Pstmt_do (body, cond) -> type_do_stmt ctx stmt.pstmt_loc body cond
  | Pstmt_for (init_opt, cond_opt, incr_opt, body) ->
      type_for_stmt ctx stmt.pstmt_loc init_opt cond_opt incr_opt body
  (* 6.8.7 Jump statements *)
  | Pstmt_goto label -> type_goto_stmt ctx stmt.pstmt_loc label
  | Pstmt_continue -> type_continue_stmt ctx stmt.pstmt_loc
  | Pstmt_break -> type_break_stmt ctx stmt.pstmt_loc
  | Pstmt_return v -> type_return_stmt ctx stmt.pstmt_loc v

and type_decl_stmt ctx decl =
  let tdecl = type_decl ctx decl in
  match tdecl with
  | None -> mk_tstmt Tstmt_null decl.pdecl_loc
  | Some tdecl -> mk_tstmt (Tstmt_decl tdecl) tdecl.tdecl_loc

(* C23 6.8.2 Labeled statements *)
and type_label_stmt ctx loc label stmt =
  let tlabel = { tlabel_name = label; tlabel_loc = label.loc } in
  Hashtbl.add ctx.ctx_labels label.id tlabel;
  let tstmt = type_stmt ctx stmt in
  mk_tstmt (Tstmt_label (tlabel, tstmt)) loc

(* C23 6.8.2 Labeled statements *)
(* C23 6.8.5.3 The switch statement *)
and type_case_stmt ctx loc expr stmt =
  let switch_labels =
    match ctx.ctx_switchs with
    | [] ->
        (* C23 6.8.2 Labeled statements
         > A case or default label shall appear only in a switch statement. *)
        let msg = "This case label is not within a switch statement." in
        error loc msg
    | sl :: _ -> sl
  in

  let texpr = lvalue_conversion (type_expr ctx expr) in
  let case_value = expect_constant_integer_expr texpr in

  (* C23 6.8.5.3 The switch statement
   > No two case labels in the same switch statement shall have the same
   > constant expression value. *)
  if List.exists (fun (v, _) -> v = case_value) switch_labels.tswitch_cases then
    let msg = "Duplicate case label in one switch statement." in
    error loc msg
  else
    switch_labels.tswitch_cases <-
      (case_value, mk_tstmt (Tstmt_case (texpr, mk_tstmt Tstmt_null loc)) loc)
      :: switch_labels.tswitch_cases;

  let tstmt = type_stmt ctx stmt in
  mk_tstmt (Tstmt_case (texpr, tstmt)) loc

(* C23 6.8.2 Labeled statements *)
(* C23 6.8.5.3 The switch statement *)
and type_default_stmt ctx loc stmt =
  let switch_labels =
    match ctx.ctx_switchs with
    | [] ->
        (* C23 6.8.2 Labeled statements
         > A case or default label shall appear only in a switch statement. *)
        let msg = "This default label is not within a switch statement." in
        error loc msg
    | sl :: _ -> sl
  in

  let tstmt = type_stmt ctx stmt in
  let tdefault_stmt = mk_tstmt (Tstmt_default tstmt) loc in

  (* C23 6.8.5.3 The switch statement
   > There may be at most one default label associated to a switch statement. *)
  if switch_labels.tswitch_default <> None then
    let msg = "Multiple default labels in one switch statement." in
    error loc msg
  else switch_labels.tswitch_default <- Some tdefault_stmt;

  tdefault_stmt

(* C23 6.8.3 The compound statement *)
and type_compound_stmt ctx loc stmts =
  push_scope ctx;
  let tstmts = List.map (type_stmt ctx) stmts in
  pop_scope ctx;
  mk_tstmt (Tstmt_compound tstmts) loc

(* C23 6.8.5.2 The if statement *)
and type_if_stmt ctx loc cond true_stmt false_stmt =
  let tcond = type_boolean_expr ctx cond in
  let ttrue = type_stmt ctx true_stmt in
  let tfalse = Option.map (type_stmt ctx) false_stmt in
  mk_tstmt (Tstmt_if (tcond, ttrue, tfalse)) loc

(* C23 6.8.5.3 The switch statement *)
and type_switch_stmt ctx loc cond body =
  let tcond = lvalue_conversion (type_expr ctx cond) in
  expect_integer_type tcond;
  let tswitch_labels = { tswitch_default = None; tswitch_cases = [] } in
  ctx.ctx_switchs <- tswitch_labels :: ctx.ctx_switchs;
  let tbody = type_stmt ctx body in
  ctx.ctx_switchs <- List.tl ctx.ctx_switchs;
  mk_tstmt (Tstmt_switch (tcond, tswitch_labels, tbody)) loc

(* C23 6.8.6.2 The while statement *)
and type_while_stmt ctx loc cond body =
  let tcond = type_boolean_expr ctx cond in
  let tbody = type_stmt ctx body in
  mk_tstmt (Tstmt_while (tcond, tbody)) loc

(* C23 6.8.6.3 The do statement *)
and type_do_stmt ctx loc body cond =
  let tbody = type_stmt ctx body in
  let tcond = type_boolean_expr ctx cond in
  mk_tstmt (Tstmt_do (tbody, tcond)) loc

(* C23 6.8.6.4 The for statement *)
and type_for_stmt ctx loc init_opt cond_opt incr_opt body =
  let stmt_from_expr e =
    let te = type_expr ctx e in
    mk_tstmt (Tstmt_expr te) te.texpr_loc
  in

  push_scope ctx;
  let tinit = Option.map (type_stmt ctx) init_opt in
  let tcond = Option.map (fun c -> type_boolean_expr ctx c) cond_opt in
  let tincr = Option.map stmt_from_expr incr_opt in
  let tbody = type_stmt ctx body in
  pop_scope ctx;
  mk_tstmt (Tstmt_for (tinit, tcond, tincr, tbody)) loc

(* C23 6.8.7.2 The goto statement *)
and type_goto_stmt ctx loc label =
  match Hashtbl.find_opt ctx.ctx_labels label.id with
  | Some tlabel -> mk_tstmt (Tstmt_goto tlabel) loc
  | None ->
      let msg = Format.asprintf "Unknown label '%s'." label.id in
      error loc msg

(* C23 6.8.7.3 The continue statement *)
and type_continue_stmt ctx loc =
  (* C23 6.8.7.3 §1
     A continue statement shall appear only in or as a loop body. *)
  (if ctx.ctx_loop_depth < 0 then
     let msg = "The continue statement is not within a loop." in
     error loc msg);

  mk_tstmt Tstmt_continue loc

(* C23 6.8.7.4 The break statement *)
and type_break_stmt ctx loc =
  (* C23 6.8.7.4 §1
     A break statement shall appear only in or as a switch body or loop body. *)
  (if ctx.ctx_loop_depth < 0 && ctx.ctx_switchs = [] then
     let msg =
       "The break statement is not within a loop or switch statement."
     in
     error loc msg);

  mk_tstmt Tstmt_break loc

(* C23 6.8.7.5 The return statement *)
and type_return_stmt ctx loc v_opt =
  let return_type = expected_return_type ctx in
  match v_opt with
  | Some v ->
      let tv = lvalue_conversion (type_expr ctx v) in
      let tv_casted = try_implicit_cast_to return_type tv in
      mk_tstmt (Tstmt_return (Some tv_casted)) loc
  | None ->
      if not (Type.is_void return_type) then
        let msg =
          "Return statement missing expression for non-void function."
        in
        error loc msg
      else mk_tstmt (Tstmt_return None) loc

and type_variable_declarator ctx ttyp declarator init_opt =
  let name, var_type, params = parse_declarator_with_type ctx declarator ttyp in
  let typed_init_opt = Option.map (type_expr ctx) init_opt in
  let value_decl =
    handle_value_declaration ctx declarator.loc var_type name params
      typed_init_opt
  in
  mk_tdecl (Tdecl_value value_decl) value_decl.tvalue_decl_loc

and type_variable_declaration ctx loc typ declarators_and_initializers =
  let ttyp = parse_type ctx typ in
  let decls =
    List.map
      (fun (declarator, initializer_expr_opt) ->
        type_variable_declarator ctx ttyp declarator initializer_expr_opt)
      declarators_and_initializers
  in
  Some (mk_tdecl (Tdecl_decls decls) loc)

and type_typedef_declarator ctx ttyp declarator initializer_expr_opt =
  match initializer_expr_opt with
  | Some _ ->
      let msg = "A typedef declarator cannot have an initializer." in
      error declarator.loc msg
  | None ->
      ();

      let name, typedef_type, _ =
        parse_declarator_with_type ctx declarator ttyp
      in

      let typedef_decl =
        { ttypedef_name = name; ttypedef_type = typedef_type }
      in
      let decl = mk_tdecl (Tdecl_typedef typedef_decl) declarator.loc in
      Symbol_table.add_typedef ctx.ctx_symbol_table name.id typedef_decl;
      decl

and type_typedef_declaration ctx loc typ declarators_and_initializers =
  let ttyp = parse_type ctx typ in
  let decls =
    List.map
      (fun (declarator, initializer_expr_opt) ->
        type_typedef_declarator ctx ttyp declarator initializer_expr_opt)
      declarators_and_initializers
  in
  Some (mk_tdecl (Tdecl_decls decls) loc)

and type_function_body ctx return_type params body =
  ctx.ctx_current_return_type <- Some return_type;

  push_scope ctx;

  (* Register the parameters into the function's symbol table. *)
  List.iter
    (fun param ->
      match param.tvalue_decl_name with
      | None -> ()
      | Some name -> Symbol_table.add_value ctx.ctx_symbol_table name.id param)
    params;

  let tbody = type_stmt ctx body in
  pop_scope ctx;
  tbody

and type_function_definition ctx typ declarator body =
  let return_type = parse_type ctx typ in
  let name, func_type, params =
    parse_declarator_with_type ctx declarator return_type
  in
  assert (Type.is_function func_type);

  let value_decl, func_decl =
    handle_function_definition ctx declarator.loc func_type name params
  in

  let tbody = type_function_body ctx return_type params body in
  func_decl.tfun_body <- Some tbody;

  let decl = mk_tdecl (Tdecl_value value_decl) value_decl.tvalue_decl_loc in
  Some decl

and type_decl ctx (decl : pdecl) =
  match decl.pdecl_kind with
  | Pdecl_null -> None
  | Pdecl_variable (typ, declarators) ->
      type_variable_declaration ctx decl.pdecl_loc typ declarators
  | Pdecl_typedef (typ, declarators_and_initializers) ->
      type_typedef_declaration ctx decl.pdecl_loc typ
        declarators_and_initializers
  | Pdecl_static_assert _ -> failwith "type_decl: Not implemented"
  | Pdecl_function_definition (typ, declarator, body) ->
      type_function_definition ctx typ declarator body

and type_translation_unit (tu : ptranslation_unit) =
  let ctx =
    {
      ctx_switchs = [];
      ctx_symbol_table = Symbol_table.create ();
      ctx_struct_declarations = Hashtbl.create 7;
      ctx_union_declarations = Hashtbl.create 7;
      ctx_enum_declarations = Hashtbl.create 7;
      ctx_labels = Hashtbl.create 7;
      ctx_loop_depth = 0;
      ctx_current_return_type = None;
    }
  in

  let builtin_type name typ =
    let loc = (Lexing.dummy_pos, Lexing.dummy_pos) in
    let typedef_decl =
      { ttypedef_name = { id = name; loc }; ttypedef_type = typ }
    in
    Symbol_table.add_typedef ctx.ctx_symbol_table name typedef_decl
  in

  (* Add built-in typedefs *)
  builtin_type "__builtin_va_list" (Ttyp_array (Ttyp_char, None));

  List.filter_map (type_decl ctx) tu
